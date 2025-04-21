module Orsak.Myriad.Ast

open Myriad.Core
open System
open Orsak.Myriad
open Fantomas.FCS.Syntax
open System.Text

let toString (ident: LongIdent) =
    String.Join(".", ident |> List.map (fun i -> i.idText))

let getName (SynTypeDefn(c, _, _, _, _, _)) =
    match c with
    | SynComponentInfo({ Attributes = { TypeName = SynLongIdent(name, _, _) } :: _ } :: _, _, _, _, _, _, _, _) ->
        toString name
    | _ -> ""

let (|Provider|_|) =
    function
    | SynType.App(SynType.LongIdent(SynLongIdent([ xx: Ident ], _, _)), _, [ SynType.LongIdent a ], _, _, _, _) ->
        if xx.idText = "IProvide" then Some a.LongIdent else None
    | _ -> None

let getInherits (SynTypeDefn(_c, typeRepr: SynTypeDefnRepr, _, _, _, _)) = [|
    match typeRepr with
    | SynTypeDefnRepr.ObjectModel(_kind, members: SynMemberDefn list, _range) ->
        for mem in members do
            match mem with
            | SynMemberDefn.Inherit(Provider name, _, _) ->

                yield { name = name |> List.last |> _.idText; fullName = toString name }
            | _ ->

                ()
    | _ -> ()
|]

let mkEffectAttributeMatches (s: SynTypeDefn) = { effects = getInherits s; typeName = getName s }

let rec createContextScope (decls: SynModuleDecl list) (acc: ContextWriterScope) =
    match decls with
    | [] -> acc
    | x :: xs ->
        match x with
        | SynModuleDecl.Types(types, _) ->
            let attributedTypes =
                types
                |> List.filter Ast.hasAttribute<GenRunnerAttribute>
                |> List.map mkEffectAttributeMatches

            createContextScope xs { acc with effects = attributedTypes @ acc.effects }

        | SynModuleDecl.Open(SynOpenDeclTarget.ModuleOrNamespace(SynLongIdent(target, _, _), _), _) ->
            let ns = toString target

            createContextScope xs { acc with openStatements = ns :: acc.openStatements }

        | SynModuleDecl.NestedModule(SynComponentInfo(_, _, _, _longId, _, _, _, _), _, _decls, _, _, _) ->
            createContextScope xs acc
        | _other -> createContextScope xs acc


let parseRunnerDefn (ast: ParsedInput) = [
    match ast with
    | ParsedInput.ImplFile(ParsedImplFileInput(_name, _, _, _, _, modules, _, _, _)) ->
        for SynModuleOrNamespace(namespaceId, _, _, moduleDecls, _, _, _, _, _) in modules do
            createContextScope moduleDecls { openStatements = []; effects = []; ns = toString namespaceId }
    | _ -> ()
]

let isTuple (t: SynType) =
    match t with
    | SynType.Fun(SynType.Tuple _, _returnType, _range, _) -> true
    | _ -> false

let parseInParamCount (m: SynMemberDefn) =
    match m with
    | SynMemberDefn.AbstractSlot(slotSig = s) ->
        let (SynValSig(synType = d)) = s

        let rec countArguments synType acc =
            match synType with
            //if returnType is a fun, we are of type 'a -> 'b -> ...
            | SynType.Fun(_argType, (SynType.Fun _ as t), _range, _) -> countArguments t (acc + 1)
            | SynType.Fun(argType, _, _range, _) -> countArguments argType acc
            | SynType.LongIdent(SynLongIdent _) -> 1 + acc
            | SynType.Paren(innerType = inner) -> countArguments inner acc
            | SynType.Tuple(_, types, _) -> types.Length / 2 + 1 + acc
            //| SynType.Var _ -> acc
            | _ -> acc

        countArguments d 0

    | _ -> 0

let parseMemberName (m: SynMemberDefn) =
    match m with
    | SynMemberDefn.AbstractSlot(slotSig = (SynValSig(ident = SynIdent(id, _)))) -> id.idText
    | _ -> ""

let parseTypeName (SynComponentInfo(_, _, _, longId, _, _, _, _)) = longId |> List.last |> _.idText

let (|EffectMemberCfg|_|) (SynTypeDefn(typeInfo: SynComponentInfo, objModel, _, _, _, _)) =
    match objModel with
    | SynTypeDefnRepr.ObjectModel(_, members, _) ->
        let members = [
            for m in members do
                match m with
                | SynMemberDefn.AbstractSlot(slotSig = (SynValSig(synType = d))) -> {
                    argumentCount = parseInParamCount m
                    memberName = parseMemberName m
                    isTuple = isTuple d
                  }
                | _ -> ()
        ]

        Some({ effectName = parseTypeName typeInfo; members = members })
    | _ -> None

let rec createEffectModule (decls: SynModuleDecl list) (agg: ContextEffectScope) =
    match decls with
    | [] -> agg
    | x :: xs ->
        match x with
        | SynModuleDecl.Types(types, _) ->
            let providerTypes =
                types
                |> List.filter Ast.hasAttribute<GenEffectsAttribute>
                |> List.choose ((|EffectMemberCfg|_|))

            createEffectModule xs { agg with effects = providerTypes @ agg.effects }
        | _ -> createEffectModule xs agg

let parseEffects (ast: ParsedInput) : ContextEffectScope list = [

    match ast with
    | ParsedInput.ImplFile(ParsedImplFileInput(_name, _, _, _, _, modules, _, _, _)) ->
        for SynModuleOrNamespace(namespaceId, _, _, moduleDecls, _, _, _, _, _) in modules do
            { ContextEffectScope.effects = []; openStatements = []; ns = toString namespaceId }
            |> createEffectModule moduleDecls
    | _ -> ()
]
