namespace Orsak.Myriad

open Myriad.Core
open System
open Myriad.Core
open Myriad.Core.Ast
open Fantomas.FCS.Syntax
open System.Text

[<AttributeUsage(AttributeTargets.Interface)>]
type GenRunnerAttribute() =
    inherit Attribute()

module Helpers =
    let toString (ident: LongIdent) =
        String.Join(".", ident |> List.map (fun i -> i.idText))

    let getName (SynTypeDefn(c, _, _, _, _, _)) =
        match c with
        | SynComponentInfo({ Attributes = { TypeName = SynLongIdent(name, _, _) } :: _ } :: _, _, _, _, _, _, _, _) ->
            toString name
        | _ -> ""

    let (|Provider|_|) =
        function
        | (SynType.App(SynType.LongIdent(SynLongIdent([ xx: Ident ], _, _)), _, [ SynType.LongIdent a ], _, _, _, _)) ->
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
                    |> List.filter hasAttribute<GenRunnerAttribute>
                    |> List.map mkEffectAttributeMatches

                createContextScope xs { acc with effects = attributedTypes @ acc.effects }

            | SynModuleDecl.Open(SynOpenDeclTarget.ModuleOrNamespace(SynLongIdent(target, _, _), _), _) ->
                let ns = toString target

                createContextScope xs { acc with openStatements = ns :: acc.openStatements }

            | SynModuleDecl.NestedModule(SynComponentInfo(_, _, _, _longId, _, _, _, _), _, _decls, _, _, _) ->
                createContextScope xs acc
            | _other -> createContextScope xs acc


    let extractTypeDefn (ast: ParsedInput) = [
        match ast with
        | ParsedInput.ImplFile(ParsedImplFileInput(_name, _, _, _, _, modules, _, _, _)) ->
            for SynModuleOrNamespace(namespaceId, _, _, moduleDecls, _, _, _, _, _) in modules do
                createContextScope moduleDecls { openStatements = []; effects = []; ns = toString namespaceId }
        | _ -> ()
    ]



[<MyriadGenerator("EffectRunnerGen")>]
type EffectRunnerGen() =
    interface IMyriadGenerator with
        member _.ValidInputExtensions = seq { ".fs" }

        member _.Generate(context: GeneratorContext) =
            let ast, _x =
                Ast.fromFilename context.InputFilename |> Async.RunSynchronously |> Array.head

            let sb = StringBuilder().ToIndentingBuilder()
            match Helpers.extractTypeDefn ast with
            | [ a ] ->
                Writer.writeAll a sb
                let red = "\x1b[91m"
                let normal = "\x1b[39m"
                Console.WriteLine $"Namespace {red}%A{a}{normal}"
                ()
            | _ -> ()
            Output.Source(sb.ToString())
