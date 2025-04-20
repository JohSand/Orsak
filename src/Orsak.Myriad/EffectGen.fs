namespace Orsak.Myriad

open Myriad.Core
open System
open Myriad.Core
open Fantomas.FCS.Syntax
open System.Text

[<AttributeUsage(AttributeTargets.Interface)>]
type GenEffectsAttribute() =
    inherit Attribute()

module Helpers2 =
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
                | (SynType.Fun(_argType, (SynType.Fun _ as t), _range, _)) -> countArguments t (acc + 1)
                | (SynType.Fun(argType, _, _range, _)) -> countArguments argType acc
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

    let toCamelCase (name: string) =
        if name.Length > 0 then
            name.[0].ToString().ToLowerInvariant() + name.Substring(1)
        else
            name

    let parseMembers (SynTypeDefn(typeInfo: SynComponentInfo, objModel, _, _, _, _)) (sb: IndentingStringBuilder) =

        let effectName = parseTypeName typeInfo
        sb.AppendLine($"module {effectName.TrimStart('I')} =") |> Console.WriteLine

        match objModel with
        | SynTypeDefnRepr.ObjectModel(_, members, _) ->
            for m in members do
                match m with
                | SynMemberDefn.AbstractSlot(slotSig = (SynValSig(synType = d))) ->
                    let memberName = parseMemberName m
                    sb.Append($"    let {memberName |> toCamelCase} ")
                    let parameterCount = parseInParamCount m

                    for i = 1 to parameterCount do
                        let varName = char (96 + i) |> string
                        sb.Append($"{varName} ")

                    sb.AppendLine("=")

                    let apply =
                        if isTuple d then
                            let p = String.Join(", ", [ for i in 1..parameterCount -> char (96 + i) |> string ])
                            $"({p})"
                        else
                            let p = String.Join(" ", [ for i in 1..parameterCount -> char (96 + i) |> string ])
                            $" {p}"

                    sb.AppendLine
                        $"        Effect.Create(fun (er: #IProvide<{effectName}>) -> er.Effect.{memberName}{apply})"

                | _ ->
                    Console.WriteLine($"not abstract slot fun ident {m}")
                    ()


        | _ -> ()

    let rec createEffectModule (decls: SynModuleDecl list) (sb: IndentingStringBuilder) =
        match decls with
        | [] -> ()
        | x :: xs ->
            match x with
            | SynModuleDecl.Types(types, _) ->
                for t in types do
                    //Console.WriteLine t
                    if Ast.hasAttribute<GenEffectsAttribute> t then
                        parseMembers t sb

                createEffectModule xs sb
            | _ -> createEffectModule xs sb

    let extractTypeDefn (ast: ParsedInput) (sb: IndentingStringBuilder) =
        match ast with
        | ParsedInput.ImplFile(ParsedImplFileInput(_name, _, _, _, _, modules, _, _, _)) ->
            for SynModuleOrNamespace(namespaceId, _, _, moduleDecls, _, _, _, _, _) in modules do
                let ns = String.Join(".", namespaceId |> List.map (fun i -> i.idText))
                sb.AppendLine($"namespace {ns}")
                sb.AppendLine("open Orsak")
                createEffectModule moduleDecls sb
        | _ -> ()


[<MyriadGenerator("EffectRunnerGen")>]
type EffectGen() =
    interface IMyriadGenerator with
        member _.ValidInputExtensions = seq { ".fs" }

        member _.Generate(context: GeneratorContext) =
            let _ast, _x =
                Ast.fromFilename context.InputFilename |> Async.RunSynchronously |> Array.head

            let sb = StringBuilder().ToIndentingBuilder()
            do Helpers2.extractTypeDefn _ast sb

            Output.Source(sb.ToString())
