namespace Orsak.Myriad

open Myriad.Core
open Orsak.Myriad
open System.Text

[<MyriadGenerator("EffectRunnerGen")>]
type EffectRunnerGen() =
    interface IMyriadGenerator with
        member _.ValidInputExtensions = seq { ".fs" }

        member _.Generate(context: GeneratorContext) =
            let ast, _x =
                Ast.fromFilename context.InputFilename |> Async.RunSynchronously |> Array.head

            let sb = StringBuilder().ToIndentingBuilder()
            match Ast.parseRunnerDefn ast with
            | [ a ] ->
                if a.effects.Length > 0 then
                    Writer.writeForScope a sb

            | _ -> ()
            Output.Source(sb.ToString())
