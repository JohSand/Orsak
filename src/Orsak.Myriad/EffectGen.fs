namespace Orsak.Myriad

open Myriad.Core
open System.Text

[<MyriadGenerator("EffectGen")>]
type EffectGen() =
    interface IMyriadGenerator with
        member _.ValidInputExtensions = seq { ".fs" }

        member _.Generate(context: GeneratorContext) =
            let _ast, _x =
                Ast.fromFilename context.InputFilename |> Async.RunSynchronously |> Array.head

            let sb = StringBuilder().ToIndentingBuilder()

            match Ast.parseEffects _ast with
            | [ ctx ] ->
                Writer.writeEffectGen ctx sb
                ()
            | _ -> ()


            Output.Source(sb.ToString())
