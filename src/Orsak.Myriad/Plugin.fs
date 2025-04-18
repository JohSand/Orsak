namespace Orsak.Myriad

open Myriad.Core
open System

[<AttributeUsage(AttributeTargets.Interface)>]
type GenRunnerAttribute() =
    inherit Attribute()

[<MyriadGenerator("EffectRunnerGen")>]
type EffectRunnerGen() =
    interface IMyriadGenerator with
        member _.ValidInputExtensions = seq { ".fs" }

        member _.Generate(_context: GeneratorContext) =
            // let ast, _ =
            //     Ast.fromFilename context.InputFilename |> Async.RunSynchronously |> Array.head
            //
            // let namespaceAndRecords = Ast.extractTypeDefn ast |> List.toArray
            //
            // let recordsModules =
            //     namespaceAndRecords
            //     |> Array.collect (fun (ns, records) ->
            //         records
            //         |> List.toArray
            //         |> Array.choose (fun r ->
            //             let attr = Ast.getAttribute<Example1GenAttribute> r
            //             Option.map (fun a -> r, a) attr))
            //
            // let red = "\x1b[91m"
            // let normal = "\x1b[39m"
            // let inherits = recordsModules |> Array.collect Helpers.getInherits
            // Console.WriteLine $"inherits {red}%A{inherits}{normal}"

            Output.Source("module HelloWorldModule = ()")
