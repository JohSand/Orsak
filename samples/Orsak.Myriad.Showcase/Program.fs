// For more information see https://aka.ms/fsharp-console-apps

open Orsak
open Orsak.Myriad
open System.Threading
open Microsoft.Extensions.Caching.Memory
open Orsak.Myriad.Showcase

let myRunner  (cache: IFace) (rnd: IRandomGenerator) =
    Runner.mkRunner {
        fromEffect rnd
        fromEffect cache
    }
    |> fun runner ->
        Face.countBeans 1 "" |> Effect.run runner

printfn "Hello from F#"


