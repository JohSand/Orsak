// For more information see https://aka.ms/fsharp-console-apps

open Orsak
open Orsak.Myriad
open System.Threading
open Microsoft.Extensions.Caching.Memory
open ConsoleApp1

let myRunner (src: CancellationTokenSource) (cache: IMemoryCache) (rnd: IRandomGenerator) =
    Runner.mkRunner {
        fromEffect src
        fromEffect rnd
        fromEffect cache
    }


printfn "Hello from F#"


