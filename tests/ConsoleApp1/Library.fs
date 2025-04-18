module ConsoleApp1.Library

open Orsak
open Orsak.Myriad
open Orsak.Myriad.Gen

open System.Threading
open Microsoft.Extensions.Caching.Memory

[<GenRunner>]
type ITest =
    inherit IProvide<CancellationTokenSource>
    inherit IProvide<IMemoryCache>
    inherit IProvide<IRandomGenerator>



