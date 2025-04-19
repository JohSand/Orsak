namespace ConsoleApp1

open Orsak
open Orsak.Myriad
open System.Threading
open Microsoft.Extensions.Caching.Memory

[<GenRunner>]
type ITest =
    inherit IProvide<CancellationTokenSource>
    inherit IProvide<IMemoryCache>
    inherit IProvide<IRandomGenerator>

[<GenRunner>]
type ITest2 =
    inherit IProvide<CancellationTokenSource>
    inherit IProvide<IMemoryCache>

[<GenRunner>]
type ITest3 =
    inherit IProvide<IRandomGenerator>
    inherit IProvide<IMemoryCache>

[<GenRunner>]
type ITest4 =
    inherit IProvide<IRandomGenerator>
    inherit IProvide<CancellationTokenSource>
