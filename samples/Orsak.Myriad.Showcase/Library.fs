namespace Orsak.Myriad.Showcase

open Orsak
open Orsak.Myriad


[<GenEffects>]
type IFace =
    abstract CountBeans: int -> string -> Result<int, string>

[<GenRunner>]
type ITest =
    inherit IProvide<IFace>
    inherit IProvide<IRandomGenerator>
//
// [<GenRunner>]
// type ITest2 =
//     inherit IProvide<CancellationTokenSource>
//     inherit IProvide<IMemoryCache>
//
// [<GenRunner>]
// type ITest3 =
//     inherit IProvide<IRandomGenerator>
//     inherit IProvide<IMemoryCache>
//
// [<GenRunner>]
// type ITest4 =
//     inherit IProvide<IRandomGenerator>
//     inherit IProvide<CancellationTokenSource>
