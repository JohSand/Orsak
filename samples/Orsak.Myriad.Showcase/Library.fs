namespace Orsak.Myriad.Showcase

open Orsak
open Orsak.Myriad
open System.Threading.Tasks

[<GenEffects>]
type IFace =
    abstract CountBeans: int * string -> Result<int, string>
    abstract PushButton: unit -> Task<unit>
/////
///
[<GenRunner(Name = "BasicRunner")>]
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

type IBeanCounter =
    abstract Count: unit -> int

type IBeanCounterProvider =
    abstract Effect: IBeanCounter

/// Generates AppEnvironment.create, which implements every provider from an anonymous record.
[<GenEnvironment>]
type IAppEnvironment =
    inherit IBeanCounterProvider
    inherit IGuidGenProvider
    inherit IRandomProvider
