#r "../bin/Debug/net10.0/Orsak.dll"
#r "../bin/Debug/net10.0/Orsak.Myriad.dll"

open System
open System.Threading
open Orsak
open Orsak.Myriad

[<GenRunner(UserInterfaceName = true)>]
type IRunner1 =
    inherit IProvide<IGuidGenerator>
    inherit IProvide<TimeProvider>

[<GenRunner(UserInterfaceName = true)>]
type IRunner2 =
    inherit IProvide<IGuidGenerator>
    inherit IProvide<CancellationTokenSource>

[<GenRunner(UserInterfaceName = true)>]
type IRunner3 =
    inherit IProvide<IGuidGenerator>
    inherit IProvide<IRandom>
    inherit IProvide<CancellationTokenSource>
