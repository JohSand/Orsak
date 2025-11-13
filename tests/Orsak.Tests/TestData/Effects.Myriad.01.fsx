#r "../bin/Debug/net9.0/Orsak.dll"
#r "../bin/Debug/net9.0/Orsak.Myriad.dll"

open System
open Orsak
open Orsak.Myriad

[<GenRunner(Name = "Test")>]
type IProvide2W0 =
    inherit IProvide<IGuidGenerator>
    inherit IProvide<TimeProvider>
