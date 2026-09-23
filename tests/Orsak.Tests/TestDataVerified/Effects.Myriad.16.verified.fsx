namespace Tmp
module Company.App.Clock

open System
open Orsak.Myriad

[<GenEffects>]
type IClock =
    abstract Now: unit -> DateTimeOffset

namespace Company.App

open Company.App.Clock
open Orsak

type IClockProvider =
    abstract Effect: IClock

module Clock =
    let now () =
        Effect.Create(fun (er: #IClockProvider) -> er.Effect.Now())
