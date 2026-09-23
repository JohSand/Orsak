module Company.App.Clock

open System
open Orsak.Myriad

[<GenEffects>]
type IClock =
    abstract Now: unit -> DateTimeOffset
