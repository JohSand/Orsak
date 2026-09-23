module rec Company.App.Clock

open System
open Orsak
open Orsak.Myriad

// The environment inherits IClockProvider, which is generated further down the same file.
[<GenEffects(Inline = true)>]
type IClock =
    abstract Now: unit -> DateTimeOffset

[<GenEnvironment(Inline = true)>]
type IClockEnvironment =
    inherit IClockProvider
