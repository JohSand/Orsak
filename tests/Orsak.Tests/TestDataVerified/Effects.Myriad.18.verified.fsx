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

type IClockProvider =
    abstract Effect: IClock

module Clock =
    let now () =
        Effect.Create(fun (er: #IClockProvider) -> er.Effect.Now())

module ClockEnvironment =
    let create (effects: {| Clock: IClock |}) : IClockEnvironment =
        { new IClockEnvironment

          interface IClockProvider with
              member _.Effect = effects.Clock
        }

    let run effects = Effect.run (create effects)
    let runOrFail effects = Effect.runOrFail (create effects)
