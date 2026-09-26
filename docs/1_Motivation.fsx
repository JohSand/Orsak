(**
---
title: Introduction
category: Motivation
categoryindex: 1
index: 1
---
*)
(*** condition: prepare ***)
#r "nuget: Microsoft.Extensions.Logging.Abstractions, 8.0.0"
#r "../src/Orsak/bin/Release/net8.0/Orsak.dll"

open System.Threading.Tasks
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Logging.Abstractions
(**
### Basic usage

This is a library intended to make effectful code easier to manage. It allows the user to easily create and consume
side-effects modelled as interfaces, by making use of computation expressions.

Imagine having the interface
*)

type IButtonPusher =
    abstract member PushButton: unit -> Task<unit>

(**
describing some business-critical capability. To integrate that with the effect framework, we create the following helpers:
*)
type IButtonPusherProvider =
    abstract member ButtonPusher: IButtonPusher

open Orsak

module ButtonPusher =
    let pushButton () =
        Effect.Create(fun (provider: #IButtonPusherProvider) -> provider.ButtonPusher.PushButton())

(**
Then, we can write code like
*)

let work () : Effect<#IButtonPusherProvider, unit, _> = eff {
    //code
    do! ButtonPusher.pushButton ()
//more code
}

(**
Which can be run with some type which implements ``IButtonPusherProvider``.

The big win with effects is that they compose. By using a flexible constraint in the effect creation function, we can
simply add effects, which will constrain the effect runner type to implement both ``IButtonPusherProvider`` and ``ILogProvider``
*)
type ILogProvider =
    abstract member Logger: ILogger

let logInformation (message: string) =
    Effect.Create(fun (provider: #ILogProvider) -> provider.Logger.LogInformation(message))

let workThenLog () = eff {
    do! work ()
    do! logInformation "Did the work."
}

(** Then, in the composition root of the program, one could define: *)
type EffectRunner(buttonPusher: IButtonPusher) =
    interface ILogProvider with
        member _.Logger = NullLogger.Instance

    interface IButtonPusherProvider with
        member _.ButtonPusher = buttonPusher
(** And then run the effects: *)
let runner =
    EffectRunner(
        { new IButtonPusher with
            member this.PushButton() = Task.FromResult()
        }
    )

let result: ValueTask<Result<unit, string>> = workThenLog () |> Effect.run runner
(**
Running an effect returns a `ValueTask` of a `Result`, whose error type is left open by `work` and chosen here.
Nothing happens until then, so the same effect can be run again, with the same runner or with another one, such as
one with fakes in a test.
*)
