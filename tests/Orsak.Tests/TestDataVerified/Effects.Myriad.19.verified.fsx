open Orsak
open Orsak.Myriad

type ITestEffect1 =
    abstract Test: unit -> int

type ITestEffect1Provider =
    abstract Effect: ITestEffect1

type ITestEffect2 =
    abstract Test: unit -> int

type ITestEffect2Provider =
    abstract Effect: ITestEffect2

[<GenEnvironment>]
type IAppEnvironment =
    inherit ITestEffect1Provider
    inherit IGuidGenProvider

// extends IAppEnvironment, so it also gets an extend function
[<GenEnvironment>]
type IScopedEnvironment =
    inherit IAppEnvironment
    inherit ITestEffect2Provider
    inherit IGuidGenProvider

// adds nothing of its own, so it only gets create
[<GenEnvironment>]
type IAliasEnvironment =
    inherit IAppEnvironment

namespace Tmp

open Orsak
open Orsak.Myriad

module AppEnvironment =
    let create (effects: {| TestEffect1: ITestEffect1; GuidGenerator: Orsak.IGuidGenerator |}) : IAppEnvironment =
        { new IAppEnvironment

          interface ITestEffect1Provider with
              member _.Effect = effects.TestEffect1
          interface IGuidGenProvider with
              member _.GuidGenerator = effects.GuidGenerator
        }

    let run effects = Effect.run (create effects)
    let runOrFail effects = Effect.runOrFail (create effects)

module ScopedEnvironment =
    let extend (outer: IAppEnvironment) (effects: {| TestEffect2: ITestEffect2 |}) : IScopedEnvironment =
        { new IScopedEnvironment

          interface ITestEffect1Provider with
              member _.Effect = (outer :> ITestEffect1Provider).Effect
          interface IGuidGenProvider with
              member _.GuidGenerator = (outer :> IGuidGenProvider).GuidGenerator
          interface ITestEffect2Provider with
              member _.Effect = effects.TestEffect2
        }

    let create
        (effects: {| TestEffect1: ITestEffect1; GuidGenerator: Orsak.IGuidGenerator; TestEffect2: ITestEffect2 |})
        : IScopedEnvironment =
        { new IScopedEnvironment

          interface ITestEffect1Provider with
              member _.Effect = effects.TestEffect1
          interface IGuidGenProvider with
              member _.GuidGenerator = effects.GuidGenerator
          interface ITestEffect2Provider with
              member _.Effect = effects.TestEffect2
        }

    let run effects = Effect.run (create effects)
    let runOrFail effects = Effect.runOrFail (create effects)

module AliasEnvironment =
    let create (effects: {| TestEffect1: ITestEffect1; GuidGenerator: Orsak.IGuidGenerator |}) : IAliasEnvironment =
        { new IAliasEnvironment

          interface ITestEffect1Provider with
              member _.Effect = effects.TestEffect1
          interface IGuidGenProvider with
              member _.GuidGenerator = effects.GuidGenerator
        }

    let run effects = Effect.run (create effects)
    let runOrFail effects = Effect.runOrFail (create effects)
