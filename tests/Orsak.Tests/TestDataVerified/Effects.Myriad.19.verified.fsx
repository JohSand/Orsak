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

type ITestEffect3 =
    abstract Test: unit -> int

type ITestEffect3Provider =
    abstract Effect: ITestEffect3

[<GenEnvironment>]
type IAppEnvironment =
    inherit ITestEffect1Provider
    inherit IGuidGenProvider

// extends IAppEnvironment, so it also gets an extend function, taking the one effect it adds
[<GenEnvironment>]
type IScopedEnvironment =
    inherit IAppEnvironment
    inherit ITestEffect2Provider
    inherit IGuidGenProvider

// adds two effects, so its extend function takes a record of them
[<GenEnvironment>]
type IWideEnvironment =
    inherit IAppEnvironment
    inherit ITestEffect2Provider
    inherit ITestEffect3Provider

// these two add the same effect, so neither gets an Extend member, as both would take the same type
[<GenEnvironment>]
type ISiblingEnvironment =
    inherit IAppEnvironment
    inherit ITestEffect3Provider

[<GenEnvironment>]
type ITwinEnvironment =
    inherit IAppEnvironment
    inherit ITestEffect3Provider

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
    let extend (outer: IAppEnvironment) (effect: ITestEffect2) : IScopedEnvironment =
        { new IScopedEnvironment

          interface ITestEffect1Provider with
              member _.Effect = (outer :> ITestEffect1Provider).Effect
          interface IGuidGenProvider with
              member _.GuidGenerator = (outer :> IGuidGenProvider).GuidGenerator
          interface ITestEffect2Provider with
              member _.Effect = effect
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

module WideEnvironment =
    let extend
        (outer: IAppEnvironment)
        (effects: {| TestEffect2: ITestEffect2; TestEffect3: ITestEffect3 |})
        : IWideEnvironment =
        { new IWideEnvironment

          interface ITestEffect1Provider with
              member _.Effect = (outer :> ITestEffect1Provider).Effect
          interface IGuidGenProvider with
              member _.GuidGenerator = (outer :> IGuidGenProvider).GuidGenerator
          interface ITestEffect2Provider with
              member _.Effect = effects.TestEffect2
          interface ITestEffect3Provider with
              member _.Effect = effects.TestEffect3
        }

    let create
        (effects:
            {|
                TestEffect1: ITestEffect1
                GuidGenerator: Orsak.IGuidGenerator
                TestEffect2: ITestEffect2
                TestEffect3: ITestEffect3
            |})
        : IWideEnvironment =
        { new IWideEnvironment

          interface ITestEffect1Provider with
              member _.Effect = effects.TestEffect1
          interface IGuidGenProvider with
              member _.GuidGenerator = effects.GuidGenerator
          interface ITestEffect2Provider with
              member _.Effect = effects.TestEffect2
          interface ITestEffect3Provider with
              member _.Effect = effects.TestEffect3
        }

    let run effects = Effect.run (create effects)
    let runOrFail effects = Effect.runOrFail (create effects)

module SiblingEnvironment =
    let extend (outer: IAppEnvironment) (effect: ITestEffect3) : ISiblingEnvironment =
        { new ISiblingEnvironment

          interface ITestEffect1Provider with
              member _.Effect = (outer :> ITestEffect1Provider).Effect
          interface IGuidGenProvider with
              member _.GuidGenerator = (outer :> IGuidGenProvider).GuidGenerator
          interface ITestEffect3Provider with
              member _.Effect = effect
        }

    let create
        (effects: {| TestEffect1: ITestEffect1; GuidGenerator: Orsak.IGuidGenerator; TestEffect3: ITestEffect3 |})
        : ISiblingEnvironment =
        { new ISiblingEnvironment

          interface ITestEffect1Provider with
              member _.Effect = effects.TestEffect1
          interface IGuidGenProvider with
              member _.GuidGenerator = effects.GuidGenerator
          interface ITestEffect3Provider with
              member _.Effect = effects.TestEffect3
        }

    let run effects = Effect.run (create effects)
    let runOrFail effects = Effect.runOrFail (create effects)

module TwinEnvironment =
    let extend (outer: IAppEnvironment) (effect: ITestEffect3) : ITwinEnvironment =
        { new ITwinEnvironment

          interface ITestEffect1Provider with
              member _.Effect = (outer :> ITestEffect1Provider).Effect
          interface IGuidGenProvider with
              member _.GuidGenerator = (outer :> IGuidGenProvider).GuidGenerator
          interface ITestEffect3Provider with
              member _.Effect = effect
        }

    let create
        (effects: {| TestEffect1: ITestEffect1; GuidGenerator: Orsak.IGuidGenerator; TestEffect3: ITestEffect3 |})
        : ITwinEnvironment =
        { new ITwinEnvironment

          interface ITestEffect1Provider with
              member _.Effect = effects.TestEffect1
          interface IGuidGenProvider with
              member _.GuidGenerator = effects.GuidGenerator
          interface ITestEffect3Provider with
              member _.Effect = effects.TestEffect3
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

[<AutoOpen>]
module AppEnvironmentExtensions =
    type IAppEnvironment with
        member this.Extend(effect: ITestEffect2) = ScopedEnvironment.extend this effect

        member this.Extend(effects: {| TestEffect2: ITestEffect2; TestEffect3: ITestEffect3 |}) =
            WideEnvironment.extend this effects
