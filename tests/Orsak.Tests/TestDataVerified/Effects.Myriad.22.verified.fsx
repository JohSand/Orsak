open Orsak
open Orsak.Myriad
open Orsak.Scoped

type ITestEffect1 =
    abstract Test: unit -> int

type ITestEffect1Provider =
    abstract Effect: ITestEffect1

// a CancellationScope is the source of the ICancellationProvider of the environment grown in it
[<GenEnvironment>]
type IAppEnvironment =
    inherit ITestEffect1Provider
    inherit IGrowsInto<CancellationScope, ICancellableEnvironment>

// the environment in the scope: the application's, with the scope as its source
and [<GenEnvironment>] ICancellableEnvironment =
    inherit IAppEnvironment
    inherit ICancellationProvider

namespace Tmp

open Orsak
open Orsak.Myriad
open Orsak.Scoped

module CancellableEnvironment =
    let extend (outer: IAppEnvironment) (effect: System.Threading.CancellationTokenSource) : ICancellableEnvironment =
        { new ICancellableEnvironment

          interface ITestEffect1Provider with
              member _.Effect = (outer :> ITestEffect1Provider).Effect
          interface ICancellationProvider with
              member _.Source = effect
          interface ScopeProvider<CancellationScope> with
              member _.BeginScope() =
                  (outer :> ScopeProvider<CancellationScope>).BeginScope()
          interface IGrowsInto<CancellationScope, ICancellableEnvironment> with
              member _.Grow scope =
                  (outer :> IGrowsInto<CancellationScope, ICancellableEnvironment>).Grow scope
        }

    let create
        (effects:
            {|
                TestEffect1: ITestEffect1
                CancellationTokenSource: System.Threading.CancellationTokenSource
                CancellationScopeScope: ScopeProvider<CancellationScope>
            |})
        : ICancellableEnvironment =
        { new ICancellableEnvironment

          interface ITestEffect1Provider with
              member _.Effect = effects.TestEffect1
          interface ICancellationProvider with
              member _.Source = effects.CancellationTokenSource
          interface ScopeProvider<CancellationScope> with
              member _.BeginScope() =
                  effects.CancellationScopeScope.BeginScope()
          interface IGrowsInto<CancellationScope, ICancellableEnvironment> with
              member this.Grow scope =
                  CancellableEnvironment.extend (this :?> IAppEnvironment) scope
        }

    let run effects = Effect.run (create effects)
    let runOrFail effects = Effect.runOrFail (create effects)

module AppEnvironment =
    let create
        (effects: {| TestEffect1: ITestEffect1; CancellationScopeScope: ScopeProvider<CancellationScope> |})
        : IAppEnvironment =
        { new IAppEnvironment

          interface ITestEffect1Provider with
              member _.Effect = effects.TestEffect1
          interface ScopeProvider<CancellationScope> with
              member _.BeginScope() =
                  effects.CancellationScopeScope.BeginScope()
          interface IGrowsInto<CancellationScope, ICancellableEnvironment> with
              member this.Grow scope =
                  CancellableEnvironment.extend (this :?> IAppEnvironment) scope
        }

    let run effects = Effect.run (create effects)
    let runOrFail effects = Effect.runOrFail (create effects)

[<AutoOpen>]
module AppEnvironmentExtensions =
    type IAppEnvironment with
        member this.Extend(effect: System.Threading.CancellationTokenSource) =
            CancellableEnvironment.extend this effect
