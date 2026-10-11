open Orsak
open Orsak.Myriad
open Orsak.Scoped

type ITestEffect1 =
    abstract Test: unit -> int

type ITestEffect1Provider =
    abstract Effect: ITestEffect1

type ITransaction =
    inherit CompletableScope

type ITransactionProvider =
    abstract Effect: ITransaction

// IGrowsInto inherits ScopeProvider<ITransaction>, so the environment begins the scope without saying so
[<GenEnvironment>]
type IAppEnvironment =
    inherit ITestEffect1Provider
    inherit IGrowsInto<ITransaction, IScopedEnvironment>

// the environment in the scope: the application's, and the transaction
and [<GenEnvironment>] IScopedEnvironment =
    inherit IAppEnvironment
    inherit ITransactionProvider

namespace Tmp

open Orsak
open Orsak.Myriad
open Orsak.Scoped

module ScopedEnvironment =
    let extend (outer: IAppEnvironment) (effect: ITransaction) : IScopedEnvironment =
        { new IScopedEnvironment

          interface ITestEffect1Provider with
              member _.Effect = (outer :> ITestEffect1Provider).Effect
          interface ITransactionProvider with
              member _.Effect = effect
          interface ScopeProvider<ITransaction> with
              member _.BeginScope() =
                  (outer :> ScopeProvider<ITransaction>).BeginScope()
          interface IGrowsInto<ITransaction, IScopedEnvironment> with
              member _.Grow scope =
                  (outer :> IGrowsInto<ITransaction, IScopedEnvironment>).Grow scope
        }

    let create
        (effects:
            {| TestEffect1: ITestEffect1; Transaction: ITransaction; TransactionScope: ScopeProvider<ITransaction> |})
        : IScopedEnvironment =
        { new IScopedEnvironment

          interface ITestEffect1Provider with
              member _.Effect = effects.TestEffect1
          interface ITransactionProvider with
              member _.Effect = effects.Transaction
          interface ScopeProvider<ITransaction> with
              member _.BeginScope() = effects.TransactionScope.BeginScope()
          interface IGrowsInto<ITransaction, IScopedEnvironment> with
              member this.Grow scope =
                  ScopedEnvironment.extend (this :?> IAppEnvironment) scope
        }

    let run effects = Effect.run (create effects)
    let runOrFail effects = Effect.runOrFail (create effects)

module AppEnvironment =
    let create
        (effects: {| TestEffect1: ITestEffect1; TransactionScope: ScopeProvider<ITransaction> |})
        : IAppEnvironment =
        { new IAppEnvironment

          interface ITestEffect1Provider with
              member _.Effect = effects.TestEffect1
          interface ScopeProvider<ITransaction> with
              member _.BeginScope() = effects.TransactionScope.BeginScope()
          interface IGrowsInto<ITransaction, IScopedEnvironment> with
              member this.Grow scope =
                  ScopedEnvironment.extend (this :?> IAppEnvironment) scope
        }

    let run effects = Effect.run (create effects)
    let runOrFail effects = Effect.runOrFail (create effects)

[<AutoOpen>]
module AppEnvironmentExtensions =
    type IAppEnvironment with
        member this.Extend(effect: ITransaction) = ScopedEnvironment.extend this effect
