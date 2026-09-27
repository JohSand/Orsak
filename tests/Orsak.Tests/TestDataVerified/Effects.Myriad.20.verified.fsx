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

// declares that it grows into IScopedEnvironment, in a scope of ITransaction
[<GenEnvironment>]
type IAppEnvironment =
    inherit ITestEffect1Provider
    inherit CompletableScopeProvider<ITransaction>
    inherit ExceptionHandler<string>
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
    let extend (outer: IAppEnvironment) (effects: {| Transaction: ITransaction |}) : IScopedEnvironment =
        { new IScopedEnvironment

          interface ITestEffect1Provider with
              member _.Effect = (outer :> ITestEffect1Provider).Effect
          interface ITransactionProvider with
              member _.Effect = effects.Transaction
          interface CompletableScopeProvider<ITransaction> with
              member _.BeginScope() =
                  (outer :> CompletableScopeProvider<ITransaction>).BeginScope()
          interface ExceptionHandler<string> with
              member _.Handle e =
                  (outer :> ExceptionHandler<string>).Handle e
          interface IGrowsInto<ITransaction, IScopedEnvironment> with
              member _.Grow scope =
                  (outer :> IGrowsInto<ITransaction, IScopedEnvironment>).Grow scope
        }

    let create
        (effects:
            {|
                TestEffect1: ITestEffect1
                Transaction: ITransaction
                TransactionScope: CompletableScopeProvider<ITransaction>
                ExceptionHandler: ExceptionHandler<string>
            |})
        : IScopedEnvironment =
        { new IScopedEnvironment

          interface ITestEffect1Provider with
              member _.Effect = effects.TestEffect1
          interface ITransactionProvider with
              member _.Effect = effects.Transaction
          interface CompletableScopeProvider<ITransaction> with
              member _.BeginScope() = effects.TransactionScope.BeginScope()
          interface ExceptionHandler<string> with
              member _.Handle e = effects.ExceptionHandler.Handle e
          interface IGrowsInto<ITransaction, IScopedEnvironment> with
              member this.Grow scope =
                  ScopedEnvironment.extend (this :?> IAppEnvironment) {| Transaction = scope |}
        }

    let run effects = Effect.run (create effects)
    let runOrFail effects = Effect.runOrFail (create effects)

module AppEnvironment =
    let create
        (effects:
            {|
                TestEffect1: ITestEffect1
                TransactionScope: CompletableScopeProvider<ITransaction>
                ExceptionHandler: ExceptionHandler<string>
            |})
        : IAppEnvironment =
        { new IAppEnvironment

          interface ITestEffect1Provider with
              member _.Effect = effects.TestEffect1
          interface CompletableScopeProvider<ITransaction> with
              member _.BeginScope() = effects.TransactionScope.BeginScope()
          interface ExceptionHandler<string> with
              member _.Handle e = effects.ExceptionHandler.Handle e
          interface IGrowsInto<ITransaction, IScopedEnvironment> with
              member this.Grow scope =
                  ScopedEnvironment.extend (this :?> IAppEnvironment) {| Transaction = scope |}
        }

    let run effects = Effect.run (create effects)
    let runOrFail effects = Effect.runOrFail (create effects)
