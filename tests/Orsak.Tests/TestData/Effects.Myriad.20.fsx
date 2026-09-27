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
