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
