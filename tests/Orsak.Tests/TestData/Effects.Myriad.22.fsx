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
