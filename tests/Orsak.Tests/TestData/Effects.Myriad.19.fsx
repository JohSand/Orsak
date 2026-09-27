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
