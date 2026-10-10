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
