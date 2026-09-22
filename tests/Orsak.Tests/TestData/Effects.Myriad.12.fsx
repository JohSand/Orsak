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
type ITestEffect =
    inherit ITestEffect1Provider
    inherit ITestEffect2Provider
    inherit ITestEffect3Provider
