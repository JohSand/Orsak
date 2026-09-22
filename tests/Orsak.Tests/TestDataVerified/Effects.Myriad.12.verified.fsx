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

namespace Tmp

open Orsak
open Orsak.Myriad

module TestEffect =
    let create
        (effects: {| TestEffect1: ITestEffect1; TestEffect2: ITestEffect2; TestEffect3: ITestEffect3 |})
        : ITestEffect =
        { new ITestEffect

          interface ITestEffect1Provider with
              member _.Effect = effects.TestEffect1
          interface ITestEffect2Provider with
              member _.Effect = effects.TestEffect2
          interface ITestEffect3Provider with
              member _.Effect = effects.TestEffect3
        }
