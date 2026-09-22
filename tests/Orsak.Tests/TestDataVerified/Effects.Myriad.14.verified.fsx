module Company.App.Composition

open Orsak.Myriad

type IFoo =
    abstract Foo: unit -> int

type IFooProvider =
    abstract Effect: IFoo

[<GenEnvironment>]
type IEnvironment =
    inherit IFooProvider

namespace Company.App

open Company.App.Composition
open Orsak.Myriad

module Environment =
    let create (effects: {| Foo: IFoo |}) : IEnvironment =
        { new IEnvironment

          interface IFooProvider with
              member _.Effect = effects.Foo
        }
