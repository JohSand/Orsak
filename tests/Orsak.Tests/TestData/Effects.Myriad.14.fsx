module Company.App.Composition

open Orsak.Myriad

type IFoo =
    abstract Foo: unit -> int

type IFooProvider =
    abstract Effect: IFoo

[<GenEnvironment>]
type IEnvironment =
    inherit IFooProvider
