namespace rec Company.Tests

open Orsak
open Orsak.Myriad

type IFoo =
    abstract Foo: unit -> int

type IFooProvider =
    abstract Effect: IFoo

// An ad hoc environment for the tests in this file. Its create function is appended to the end
// of the file, which the tests above can use since the namespace is rec.
[<GenEnvironment(Inline = true)>]
type ITestEnvironment =
    inherit IFooProvider
    inherit IGuidGenProvider

module FooTests =
    let ``uses the generated environment`` () =
        TestEnvironment.create {|
            Foo = { new IFoo with member _.Foo() = 1 }
            GuidGenerator = GuidGenerator.defaultGen ()
        |}
