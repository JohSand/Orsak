/// The Effect type's operators, as FSharpPlus finds them: its generic functions and operators resolve to the type's
/// static members, Map, Return, >>= and <*>.
module Orsak.Tests.FSharpPlusTests

open FSharpPlus
open Orsak
open Xunit

let ok a : Effect<unit, _, string> = eff { return a }

let error e : Effect<unit, _, string> = Effect.Error e

[<Fact>]
let ``FSharpPlus applies an effect of a function to an effect of its argument with <*>`` () = task {
    let! value = ok (sprintf "%i") <*> ok 2 |> Effect.runOrFail ()
    Assert.Equal("2", value)
}

[<Fact>]
let ``FSharpPlus lifts a function of two arguments over effects with <!> and <*>`` () = task {
    let! value = (fun a b -> a + b) <!> ok 1 <*> ok 2 |> Effect.runOrFail ()
    Assert.Equal(3, value)
}

[<Fact>]
let ``FSharpPlus combines the errors of both effects in <*>`` () = task {
    let! result = (error "a": Effect<unit, int -> int, string>) <*> error "b" |> Effect.run ()
    Assert.Equal(Error "ab", result)
}

[<Fact>]
let ``FSharpPlus maps and binds effects`` () = task {
    let! mapped = map (fun a -> a + 1) (ok 1) |> Effect.runOrFail ()
    let! bound = ok 1 >>= (fun a -> ok (a + 2)) |> Effect.runOrFail ()
    Assert.Equal((2, 3), (mapped, bound))
}
