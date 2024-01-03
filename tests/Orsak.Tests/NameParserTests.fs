module Orsak.Tests.NameParserTests

open Xunit
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations

type Helper =
    static member Convert([<ReflectedDefinition(includeValue = true)>] a: Expr<_>) =
        match a with
        | WithValue(_, _, expr) -> Orsak.AspNetCore.Helpers.getNames expr
        | _ -> failwith ""

    static member ConvertMethodInfo([<ReflectedDefinition(includeValue = true)>] a: Expr<_>) =
        match a with
        | WithValue(_, _, expr) -> Orsak.AspNetCore.Helpers.getMethodInfo expr
        | _ -> failwith ""

let singleArgFunction (i: int) = ignore i
let twoArgFunction (i: int) (j: int) = ignore (i, j)

let twoTupleFunction (i: int, j: int) = ignore (i, j)

let mixedFunction (i: int, j: int) (k: int) = ignore (i, j, k)
let threeTupleFunction (i: int, j: int, k: int) = ignore (i, j, k)

let mixedThreeTupleFunction (i: int, j: int, k: int) (l: int) = ignore (i, j, k, l)

let fourTupleFunction (i: int, j: int, k: int, l: int) = ignore (i, j, k, l)
let mixedFourTupleFunction (i: int, j: int, k: int, l: int) (m: int) = ignore (i, j, k, l, m)

[<Fact>]
let ``Can parse single argument`` () =
    let s = Helper.Convert(singleArgFunction)
    Assert.Equal<_ array>([| "i" |], s)

[<Fact>]
let ``Can parse single argument annonymous function`` () =
    let s = Helper.Convert(fun (i: int) -> ignore i)
    Assert.Equal<_ array>([| "i" |], s)

[<Fact>]
let ``Can parse single argument lambda`` () =
    let s = Helper.Convert(fun (i: int) -> ignore i)
    Assert.Equal<_ array>([| "i" |], s)

[<Fact>]
let ``Can parse two argument`` () =
    let s = Helper.Convert(twoArgFunction)
    Assert.Equal<_ array>([| "i"; "j" |], s)

[<Fact>]
let ``Can parse two argument lambda`` () =
    let s = Helper.Convert(fun (i: int) (j: int) -> ignore (i, j))
    Assert.Equal<_ array>([| "i"; "j" |], s)

[<Fact>]
let ``Can parse two tuple argument`` () =
    let s = Helper.Convert(twoTupleFunction)
    Assert.Equal<_ array>([| "i"; "j" |], s)

[<Fact>]
let ``Can parse two tuple lambda`` () =
    let s = Helper.Convert(fun (i: int, j: int) -> ignore (i, j))
    Assert.Equal<_ array>([| "i"; "j" |], s)

[<Fact>]
let ``Can parse two arguments one which is tuple`` () =
    let s = Helper.Convert(mixedFunction)
    Assert.Equal<_ array>([| "i"; "j"; "k" |], s)

[<Fact>]
let ``Can parse three tuple argument`` () =
    let s = Helper.Convert(threeTupleFunction)
    Assert.Equal<_ array>([| "i"; "j"; "k" |], s)

[<Fact>]
let ``Can parse three tuple mixed arguments`` () =
    let s = Helper.Convert(mixedThreeTupleFunction)

    Assert.Equal<_ array>(
        [|
            "i"
            "j"
            "k"
            "l"
        |],
        s
    )

[<Fact>]
let ``Can parse four tuple argument`` () =
    let s = Helper.Convert(fourTupleFunction)

    Assert.Equal<_ array>(
        [|
            "i"
            "j"
            "k"
            "l"
        |],
        s
    )

[<Fact>]
let ``Can parse four tuple mixed arguments`` () =
    let s = Helper.Convert(mixedFourTupleFunction)

    Assert.Equal<_ array>(
        [|
            "i"
            "j"
            "k"
            "l"
            "m"
        |],
        s
    )

[<Fact>]
let ``Can get method info`` () =
    let s = Helper.ConvertMethodInfo(mixedFourTupleFunction)

    Assert.Equal(nameof mixedFourTupleFunction, s.Name)
