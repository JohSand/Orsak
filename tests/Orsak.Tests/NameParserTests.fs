module Orsak.Tests.NameParserTests

open Xunit
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations

type Helper =
    static member Convert([<ReflectedDefinition(includeValue = true)>] a: Expr<_>) =
        match a with
        | WithValue(_, _, expr) -> (Orsak.AspNetCore.ReflectedFunction.read expr).ParameterNames
        | _ -> failwith ""

    static member ConvertMethodInfo([<ReflectedDefinition(includeValue = true)>] a: Expr<_>) =
        match a with
        | WithValue(_, _, expr) -> (Orsak.AspNetCore.ReflectedFunction.read expr).Method.Value

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

// ReflectedFunction.read, beyond the parameter names

type Shapes =
    static member Read([<ReflectedDefinition(includeValue = true)>] a: Expr<_>) =
        match a with
        | WithValue(_, _, expr) -> Orsak.AspNetCore.ReflectedFunction.read expr
        | _ -> failwith "captured without its value"

let add (a: int) (b: int) = a + b
let addOne = add 1
let curriedThenTupled (i: int) (j: int, k: int) = ignore (i, j, k)
let takesUnit () = 42

[<Fact>]
let ``read names a function passed by name after its method`` () =
    let shape = Shapes.Read(twoArgFunction)
    Assert.Equal(Some(nameof twoArgFunction), shape.Name)
    Assert.Equal(nameof twoArgFunction, shape.Method.Value.Name)

[<Fact>]
let ``read gives no method or name for a lambda doing more than calling a function`` () =
    let shape = Shapes.Read(fun (i: int) -> i + 3)
    Assert.Equal<_ array>([| "i" |], shape.ParameterNames)
    Assert.Equal(None, shape.Method)
    Assert.Equal(None, shape.Name)

[<Fact>]
let ``read finds the method of a function partially applied in place`` () =
    let shape = Shapes.Read(add 1)
    Assert.Equal<_ array>([| "b" |], shape.ParameterNames)
    Assert.Equal(Some(nameof add), shape.Name)

[<Fact>]
let ``read names a partial application bound to a name after it`` () =
    let shape = Shapes.Read(addOne)
    Assert.Equal(Some(nameof addOne), shape.Name)
    Assert.Equal(None, shape.Method)

[<Fact>]
let ``read reads a tupled parameter after a curried one`` () =
    let shape = Shapes.Read(curriedThenTupled)
    Assert.Equal<_ array>([| "i"; "j"; "k" |], shape.ParameterNames)
    Assert.Equal(Some(nameof curriedThenTupled), shape.Name)

[<Fact>]
let ``read finds the method of a function taking unit`` () =
    let shape = Shapes.Read(takesUnit)
    Assert.Equal(Some(nameof takesUnit), shape.Name)

[<Fact>]
let ``capture rejects a function quoted without its value`` () =
    Assert.Throws<System.ArgumentException>(fun () ->
        Orsak.AspNetCore.ReflectedFunction.capture <@ fun (i: int) -> i @> |> ignore)
    |> ignore
