/// Effects written in a quotation, as route handlers and scaling jobs capture them: deserializing a quotation binds
/// each builder method by name, which fails for overloads that share a compiled name.
module Orsak.Tests.QuotationTests

open System.Threading.Tasks
open FSharp.Control
open Microsoft.FSharp.Quotations.Patterns
open Orsak
open Xunit

type Capture =
    /// Captures `f` as the routing does, deserializing its quotation, and returns its value.
    static member Of([<ReflectedDefinition(includeValue = true)>] f: Quotations.Expr<unit -> 'a>) : unit -> 'a =
        match f with
        | WithValue(value, _, _) -> value :?> unit -> 'a
        | _ -> failwith "captured without its value"

let inner () : Effect<unit, int, string> = eff { return 1 }

let below limit (n: int ref) : Effect<unit, bool, string> = eff { return n.Value < limit }

let run (f: unit -> Effect<unit, 'a, string>) = Effect.runOrFail () (f ())

[<Fact>]
let ``return! of an effect can be quoted`` () = task {
    let! a = run (Capture.Of(fun () -> eff { return! inner () }))
    Assert.Equal(1, a)
}

[<Fact>]
let ``return! of a result and an async can be quoted`` () = task {
    let! a = run (Capture.Of(fun () -> eff { return! (Ok 2: Result<int, string>) }))
    let! b = run (Capture.Of(fun () -> eff { return! async { return 3 } }))
    Assert.Equal((2, 3), (a, b))
}

[<Fact>]
let ``let! of an effect, a task, a value task, an async and a result can be quoted`` () = task {
    let! a =
        run (
            Capture.Of(fun () -> eff {
                let! a = inner ()
                let! b = Task.FromResult 2
                let! c = ValueTask<int>(3)
                let! d = async { return 4 }
                let! e = (Ok 5: Result<int, string>)
                return a + b + c + d + e
            })
        )

    Assert.Equal(15, a)
}

[<Fact>]
let ``a final do! of a task can be quoted`` () = task {
    do! run (Capture.Of(fun () -> eff { do! Task.CompletedTask }))
}

[<Fact>]
let ``for over a list, a struct tuple and an async sequence, and while on a value task and an effect, can be quoted`` () = task {
    let! sum =
        run (
            Capture.Of(fun () -> eff {
                let mutable sum = 0

                for i in [ 1; 2 ] do
                    sum <- sum + i

                for i in struct (3, 4) do
                    sum <- sum + i

                for i in TaskSeq.ofList [ 5 ] do
                    sum <- sum + i

                let n = ref 0

                while ValueTask<bool>(n.Value < 2) do
                    n.Value <- n.Value + 1

                while below 4 n do
                    n.Value <- n.Value + 1

                return sum + n.Value
            })
        )

    Assert.Equal(19, sum)
}

[<Fact>]
let ``Effect Create can be quoted`` () = task {
    let! a =
        run (
            Capture.Of(fun () -> eff {
                let! a = Effect.Create(fun () -> Task.FromResult 1)
                let! b = Effect.Create(fun () -> (Ok 2: Result<int, string>))
                return a + b
            })
        )

    Assert.Equal(3, a)
}

[<Fact>]
let ``effSeq with let!, for and yield! can be quoted`` () = task {
    let items =
        Capture.Of(fun () -> (effSeq {
            let! a = Task.FromResult 1
            yield a

            for b in [ 2 ] do
                yield b

            yield! inner ()
        } : EffSeq<unit, int, string>))

    let values = ResizeArray<int>()

    do!
        Effect.runOrFail
            ()
            (eff {
                for v in items () do
                    values.Add v
            })

    Assert.Equal<int list>([ 1; 2; 1 ], List.ofSeq values)
}
