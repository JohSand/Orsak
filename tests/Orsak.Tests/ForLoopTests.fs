module Orsak.Tests.ForLoopTests

open System.Collections.Generic
open System.Threading.Tasks
open Orsak
open Xunit

let private record (seen: ResizeArray<'a>) (x: 'a) : Effect<unit, unit, string> =
    mkEffect (fun _ ->
        seen.Add x
        ValueTask.FromResult(Ok()))

/// Builds `for x in items do do! f x` by calling the builder members directly, with the
/// loop body as a first-class function value rather than an inline lambda. That places a
/// resumption point inside a non-inlined closure, so the state machine cannot be compiled
/// statically and `Run` falls back to the dynamic path - the same path an ordinary
/// `eff { for .. in .. do }` takes whenever the compiler reports FS3511 for the block.
///
/// The FS3501 ("must be marked 'inline'") and FS3511 ("not statically compilable") warnings
/// on the two helpers below are expected, and are what makes these tests meaningful: if they
/// ever stop being reported, the loops compile statically and no longer cover the dynamic
/// implementation of For. Do not silence them.
let private dynamicFor (items: seq<'a>) (f: 'a -> Effect<unit, unit, string>) =
    let body = fun x -> eff.ReturnFrom(f x)
    eff.Run(eff.Delay(fun () -> eff.For(items, body)))

let private dynamicForAsync (items: IAsyncEnumerable<'a>) (f: 'a -> Effect<unit, unit, string>) =
    let body = fun x -> eff.ReturnFrom(f x)
    eff.Run(eff.Delay(fun () -> eff.For(items, body)))

let private asyncSeq (items: 'a list) : IAsyncEnumerable<'a> =
    { new IAsyncEnumerable<'a> with
        member _.GetAsyncEnumerator(_) =
            let remaining = ref items
            let current = ref Unchecked.defaultof<'a>

            { new IAsyncEnumerator<'a> with
                member _.Current = current.Value

                member _.MoveNextAsync() =
                    match remaining.Value with
                    | [] -> ValueTask.FromResult false
                    | x :: rest ->
                        current.Value <- x
                        remaining.Value <- rest
                        ValueTask.FromResult true

                member _.DisposeAsync() = ValueTask.CompletedTask }
    }

/// The same loops in ordinary `for .. in .. do` syntax. Which state machine these compile to
/// is the compiler's choice and has changed between SDK versions - dotnet 10.0.400 moved blocks
/// like these onto the dynamic path, which is how the For bugs surfaced in the first place. On a
/// toolchain that compiles them statically they pass whatever For does, so they are a guard for
/// future toolchains rather than a reproduction; the dynamicFor tests above are the reproduction.
let private plainFor (items: seq<'a>) (f: 'a -> Effect<unit, unit, string>) = eff {
    for x in items do
        do! f x
}

let private plainForAsync (items: IAsyncEnumerable<'a>) (f: 'a -> Effect<unit, unit, string>) = eff {
    for x in items do
        do! f x
}

[<Fact>]
let ``for over an F# list on the dynamic path visits every element`` () = task {
    let seen = ResizeArray()
    do! dynamicFor [ 1; 2; 3 ] (record seen) |> Effect.runOrFail ()
    Assert.Equal<int list>([ 1; 2; 3 ], List.ofSeq seen)
}

[<Fact>]
let ``for over a ResizeArray on the dynamic path visits every element`` () = task {
    let seen = ResizeArray()
    do! dynamicFor (ResizeArray [ "a"; "b"; "c" ]) (record seen) |> Effect.runOrFail ()
    Assert.Equal<string list>([ "a"; "b"; "c" ], List.ofSeq seen)
}

[<Fact>]
let ``for over an empty sequence on the dynamic path runs the body zero times`` () = task {
    let seen = ResizeArray()
    do! dynamicFor List.empty<int> (record seen) |> Effect.runOrFail ()
    Assert.Empty seen
}

[<Fact>]
let ``for over an IAsyncEnumerable on the dynamic path visits every element`` () = task {
    let seen = ResizeArray()
    do! dynamicForAsync (asyncSeq [ 1; 2; 3 ]) (record seen) |> Effect.runOrFail ()
    Assert.Equal<int list>([ 1; 2; 3 ], List.ofSeq seen)
}

[<Fact>]
let ``for over an F# list visits every element`` () = task {
    let seen = ResizeArray()
    do! plainFor [ 1; 2; 3 ] (record seen) |> Effect.runOrFail ()
    Assert.Equal<int list>([ 1; 2; 3 ], List.ofSeq seen)
}

[<Fact>]
let ``for over a ResizeArray visits every element`` () = task {
    let seen = ResizeArray()
    do! plainFor (ResizeArray [ "a"; "b"; "c" ]) (record seen) |> Effect.runOrFail ()
    Assert.Equal<string list>([ "a"; "b"; "c" ], List.ofSeq seen)
}

[<Fact>]
let ``for over an empty sequence runs the body zero times`` () = task {
    let seen = ResizeArray()
    do! plainFor List.empty<int> (record seen) |> Effect.runOrFail ()
    Assert.Empty seen
}

[<Fact>]
let ``for over an IAsyncEnumerable visits every element`` () = task {
    let seen = ResizeArray()
    do! plainForAsync (asyncSeq [ 1; 2; 3 ]) (record seen) |> Effect.runOrFail ()
    Assert.Equal<int list>([ 1; 2; 3 ], List.ofSeq seen)
}
