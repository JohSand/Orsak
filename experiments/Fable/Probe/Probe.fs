module Probe

open System
open System.Threading
open System.Threading.Tasks

// 1. task { } and Task<'T>
let taskCe (x: int) : Task<int> = task {
    let! a = Task.FromResult x
    do! Task.Delay 1
    return a + 1
}

// 2. async { } and Async<'T>
let asyncCe (x: int) : Async<int> = async {
    let! a = async { return x }
    do! Async.Sleep 1
    return a + 1
}

// 3. cancellation
let cancellation () =
    use cts = new CancellationTokenSource()
    cts.Cancel()
    cts.Token.IsCancellationRequested

// 4. an effect as a delegate over an interface environment, with a Result
type IClock =
    abstract Now: unit -> DateTime

type EffectDelegate<'r, 'a, 'e> = delegate of 'r -> Async<Result<'a, 'e>>

[<Struct>]
type Effect<'r, 'a, 'e> = Effect of EffectDelegate<'r, 'a, 'e>

let run (env: 'r) (Effect d: Effect<'r, 'a, 'e>) = d.Invoke env

let now () : Effect<#IClock, DateTime, string> =
    Effect(EffectDelegate(fun (c: #IClock) -> async { return Ok(c.Now()) }))

// 5. type test against an interface, as Time.delay does
let isClock (o: obj) =
    match o with
    | :? IClock -> true
    | _ -> false

// 6. a CE builder with Bind overloads for several types (no resumable code)
type EffBuilder() =
    member _.Return(x) : Effect<'r, 'a, 'e> = Effect(EffectDelegate(fun _ -> async { return Ok x }))
    member _.Bind(e: Effect<'r, 'a, 'e>, f: 'a -> Effect<'r, 'b, 'e>) : Effect<'r, 'b, 'e> =
        Effect(EffectDelegate(fun r -> async {
            match! run r e with
            | Ok a -> return! run r (f a)
            | Error e -> return Error e
        }))
    member _.Bind(t: Task<'a>, f: 'a -> Effect<'r, 'b, 'e>) : Effect<'r, 'b, 'e> =
        Effect(EffectDelegate(fun r -> async {
            let! a = Async.AwaitTask t
            return! run r (f a)
        }))
    member _.Bind(res: Result<'a, 'e>, f: 'a -> Effect<'r, 'b, 'e>) : Effect<'r, 'b, 'e> =
        match res with
        | Ok a -> f a
        | Error e -> Effect(EffectDelegate(fun _ -> async { return Error e }))
    member _.Zero() : Effect<'r, unit, 'e> = Effect(EffectDelegate(fun _ -> async { return Ok() }))

let eff = EffBuilder()

let workflow () = eff {
    let! t = now ()
    let! n = taskCe 1
    let! m = (Ok 2 : Result<int, string>)
    return t.AddDays(float (n + m))
}

// 7. IAsyncEnumerable
let asyncEnumerable (xs: Collections.Generic.IAsyncEnumerable<int>) = xs.GetAsyncEnumerator()

// 8. ValueTask: not supported, so only compiled with --define VALUETASK
#if VALUETASK
let valueTask () = ValueTask<int>(1)
#endif
