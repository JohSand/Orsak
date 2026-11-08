namespace Orsak

// The Fable implementation of Effect and eff, compiled instead of ../EffectBuilder.fs when FABLE_COMPILER is set.
// Fable can't translate resumable code or ValueTask, so effects run on Task, which Fable compiles to JavaScript
// promises, and eff is an ordinary builder over Effect itself. The types, members and their names follow the .NET
// implementation, so code using them compiles on both.

open System
open System.Collections.Generic
open System.Threading.Tasks

/// <summary>
/// The result of running an effect: a <see cref="T:System.Threading.Tasks.Task`1"/> of a
/// <see cref="T:Microsoft.FSharp.Core.FSharpResult`2"/>, which is a promise in JavaScript.
/// </summary>
type AsyncResult<'a, 'e> = Task<Result<'a, 'e>>

/// <summary>
/// The function an <see cref="T:Orsak.Effect`3"/> wraps: given an environment, it starts the effect.
/// </summary>
type EffectDelegate<'r, 'a, 'e> = delegate of 'r -> AsyncResult<'a, 'e>

/// <summary>
/// Describes an effect that can either succeed with <typeparamref name="'a"/>, or fail with <typeparamref name="'e"/>.
/// The effect is 'cold', and only starts when run with an <typeparamref name="'r"/>.
/// </summary>
[<Struct; NoComparison; NoEquality>]
type Effect<'r, 'a, 'e> =
    | Effect of EffectDelegate<'r, 'a, 'e>

    /// <summary>Starts the effect.</summary>
    member this.Run(r: 'r) : AsyncResult<'a, 'e> = let (Effect d) = this in d.Invoke r

    /// <summary>Starts the effect, and raises an exception in case of error. Prefer <see cref="Run"/>.</summary>
    member this.RunOrFail(r: 'r) : Task<'a> =
        let (Effect d) = this

        task {
            match! d.Invoke r with
            | Ok a -> return a
            | Error e -> return failwith (string e)
        }

/// <exclude/>
[<AutoOpen>]
module internal EffectHelpers =
    let inline mkEff (f: 'r -> AsyncResult<'a, 'e>) = Effect(EffectDelegate f)

    let inline succeed (a: 'a) : AsyncResult<'a, 'e> = Task.FromResult(Ok a)

/// <exclude/>
type EffBuilderBase() =

    member _.Delay(generator: unit -> Effect<'r, 'a, 'e>) : Effect<'r, 'a, 'e> =
        mkEff (fun r -> (generator ()).Run r)

    member _.Zero() : Effect<'r, unit, 'e> = mkEff (fun _ -> succeed ())

    member _.Return(value: 'a) : Effect<'r, 'a, 'e> = mkEff (fun _ -> succeed value)

    member _.Bind(eff: Effect<'r, 'a, 'e>, continuation: 'a -> Effect<'r, 'b, 'e>) : Effect<'r, 'b, 'e> =
        mkEff (fun r -> task {
            match! eff.Run r with
            | Ok a -> return! (continuation a).Run r
            | Error e -> return Error e
        })

    member _.Bind(t: Task<'a>, continuation: 'a -> Effect<'r, 'b, 'e>) : Effect<'r, 'b, 'e> =
        mkEff (fun r -> task {
            let! a = t
            return! (continuation a).Run r
        })

    member _.Bind(a: Async<'a>, continuation: 'a -> Effect<'r, 'b, 'e>) : Effect<'r, 'b, 'e> =
        mkEff (fun r -> task {
            let! value = Async.StartAsTask a
            return! (continuation value).Run r
        })

    member _.Bind(result: Result<'a, 'e>, continuation: 'a -> Effect<'r, 'b, 'e>) : Effect<'r, 'b, 'e> =
        match result with
        | Ok a -> continuation a
        | Error e -> mkEff (fun _ -> Task.FromResult(Error e))

    member _.ReturnFrom(eff: Effect<'r, 'a, 'e>) : Effect<'r, 'a, 'e> = eff

    member _.ReturnFrom(result: Result<'a, 'e>) : Effect<'r, 'a, 'e> = mkEff (fun _ -> Task.FromResult result)

    member this.ReturnFrom(a: Async<'a>) : Effect<'r, 'a, 'e> = this.Bind(a, this.Return)

    member this.ReturnFrom(t: Task<'a>) : Effect<'r, 'a, 'e> = this.Bind(t, this.Return)

    member this.Combine(first: Effect<'r, unit, 'e>, second: Effect<'r, 'a, 'e>) : Effect<'r, 'a, 'e> =
        this.Bind(first, (fun () -> second))

    member _.While(condition: unit -> bool, body: Effect<'r, unit, 'e>) : Effect<'r, unit, 'e> =
        mkEff (fun r -> task {
            let mutable result = Ok()

            while Result.isOk result && condition () do
                let! next = body.Run r
                result <- next

            return result
        })

    member _.While(condition: unit -> Task<bool>, body: Effect<'r, unit, 'e>) : Effect<'r, unit, 'e> =
        mkEff (fun r -> task {
            let mutable result = Ok()
            let! first = condition ()
            let mutable go = first

            while Result.isOk result && go do
                let! next = body.Run r
                result <- next

                if Result.isOk result then
                    let! again = condition ()
                    go <- again

            return result
        })

    member _.While(condition: unit -> Effect<'r, bool, 'e>, body: Effect<'r, unit, 'e>) : Effect<'r, unit, 'e> =
        mkEff (fun r -> task {
            let! first = (condition ()).Run r
            let mutable result = Result.map ignore first
            let mutable go = first = Ok true

            while go do
                let! next = body.Run r
                result <- next

                match result with
                | Ok() ->
                    let! again = (condition ()).Run r
                    result <- Result.map ignore again
                    go <- again = Ok true
                | Error _ -> go <- false

            return result
        })

    member _.For(s: seq<'a>, body: 'a -> Effect<'r, unit, 'e>) : Effect<'r, unit, 'e> =
        mkEff (fun r -> task {
            use enumerator = s.GetEnumerator()
            let mutable result = Ok()

            while Result.isOk result && enumerator.MoveNext() do
                let! next = (body enumerator.Current).Run r
                result <- next

            return result
        })

    member _.TryWith(body: Effect<'r, 'a, 'e>, handler: exn -> Effect<'r, 'a, 'e>) : Effect<'r, 'a, 'e> =
        mkEff (fun r -> task {
            try
                return! body.Run r
            with ex ->
                return! (handler ex).Run r
        })

    member _.TryFinally(body: Effect<'r, 'a, 'e>, compensation: unit -> unit) : Effect<'r, 'a, 'e> =
        mkEff (fun r -> task {
            try
                return! body.Run r
            finally
                compensation ()
        })

    member this.Using(resource: 'Resource :> IDisposable, body: 'Resource -> Effect<'r, 'a, 'e>) : Effect<'r, 'a, 'e> =
        this.TryFinally(
            this.Delay(fun () -> body resource),
            fun () ->
                if not (isNull (box resource)) then
                    resource.Dispose()
        )

    // and!: both effects start before either is awaited, and when both fail, their errors are combined with +, as
    // the .NET builder's Zip does. F# nests MergeSources for more than two, combining e1 + (e2 + e3).
    member inline _.MergeSources< ^r, 'a, 'b, ^e when ^e: (static member (+): ^e -> ^e -> ^e)>
        (e1: Effect< ^r, 'a, ^e >, e2: Effect< ^r, 'b, ^e >)
        : Effect< ^r, 'a * 'b, ^e > =
        Effect(
            EffectDelegate(fun r -> task {
                let t1 = e1.Run r
                let t2 = e2.Run r
                let! r1 = t1
                let! r2 = t2

                match r1, r2 with
                | Ok a, Ok b -> return Ok(a, b)
                | Error e, Ok _
                | Ok _, Error e -> return Error e
                | Error err1, Error err2 -> return Error(err1 + err2)
            })
        )

    member _.Recover(eff: Effect<'r, 'a, 'e>, continuation: 'e -> 'a) : Effect<'r, 'a, 'e> =
        mkEff (fun r -> task {
            match! eff.Run r with
            | Ok a -> return Ok a
            | Error e -> return Ok(continuation e)
        })

    member _.TryRecover(eff: Effect<'r, 'a, 'e>, continuation: 'e -> Result<'a, 'e>) : Effect<'r, 'a, 'e> =
        mkEff (fun r -> task {
            match! eff.Run r with
            | Ok a -> return Ok a
            | Error e -> return continuation e
        })

    member _.TryRecover(eff: Effect<'r, 'a, 'e>, continuation: 'e -> Effect<'r, 'a, 'e>) : Effect<'r, 'a, 'e> =
        mkEff (fun r -> task {
            match! eff.Run r with
            | Ok a -> return Ok a
            | Error e -> return! (continuation e).Run r
        })

    member _.ChangeError(eff: Effect<'r, 'a, 'e1>, f: 'e1 -> 'e2) : Effect<'r, 'a, 'e2> =
        mkEff (fun r -> task {
            match! eff.Run r with
            | Ok a -> return Ok a
            | Error e -> return Error(f e)
        })

    // All effects start before any is awaited, and the result is the first error in their order, as the .NET
    // builder's WhenAll does: it waits for all of them, but doesn't observe the exceptions of those after an error.
    member _.WhenAll(effects: Effect<'r, 'a, 'e> seq) : Effect<'r, 'a array, 'e> =
        mkEff (fun r -> task {
            let started = [| for e in effects -> e.Run r |]
            let results = Array.zeroCreate started.Length
            let mutable error = ValueNone
            let mutable i = 0

            while i < started.Length do
                if error.IsNone then
                    match! started[i] with
                    | Ok a -> results[i] <- a
                    | Error e -> error <- ValueSome e
                else
                    try
                        let! _ = started[i]
                        ()
                    with _ ->
                        ()

                i <- i + 1

            match error with
            | ValueSome e -> return Error e
            | ValueNone -> return Ok results
        })

/// <exclude/>
[<AutoOpen>]
module LowPriority =
    // A task without a value, as on .NET: generic over anything awaitable, in an extension, and not an overload for
    // Task. Beside Bind(Task<'a>), a Bind(Task) would take a Task<'a> too, with a continuation of unit rather than 'a,
    // so F# would check the continuation before choosing, with the bound value's type unknown, and a return! of it
    // would be ambiguous. Here the value's type comes from the awaiter, and Bind(Task<'a>) is chosen first. The
    // constraint only types it: in JavaScript a task is a promise, which is awaited as one.
    type EffBuilderBase with

        member inline _.Bind< ^TaskLike, ^Awaiter, 'a, 'r, 'b, 'e
            when ^TaskLike: (member GetAwaiter: unit -> ^Awaiter) and ^Awaiter: (member GetResult: unit -> 'a)>
            (t: ^TaskLike, continuation: 'a -> Effect<'r, 'b, 'e>)
            : Effect<'r, 'b, 'e> =
            Effect(
                EffectDelegate(fun r -> task {
                    let! a = unbox<Task<'a>> (box t)
                    return! (continuation a).Run r
                })
            )

        member inline this.ReturnFrom< ^TaskLike, ^Awaiter, 'a, 'r, 'e
            when ^TaskLike: (member GetAwaiter: unit -> ^Awaiter) and ^Awaiter: (member GetResult: unit -> 'a)>
            (t: ^TaskLike)
            : Effect<'r, 'a, 'e> =
            this.Bind(t, this.Return)

/// <exclude/>
type EffBuilder() =
    inherit EffBuilderBase()

    member _.Run(eff: Effect<'r, 'a, 'e>) : Effect<'r, 'a, 'e> = eff

/// <summary>The <c>eff</c> computation expression, and <c>mkEffect</c>.</summary>
[<AutoOpen>]
module Builder =
    /// <summary>Creates an effect from the function that starts it.</summary>
    let inline mkEffect d = Effect(EffectDelegate d)

    /// <summary>The computation expression for effects.</summary>
    let eff = EffBuilder()

// In this file, as on .NET, so they are intrinsic members of Effect, which SRTP can find, not extensions.
type Effect<'R, 'T, 'E> with
    (* Functor *)
    static member inline Map(h: Effect<'r, 'a, 'e>, f: 'a -> 'b) = eff {
        let! e = h
        return f e
    }

    static member inline Return(a) = eff { return a }

    (* Monad *)
    static member inline (>>=)(h: Effect<'r, 'a, 'e>, f: 'a -> Effect<'r, 'b, 'e>) = eff {
        let! e = h
        return! f e
    }

    static member inline ap<'r, 'a, 'b, 'e>
        (applicative: Effect<'r, 'b -> 'a, 'e>)
        (e: Effect<'r, 'b, 'e>)
        : Effect<'r, 'a, 'e> =
        eff {
            let! fn = applicative
            let! a = e
            return fn a
        }

    (* applicative *)
    // Type variables of its own, as on .NET: in Effect's members, 'r, 'a and 'e are the type's own parameters
    static member inline (<*>)(f: Effect<'env, 'arg -> 'res, _>, e: Effect<'env, 'arg, _>) : Effect<'env, 'res, _> =
        eff {
            let! fn = f
            and! a = e
            return fn a
        }
