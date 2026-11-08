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

    member _.Bind(t: Task, continuation: unit -> Effect<'r, 'b, 'e>) : Effect<'r, 'b, 'e> =
        mkEff (fun r -> task {
            do! t
            return! (continuation ()).Run r
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

    member this.ReturnFrom(t: Task) : Effect<'r, unit, 'e> = this.Bind(t, this.Return)

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
