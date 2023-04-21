﻿namespace Orsak

open System.Threading
open FSharp.Control
open System.Threading.Tasks
open System.Runtime.InteropServices
open System

module Experiment =
    let inline par s =
        eff.Run(eff.Extra(s))

[<RequireQualifiedAccess>]
module Effect =

    /// <summary>
    /// Starts the effect.
    /// </summary>
    /// <typeparam name="'r" > The environment required to run the effect </typeparam>
    /// <typeparam name="'a" > The resulting type when the effect runs successfully </typeparam>
    /// <typeparam name="'e" > The resulting type when the effect fails</typeparam>
    /// <param name="env">The environment needed to start the effect</param>
    /// <param name="e">The effect to run</param>
    let inline run<'r, 'a, 'e> (env: 'r) (e: Effect<'r, 'a, 'e>) = e.Run env

    let inline runOrFail<'r, 'a, 'e> (env: 'r) (e: Effect<'r, 'a, 'e>) = e.RunOrFail env

    /// <summary>
    /// Creates a new effect from <paramref name="a"/>.
    /// </summary>
    /// <param name="a">The value to wrap in the effect</param>
    /// <typeparam name="'r" > The environment required to run the effect </typeparam>
    /// <typeparam name="'a" > The resulting type when the effect runs successfully </typeparam>
    /// <typeparam name="'e" > The resulting type when the effect fails</typeparam>
    let inline ret<'r, 'a, 'e> (a: 'a) : Effect<'r, 'a, 'e> = eff { return a }

    /// <summary>
    /// Monadic bind of the effect.
    /// </summary>
    /// <param name="f">The continuation function to run in case <paramref name="e"/> succeeds.</param>
    /// <param name="e">The effect</param>
    let inline bind<'r, 'a, 'b, 'e> ([<InlineIfLambda>] f: 'a -> Effect<'r, 'b, 'e>) (e: Effect<'r, 'a, 'e>) = eff {
        let! a = e
        return! f a
    }

    /// <summary>
    /// Monadic join of the effect.
    /// </summary>
    /// <param name="e">The effect</param>
    let inline join e = bind id e

    /// <summary>
    /// map of the effect.
    /// </summary>
    /// <param name="f">The continuation function to run in case <paramref name="e"/> succeeds.</param>
    /// <param name="e">The effect</param>
    let inline map ([<InlineIfLambda>] f: 'a -> 'b) (e: Effect<'r, 'a, 'e>) = eff {
        let! a = e
        return f a
    }

    /// <summary>
    /// map2 of the effect.
    /// </summary>
    let map2 f s1 s2 = bind (fun a -> map (f a) s2) s1

    /// <summary>
    /// Recovers a failed effect by producing an <typeparamref name="'a"/> in case of failure. If the effect was already
    /// successful, <paramref name="f"/> is never run.
    /// </summary>
    /// <param name="f">The function to run in case of failure</param>
    /// <param name="e">The effect</param>
    let inline recover ([<InlineIfLambda>] f: 'e -> 'a) (e: Effect<'r, 'a, 'e>) : Effect<'r, 'a, 'e> =
        eff.Run(eff.Recover(e, f))

    /// <summary>
    /// Potentially recovers a failed effect by producing an <see cref="Result{'a, 'e}"/> in case of failure. If the effect was already
    /// successful, <paramref name="f"/> is never run.
    /// </summary>
    /// <param name="f">The function to run in case of failure</param>
    /// <param name="e">The effect</param>
    let inline tryRecover ([<InlineIfLambda>] f: 'e -> Result<'a, 'e>) (e: Effect<'r, 'a, 'e>) =
        eff.Run(eff.TryRecover(e, f))

    /// <summary>
    /// Potentially recovers a failed effect by producing a new effect in case of failure. If the effect was already
    /// successful, <paramref name="f"/> is never run.
    /// </summary>
    /// <param name="f">The function to run in case of failure</param>
    /// <param name="e">The effect</param>
    let inline onError ([<InlineIfLambda>] f: 'e -> Effect<'r, 'a, 'e>) (e: Effect<'r, 'a, 'e>) =
        eff.Run(eff.TryRecover(e, f))


    let inline retry (e: Effect<'r, 'a, 'e>) = onError (fun _ -> e) e

    let inline retryIf ([<InlineIfLambda>] cond) (e: Effect<'r, 'a, 'e>) =
        onError (fun err -> if cond err then e else eff { return! Error err }) e

    let rec retryWhile cond (e: Effect<'r, 'a, 'e>) =
        onError
            (fun err ->
                if (cond err) then
                    retryWhile cond e
                else
                    eff { return! Error err })
            e

    let inline retryTimes times (e: Effect<'r, 'a, 'e>) =
        let mutable count = times

        retryWhile
            (fun _ ->
                if count = 0 then
                    false
                else
                    count <- count - 1
                    true)
            e

    /// <summary>
    /// Transforms an effect which fails with an error of <typeparamref name="'e1"/> to an effect which fails with
    /// an error of type <typeparamref name="'e2"/>.
    /// </summary>
    /// <param name="f">Function which maps the error, in case the effect had failed</param>
    /// <param name="e">The effect</param>
    let changeError (f: 'e1 -> 'e2) (e: Effect<'r, 'a, 'e1>) : Effect<'r, 'a, 'e2> = eff.Run(eff.ChangeError(e, f))

    /// <summary>
    /// Binds an effect with an effect of a different error type
    /// </summary>
    /// <param name="f">Function which produces an effect that fails with a different error</param>
    /// <param name="e">The effect</param>
    let bindNewError (e: Effect<'r, 'a, 'e1>) (f: 'a -> Effect<'r, 'b, 'e2>) : Effect<_, _, Choice<_, _>> = eff {
        let! e = e |> changeError Choice1Of2
        let! a = e |> f |> changeError Choice2Of2
        return a
    }

    /// <summary>
    /// Runs an effect with the provided environment, resolving the dependencies of the providers in that environment
    /// </summary>
    /// <param name="env">Provider needed to run the effect</param>
    /// <param name="effect"> The effect to resolve </param>
    let inline resolveEnvironment (env: 'env) (effect: Effect<'env, 'a, 'err>) : Effect<'r, 'a, 'err> = eff {
        let! result = effect |> run env
        return! result
    }

    /// <summary>
    /// Zips two effects together, the resulting effect will fail if either of the effects failed. In case both failed,
    /// the error is combined. The effects will execute in parallel if possible.
    /// </summary>
    /// <param name="e1">The first effect.</param>
    /// <param name="e2">The second effect.</param>
    /// <param name="f">Function to combine the result of the effects in case both succeeds.</param>
    let inline zip (e1: Effect<'r, 'a, 'e>) (e2: Effect<'r, 'b, 'e>) ([<InlineIfLambda>] f: struct ('a * 'b) -> 'g) = eff {
        let! a = e1
        and! b = e2
        return f (a, b)
    }

    /// <summary>
    /// Merges an inner result with its surrounding effect.
    /// </summary>
    /// <param name="e">The effect</param>
    let inline joinResult (e: Effect<'r, Result<'a, 'e>, 'e>) : Effect<'r, 'a, 'e> = eff {
        let! a = e
        return! a
    }

    /// <summary>
    /// Merges an outer result with its inner effect.
    /// </summary>
    /// <param name="r">The result containing the effect</param>
    let inline resultJoin (r: Result<Effect<'r, 'a, 'e>, 'e>) : Effect<'r, 'a, 'e> =
        match r with
        | Ok e -> e
        | Error e -> eff { return! Error e }

    /// <summary>
    /// Merges an inner task with its surrounding effect.
    /// </summary>
    /// <param name="e">The effect</param>
    let inline joinTask (e: Effect<'r, Task<'a>, 'e>) : Effect<'r, 'a, 'e> = eff {
        let! t = e
        let! a = t
        return a
    }

    /// <summary>
    /// Merges an outer task with its inner effect.
    /// </summary>
    /// <param name="t">The task containing the effect</param>
    let inline taskJoin (t: Task<Effect<'r, 'a, 'e>>) : Effect<'r, 'a, 'e> = eff {
        let! e = t
        return! e
    }

    ///Executes effects in parallel if possible. Currently not efficiently implemented.
    let inline par (eff: Effect<'r, 'a, 'e> seq) =
        mkEffect (fun rEnv -> vtask {
            let! results =
                eff
                |> Seq.map (run rEnv)
                |> Seq.map (fun (t: AsyncResult<'a, 'e>) -> t.AsTask())
                |> Task.WhenAll

            return
                Ok []
                |> Array.foldBack
                    (fun curr agg ->
                        match agg with
                        | Ok(list: 'a list) ->
                            match curr with
                            | Ok a -> Ok(a :: list)
                            | Error e -> Error e
                        | Error e -> Error e)
                    results
        })

    ///Executes effects in parallel if possible. Currently not efficiently implemented.
    let inline par_ (eff: Effect<'r, unit, 'e> seq) : Effect<'r, unit, 'e> =
        mkEffect (fun rEnv -> vtask {
            let! results =
                eff
                |> Seq.map (run rEnv)
                |> Seq.map (fun (t: AsyncResult<unit, 'e>) -> t.AsTask())
                |> Task.WhenAll

            let a =
                Ok []
                |> Array.foldBack
                    (fun curr agg ->
                        match agg with
                        | Ok(list: _ list) ->
                            match curr with
                            | Ok a -> Ok(a :: list)
                            | Error e -> Error e
                        | Error e -> Error e)
                    results

            match a with
            | Ok _ -> return Ok()
            | Error e -> return Error e
        })


    ///Traverses an array of effects, turning it in to an effect of an array.
    ///For a more generic implementation, consider FSharpPlus
    let inline traverse f (eff: Effect<'r, 'a, 'e> array) =
        mkEffect (fun rEnv -> vtask {
            let mutable final = Ok(Array.zeroCreate eff.Length)

            for i = 0 to (eff.Length - 1) do
                match final with
                | Ok list ->
                    match! run rEnv eff[i] with
                    | Ok a -> list[i] <- f a
                    | Error e -> final <- Error e
                | Error _ -> ()

            return final
        })

    ///Sequences an array of effects, turning it in to an effect of an array.
    ///For a more generic implementation, consider FSharpPlus
    let inline sequence (eff: Effect<'r, 'a, 'e> array) = traverse id eff

    /// <summary>
    /// Configures an effect to fail after a given amount of time, unless it has already succeeded.
    /// The original effect still executes.
    /// </summary>
    /// <param name="ts"></param>
    /// <param name="onTimeout"></param>
    /// <param name="eff"></param>
    let inline timeout (ts: TimeSpan) onTimeout (eff: Effect<_, _, _>) =
        mkEffect (fun rEnv -> vtask {
            try
                return! eff.Run(rEnv).AsTask().WaitAsync(ts)
            with :? TaskCanceledException ->
                return Error onTimeout
        })

    //not cooperative, but could be useful for graceful shutdown maybe
    let withCancellation (ct: CancellationToken) (eff: Effect<_, unit, _>) =
        mkEffect (fun rEnv -> vtask {
            try
                return! eff.Run(rEnv).AsTask().WaitAsync(ct)
            with :? TaskCanceledException ->
                return Ok()
        })

    /// <summary>
    /// Races two effects against each other, returning the result of the winner.
    /// </summary>
    /// <param name="eff1"></param>
    /// <param name="eff2"></param>
    let inline race (eff1: Effect<_, _, _>) (eff2: Effect<_, _, _>) =
        mkEffect (fun rEnv -> vtask {
            let t1 = eff1.Run(rEnv).AsTask()
            let t2 = eff2.Run(rEnv).AsTask()
            let! winner = Task.WhenAny(t1, t2)
            return! winner
        })

type Effect =

    /// <summary>
    /// Creates a failed effect from the provided error.
    /// </summary>
    /// <param name="error">The error the effect will fail with.</param>
    static member Error(error: 'e) : Effect<'a, 'b, 'e> = eff { return! Error error }

    /// <summary>
    /// Creates an effect from a function.
    /// </summary>
    /// <param name="f">The effect creation function</param>
    static member Create(f: 'a -> Task<Result<'b, 'e>>) =
        mkEffect (fun a -> ValueTask<_>(task = f a))

    /// <summary>
    /// Creates an effect from a function.
    /// </summary>
    /// <param name="f">The effect creation function</param>
    static member Create(f: 'a -> ValueTask<Result<'b, 'e>>) = mkEffect f

    /// <summary>
    /// Creates an effect from a function.
    /// </summary>
    /// <param name="f">The effect creation function</param>
    static member Create(f: 'a -> Async<Result<'b, 'e>>) =
        mkEffect (fun a -> ValueTask<_>(task = Async.StartAsTask(f a)))

[<AutoOpen>]
module WrapTwice =
    //extensions for Effect for things weed need to wrap twice. lowest prio
    type Effect with


        /// <summary>
        /// Creates an effect from a function.
        /// </summary>
        static member Create<'a, 'b, 'e>(f: 'a -> 'b) : Effect<'a, 'b, 'e> =
            mkEffect (f >> Ok >> ValueTask.FromResult)

[<AutoOpen>]
module WrapOnce =
    //extensions for Effect for things weed need to wrap once, or which wraps once
    //we want this to have higher prio than wrapTwice, but lower than non-extensions.
    type Effect with

        /// <summary>
        /// Creates an effect from a function.
        /// </summary>
        /// <param name="f">The effect creation function</param>
        static member Create(f: 'a -> Async<'b>) : Effect<'a, 'b, _> =
            mkEffect (fun a -> vtask {
                let! b = f a
                return Ok b
            })

        /// <summary>
        /// Creates an effect from a function returning Result.
        /// </summary>
        /// <param name="f">The effect creation function</param>
        static member Create(f: 'a -> Result<'b, 'e>) : Effect<'a, 'b, 'e> = mkEffect (f >> ValueTask.FromResult)

        /// <summary>
        /// Creates an effect from a function.
        /// </summary>
        /// <example>
        ///   To create reusable effects from Interfaces used to describe the effects, use the following pattern.
        ///   <code>
        /// open Orsak
        /// open System.Threading.Tasks
        /// type MyEffect = abstract member DoEffect: int -> Task&lt;string&gt;
        /// type MyEffectProvider = abstract member Effect: MyEffect
        /// module MyEffect =
        ///     let doEffect x =
        ///         Orsak.Effect.Create(fun (e: #MyEffectProvider) -> e.Effect.DoEffect(x))
        ///   </code>
        /// </example>
        /// <param name="f">The effect creation function</param>
        static member Create(f: 'a -> Task<'b>) : Effect<'a, 'b, _> =
            mkEffect (fun a -> vtask {
                let! b = f a
                return Ok b
            })

        /// <summary>
        /// Creates an effect from a function.
        /// </summary>
        /// <param name="f">The effect creation function</param>
        static member Create(f: 'a -> ValueTask<'b>) : Effect<'a, 'b, _> =
            mkEffect (fun a -> vtask {
                let! b = f a
                return Ok b
            })
