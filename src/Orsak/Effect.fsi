namespace Orsak

open System.Threading
open FSharp.Control
open System.Threading.Tasks
open System
open System.Collections.Generic
open Microsoft.FSharp.Core

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
    val inline run: env: 'r -> e: Effect<'r, 'a, 'e> -> AsyncResult<'a, 'e>
    val inline runOrFail: env: 'r -> e: Effect<'r, 'a, 'e> -> Task<'a>
    /// <summary>
    /// Creates a new effect from <paramref name="a"/>.
    /// </summary>
    /// <param name="a">The value to wrap in the effect</param>
    /// <typeparam name="'r" > The environment required to run the effect </typeparam>
    /// <typeparam name="'a" > The resulting type when the effect runs successfully </typeparam>
    /// <typeparam name="'e" > The resulting type when the effect fails</typeparam>
    val inline ret: a: 'a -> Effect<'r, 'a, 'e>

    /// <summary>
    /// Monadic bind of the effect.
    /// </summary>
    /// <param name="f">The continuation function to run in case <paramref name="e"/> succeeds.</param>
    /// <param name="e">The effect</param>
    val inline bind<'r, 'a, 'b, 'e> :
        [<InlineIfLambda>] f: ('a -> Effect<'r, 'b, 'e>) -> e: Effect<'r, 'a, 'e> -> Effect<'r, 'b, 'e>

    /// <summary>
    /// Monadic join of the effect.
    /// </summary>
    /// <param name="e">The effect</param>
    val inline join: e: Effect<'a, Effect<'a, 'b, 'c>, 'c> -> Effect<'a, 'b, 'c>
    /// <summary>
    /// map of the effect.
    /// </summary>
    /// <param name="f">The continuation function to run in case <paramref name="e"/> succeeds.</param>
    /// <param name="e">The effect</param>
    val inline map: [<InlineIfLambda>] f: ('a -> 'b) -> e: Effect<'r, 'a, 'e> -> Effect<'r, 'b, 'e>
    /// <summary>
    /// map2 of the effect.
    /// </summary>
    val map2: f: ('a -> 'b -> 'c) -> s1: Effect<'d, 'a, 'e> -> s2: Effect<'d, 'b, 'e> -> Effect<'d, 'c, 'e>
    /// <summary>
    /// Recovers a failed effect by producing an <typeparamref name="'a"/> in case of failure. If the effect was already
    /// successful, <paramref name="f"/> is never run.
    /// </summary>
    /// <param name="f">The function to run in case of failure</param>
    /// <param name="e">The effect</param>
    val inline recover: [<InlineIfLambda>] f: ('e -> 'a) -> e: Effect<'r, 'a, 'e> -> Effect<'r, 'a, 'e>
    /// <summary>
    /// Potentially recovers a failed effect by producing an <see cref="Result{'a, 'e}"/> in case of failure. If the effect was already
    /// successful, <paramref name="f"/> is never run.
    /// </summary>
    /// <param name="f">The function to run in case of failure</param>
    /// <param name="e">The effect</param>
    val inline tryRecover: [<InlineIfLambda>] f: ('e -> Result<'a, 'e>) -> e: Effect<'r, 'a, 'e> -> Effect<'r, 'a, 'e>
    /// <summary>
    /// Potentially recovers a failed effect by producing a new effect in case of failure. If the effect was already
    /// successful, <paramref name="f"/> is never run.
    /// </summary>
    /// <param name="f">The function to run in case of failure</param>
    /// <param name="e">The effect</param>
    val inline onError: [<InlineIfLambda>] f: ('e -> Effect<'r, 'a, 'e>) -> e: Effect<'r, 'a, 'e> -> Effect<'r, 'a, 'e>
    /// <summary>
    /// Allows the effect to be bound as a result, rather than failing the surrounding effect when bound.
    /// This allows for taking recovery action inside the effect, based on the outcome of a sub-effect.
    /// </summary>
    /// <param name="e">The effect</param>
    val inline asResult: e: Effect<'r, 'a, 'e> -> Effect<'r, Result<'a, 'e>, 'e>

    [<TailCall>]
    val forever: e: Effect<'r, 'a, 'e> -> Effect<'r, 'a, 'e>

    val repeatWhileTrue: e: Effect<'r, bool, 'e> -> Effect<'r, unit, 'e>
    val inline retry: e: Effect<'r, 'a, 'e> -> Effect<'r, 'a, 'e>
    val inline retryIf: [<InlineIfLambda>] cond: ('e -> bool) -> e: Effect<'r, 'a, 'e> -> Effect<'r, 'a, 'e>
    val retryWhile: cond: ('e -> bool) -> e: Effect<'r, 'a, 'e> -> Effect<'r, 'a, 'e>
    val inline retryTimes: times: int -> e: Effect<'r, 'a, 'e> -> Effect<'r, 'a, 'e>
    /// <summary>
    /// Transforms an effect which fails with an error of <typeparamref name="'e1"/> to an effect which fails with
    /// an error of type <typeparamref name="'e2"/>.
    /// </summary>
    /// <param name="f">Function which maps the error, in case the effect had failed</param>
    /// <param name="e">The effect</param>
    val changeError: f: ('e1 -> 'e2) -> e: Effect<'r, 'a, 'e1> -> Effect<'r, 'a, 'e2>
    /// <summary>
    /// Binds an effect with an effect of a different error type
    /// </summary>
    /// <param name="f">Function which produces an effect that fails with a different error</param>
    /// <param name="e">The effect</param>
    val bindNewError: e: Effect<'r, 'a, 'e1> -> f: ('a -> Effect<'r, 'b, 'e2>) -> Effect<'r, 'b, Choice<'e1, 'e2>>
    /// <summary>
    /// Runs an effect with the provided environment, resolving the dependencies of the providers in that environment
    /// </summary>
    /// <param name="env">Provider needed to run the effect</param>
    /// <param name="effect"> The effect to resolve </param>
    val inline resolveEnvironment: env: 'env -> effect: Effect<'env, 'a, 'err> -> Effect<'r, 'a, 'err>

    /// <summary>
    /// Zips two effects together, the resulting effect will fail if either of the effects failed. In case both failed,
    /// the error is combined. The effects will execute in parallel if possible.
    /// </summary>
    /// <param name="e1">The first effect.</param>
    /// <param name="e2">The second effect.</param>
    /// <param name="f">Function to combine the result of the effects in case both succeeds.</param>
    val inline zip:
        e1: Effect<'r, 'a, ^e> ->
        e2: Effect<'r, 'b, ^e> ->
        [<InlineIfLambda>] f: (struct ('a * 'b) -> 'g) ->
            Effect<'r, 'g, ^e>
            when ^e: (static member (+): ^e * ^e -> ^e)

    /// <summary>
    /// Merges an inner result with its surrounding effect.
    /// </summary>
    /// <param name="e">The effect</param>
    val inline joinResult: e: Effect<'r, Result<'a, 'e>, 'e> -> Effect<'r, 'a, 'e>
    /// <summary>
    /// Merges an outer result with its inner effect.
    /// </summary>
    /// <param name="r">The result containing the effect</param>
    val inline resultJoin: r: Result<Effect<'r, 'a, 'e>, 'e> -> Effect<'r, 'a, 'e>
    /// <summary>
    /// Merges an inner task with its surrounding effect.
    /// </summary>
    /// <param name="e">The effect</param>
    val inline joinTask: e: Effect<'r, Task<'a>, 'e> -> Effect<'r, 'a, 'e>
    /// <summary>
    /// Merges an outer task with its inner effect.
    /// </summary>
    /// <param name="t">The task containing the effect</param>
    val inline taskJoin: t: Task<Effect<'r, 'a, 'e>> -> Effect<'r, 'a, 'e>
    /// <summary>
    /// Executes effects in parallel if possible.
    /// </summary>
    /// <param name="s">The effects to run in parallel</param>
    val inline whenAll: s: Effect<'r, 'a, 'e> seq -> Effect<'r, 'a array, 'e>
    /// <summary>
    /// Executes effects in parallel if possible.
    /// </summary>
    /// <param name="s">The effects to run in parallel</param>
    val inline par_: s: Effect<'r, unit, 'e> seq -> Effect<'r, unit, 'e>
    val inline par: s: Effect<'r, 'a, 'e> seq -> Effect<'r, 'a list, 'e>
    ///Traverses an array of effects, turning it in to an effect of an array.
    ///For a more generic implementation, consider FSharpPlus
    val inline traverse: f: ('a -> 'b) -> effects: Effect<'r, 'a, 'e> array -> Effect<'r, 'b array, 'e>
    ///Sequences an array of effects, turning it in to an effect of an array.
    ///For a more generic implementation, consider FSharpPlus
    val inline sequence: eff: Effect<'r, 'a, 'e> array -> Effect<'r, 'a array, 'e>
    /// <summary>
    /// Configures an effect to fail after a given amount of time, unless it has already succeeded.
    /// The original effect still executes.
    /// </summary>
    /// <param name="ts"></param>
    /// <param name="onTimeout"></param>
    /// <param name="eff"></param>
    val inline timeout: ts: TimeSpan -> onTimeout: 'a -> eff: Effect<'b, 'c, 'a> -> Effect<'b, 'c, 'a>
    val withCancellation: ct: CancellationToken -> eff: Effect<'a, unit, 'b> -> Effect<'a, unit, 'b>
    /// <summary>
    /// Races two effects against each other, returning the result of the winner.
    /// </summary>
    /// <param name="eff1"></param>
    /// <param name="eff2"></param>
    val inline race: eff1: Effect<'a, 'b, 'c> -> eff2: Effect<'a, 'b, 'c> -> Effect<'a, 'b, 'c>

[<Class>]
type Effect =
    /// <summary>
    /// Creates a failed effect from the provided error.
    /// </summary>
    /// <param name="error">The error the effect will fail with.</param>
    static member Error: error: 'e -> Effect<'a, 'b, 'e>
    /// <summary>
    /// Creates an effect from a function.
    /// </summary>
    /// <param name="f">The effect creation function</param>
    static member Create: f: ('a -> Task<Result<'b, 'e>>) -> Effect<'a, 'b, 'e>
    /// <summary>
    /// Creates an effect from a function.
    /// </summary>
    /// <param name="f">The effect creation function</param>
    static member Create: f: ('a -> ValueTask<Result<'b, 'e>>) -> Effect<'a, 'b, 'e>
    /// <summary>
    /// Creates an effect from a function.
    /// </summary>
    /// <param name="f">The effect creation function</param>
    static member Create: f: ('a -> Async<Result<'b, 'e>>) -> Effect<'a, 'b, 'e>

[<Class>]
type EffSeq =
    static member Create: f: ('r -> IAsyncEnumerable<Result<'a, 'e>>) -> EffSeq<'r, 'a, 'e>

[<AutoOpen>]
module WrapTwice =
    type Effect with
        /// <summary>
        /// Creates an effect from a function.
        /// </summary>
        static member Create: f: ('a -> 'b) -> Effect<'a, 'b, 'e>

    type EffSeq with
        static member Create: f: ('r -> IAsyncEnumerable<'a>) -> EffSeq<'r, 'a, string>

[<AutoOpen>]
module WrapOnce =
    type Effect with
        /// <summary>
        /// Creates an effect from a function.
        /// </summary>
        /// <param name="f">The effect creation function</param>
        static member Create: f: ('a -> Async<'b>) -> Effect<'a, 'b, 'a0>
        /// <summary>
        /// Creates an effect from a function returning Result.
        /// </summary>
        /// <param name="f">The effect creation function</param>
        static member Create: f: ('a -> Result<'b, 'e>) -> Effect<'a, 'b, 'e>
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
        static member Create: f: ('a -> Task<'b>) -> Effect<'a, 'b, 'a0>
        /// <summary>
        /// Creates an effect from a function.
        /// </summary>
        /// <param name="f">The effect creation function</param>
        static member Create: f: ('a -> ValueTask<'b>) -> Effect<'a, 'b, 'a0>
