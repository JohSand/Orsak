namespace Orsak

open System
open System.Threading
open System.Threading.Tasks
open FSharp.Control
open Orsak.Scoped

#if NET8_0_OR_GREATER
/// <summary>
/// A scope of cancellation: a <see cref="T:System.Threading.CancellationTokenSource"/> that is cancelled when the token
/// it is linked to is, and can be cancelled on its own without cancelling that token. Begun by
/// <c>Cancellation.within</c> and <c>Cancellation.timeout</c>, and disposed when their effect ends, so each source
/// lives exactly as long as the effects that use it.
/// </summary>
/// <remarks>
/// It is the <see cref="T:Orsak.ICancellationProvider"/> of the effects in the scope, so an environment grows in it by
/// providing it as its source.
/// </remarks>
[<Sealed>]
type CancellationScope private (parent: CancellationToken) =
    inherit CancellationTokenSource()

    [<DefaultValue>]
    val mutable private registration: CancellationTokenRegistration

    /// <summary>The token the scope is linked to.</summary>
    member _.Parent = parent

    /// <summary>
    /// Begins a scope linked to <paramref name="token"/>, already cancelled if the token is. Whoever begins it
    /// disposes it.
    /// </summary>
    /// <param name="token">The token whose cancellation cancels the scope.</param>
    static member LinkedTo(token: CancellationToken) =
        let scope = new CancellationScope(token)
        // as CreateLinkedTokenSource does; runs at once if the token is already cancelled
        scope.registration <- token.UnsafeRegister((fun s -> (s :?> CancellationScope).Cancel()), scope)
        scope

    /// <summary>
    /// Begins a scope nested in <paramref name="source"/>: the <c>BeginScope</c> of an environment whose
    /// <see cref="T:Orsak.ICancellationProvider"/> is that source, such as one grown in a scope, so that a scope begun
    /// in a scope is cancelled with it. Myriad's <c>[&lt;GenEnvironment&gt;]</c> implements it so.
    /// </summary>
    /// <param name="source">The source whose cancellation cancels the scope.</param>
    static member NestedIn(source: CancellationTokenSource) =
        ValueTask<CancellationScope>(CancellationScope.LinkedTo source.Token)

    /// <summary>Unlinks the scope from its parent token, then disposes the source.</summary>
    override this.Dispose(disposing: bool) =
        if disposing then
            // waits for a cancellation running on another thread, so the source is not disposed while it cancels
            this.registration.Dispose()

        base.Dispose(disposing)

    interface ICancellationProvider with
        member this.Source = this

    interface IAsyncDisposable with
        member this.DisposeAsync() =
            this.Dispose()
            ValueTask.CompletedTask

type CancellationScopeBuilder() = inherit ScopeCreatingEffectBuilder<CancellationScope>()

[<AutoOpen>]
module CancellationScopeBuilder =
    let cancellableEff = CancellationScopeBuilder()

/// <summary>
/// Creates <see cref="T:Orsak.ICancellationProvider"/>s, for an environment that doesn't begin cancellation scopes,
/// such as one in a test. To link a source to a token, use <c>CancellationScope.LinkedTo</c>, which is a provider
/// too; to cancel after a timeout, <c>Cancellation.timeout</c>.
/// </summary>
module CancellationProvider =
    /// <summary>
    /// A provider of <paramref name="source"/>, which every read of <c>Source</c> returns. The caller keeps owning the
    /// source, and disposes it.
    /// </summary>
    /// <param name="source">The source to provide.</param>
    let fromSource (source: CancellationTokenSource) = {
        new ICancellationProvider with
            member _.Source = source
    }

    /// <summary>
    /// A provider of a new source, linked to no token: a <see cref="T:Orsak.CancellationScope"/>, which is its own
    /// source. Dispose it when done with it.
    /// </summary>
    /// <example>
    /// <code lang="fsharp">
    /// use cancellation = CancellationProvider.create ()
    /// </code>
    /// </example>
    let create () =
        CancellationScope.LinkedTo CancellationToken.None

/// <summary>
/// Creates the <see cref="T:Orsak.Scoped.ScopeProvider`1"/>s that begin <see cref="T:Orsak.CancellationScope"/>s.
/// </summary>
module CancellationScope =
    /// <summary>
    /// Begins cancellation scopes linked to <paramref name="token"/>, such as a request's <c>RequestAborted</c> or a
    /// service's stopping token: for the root environment, which then needs no source of its own.
    /// </summary>
    /// <param name="token">The token the scopes are linked to.</param>
    let linkedTo (token: CancellationToken) = {
        new ScopeProvider<CancellationScope> with
            member _.BeginScope() =
                ValueTask<_>(CancellationScope.LinkedTo token)
    }

/// <summary>
/// Effects that run other effects in a <see cref="T:Orsak.CancellationScope"/>. The environment begins the scope and
/// grows into the environment of the effect, which provides the scope as its
/// <see cref="T:Orsak.ICancellationProvider"/>: so <c>CancellationSource.getToken</c>, <c>Time.delay</c> and the
/// retry delays of <c>Orsak.Resilience</c> see the scope's token, and <c>CancellationSource.cancel</c> cancels only
/// the scope.
/// </summary>
module Cancellation =
    /// <summary>
    /// Runs <paramref name="body"/> in a new cancellation scope, disposed when <paramref name="body"/> ends. In an
    /// environment that is in a scope already, the new scope is nested in it, when the environment begins it so, with
    /// <c>CancellationScope.NestedIn</c>.
    /// </summary>
    /// <param name="body">The effect to run in the scope, an effect of the grown environment.</param>
    /// <example>
    /// <code lang="fsharp">
    /// let importAll files =
    ///     eff {
    ///         for file in files do
    ///             do! import file
    ///     }
    ///     |> Cancellation.within
    /// </code>
    /// </example>
    let within<'Outer, 'Inner, 'a, 'e when 'Outer :> IGrowsInto<CancellationScope, 'Inner>>
        (body: Effect<'Inner, 'a, 'e>)
        : Effect<'Outer, 'a, 'e> =
        mkEffect (fun (outer: 'Outer) -> vtask {
            use! scope = outer.BeginScope()

            match! body.Run(outer.Grow scope) with
            | Ok a -> return Ok a
            | Error e -> return Error e
        })

    /// <summary>
    /// Runs <paramref name="body"/> in a new cancellation scope that is cancelled after <paramref name="duration"/>,
    /// and fails with <paramref name="onTimeout"/> if it is. Unlike <c>Effect.timeout</c>, the effect is stopped: the
    /// effects in it that take the scope's token end when it is cancelled. The time is measured on the clock of an
    /// environment that is an <see cref="T:Orsak.ITimeProvider"/>, so a fake clock decides when it times out, and on
    /// the system clock otherwise.
    /// </summary>
    /// <remarks>
    /// Cancelling the token the scope is linked to, or cancelling the scope from within, is not a timeout: the
    /// cancellation propagates as an exception, as without the timeout.
    /// </remarks>
    /// <param name="duration">How long the effect may run.</param>
    /// <param name="onTimeout">The error to fail with when it times out.</param>
    /// <param name="body">The effect to run in the scope, an effect of the grown environment.</param>
    /// <example>
    /// <code lang="fsharp">
    /// lookupAddress postcode
    /// |> Cancellation.timeout (TimeSpan.FromSeconds 2.0) AddressLookupTimedOut
    /// </code>
    /// </example>
    let timeout<'Outer, 'Inner, 'a, 'e when 'Outer :> IGrowsInto<CancellationScope, 'Inner>>
        (duration: TimeSpan)
        (onTimeout: 'e)
        (body: Effect<'Inner, 'a, 'e>)
        : Effect<'Outer, 'a, 'e> =
        mkEffect (fun (outer: 'Outer) -> vtask {
            use! scope = outer.BeginScope()

            let clock =
                match box outer with
                | :? ITimeProvider as p -> p.Clock
                | _ -> TimeProvider.System

            let timedOut = ref false

            use _timer =
                clock.CreateTimer(
                    (fun _ ->
                        timedOut.Value <- true
                        scope.Cancel()),
                    (null: obj),
                    duration,
                    Timeout.InfiniteTimeSpan
                )

            try
                match! body.Run(outer.Grow scope) with
                | Ok a -> return Ok a
                // an error the effect made of its cancellation is the timeout too
                | Error _ when timedOut.Value -> return Error onTimeout
                | Error e -> return Error e
            with :? OperationCanceledException when timedOut.Value ->
                return Error onTimeout
        })
#endif
