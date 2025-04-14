namespace Orsak.Resilience
open System
open System.Threading.Tasks
open FSharp.Control
open Orsak

type Never = internal Never of Never

[<AutoOpen>]
module BottomType =
    /// <summary>
    /// Active pattern for an effect that should run as long as the application is running.
    /// </summary>
    /// <param name="_result"></param>
    let (|Forever|) (_result: Result<Never, Never>) = Forever

#nowarn "3511"

type NonReturningEffectBuilder() =
    inherit EffBuilderBase()

    member _.Run code =
        eff.Run code |> Effect.map (fun _ -> Unchecked.defaultof<Never>)

[<AutoOpen>]
module Builder =
    /// <summary>
    /// An effect CE that can be used to signal that the effect is not expected to complete successfully through normal means, but instead run forever.
    /// An example would be an effect running a while true do -> loop. It does not indicate that the effect cannot fail, and thus it can return in case of an error.
    /// </summary>
    let forever = NonReturningEffectBuilder()

module Delay =
    open Microsoft.FSharp.Linq

    [<Literal>]
    let ExponentialFactor = 2.0

    [<Literal>]
    let Pfactor = 4.0

    [<Literal>]
    let RpScalingFactor = 1. / 1.4

    let calculateDelayWithJitter
        (attempt: int64)
        (prev: byref<float>)
        (baseDelay: TimeSpan)
        (randomizer: IRandomGenerator)
        =
        //cap attempt for purpose of calc, thus preventing next from overflowing, and
        //infinite values of randomPart
        let attempt = min attempt 1000

        let exponent = float attempt + randomizer.NextDouble()
        let next = ExponentialFactor ** exponent * Math.Tanh(Math.Sqrt(Pfactor * exponent))
        let randomPart = next - prev
        let staticPart = float baseDelay.Ticks
        let delayTicks = randomPart * RpScalingFactor * staticPart

        prev <- next

        if delayTicks > 42949672940000. then
            TimeSpan.FromTicks(42949672940000L)
        elif delayTicks < 0 then
            TimeSpan.FromTicks(42949672940000L)
        else
            TimeSpan.FromTicks(int64 delayTicks)

    let getDelay
        (attempt: int64)
        (prev: byref<float>)
        (baseDelay: TimeSpan)
        (maxDelay: Nullable<TimeSpan>)
        (randomizer: IRandomGenerator)
        =
        try
            let delay = calculateDelayWithJitter attempt &prev baseDelay randomizer

            if delay >? maxDelay then maxDelay.Value else delay

        with :? OverflowException ->
            if maxDelay.HasValue then
                maxDelay.Value
            else
                //delay's System. TimeSpan. TotalMilliseconds property is greater than 4294967294.
                TimeSpan.FromMilliseconds(4294967294.)

open System.Threading
open Microsoft.Extensions.Logging

type EffectDelayState
    (
        p: TimeProvider,
        baseDelay: TimeSpan,
        maxDelay: Nullable<TimeSpan>,
        randomizer: IRandomGenerator,
        ?cancellationToken: CancellationToken
    ) =
    let mutable attempt = 0L
    let mutable prevDelay = 0.

    let cancellationToken =
        cancellationToken |> Option.defaultValue CancellationToken.None

    member _.Attempt = attempt

    //the effect completed and did something, return immediately
    member this.Reset() =
        attempt <- 0L
        prevDelay <- 0

    member this.Delay() =
        //the effect completed but did not do any work
        //we can delay before we return, so we don't immediately run again.
        let delay = Delay.getDelay attempt &prevDelay baseDelay maxDelay randomizer

        attempt <- attempt + 1L

        Task.Delay(delay, p, cancellationToken)

[<Measure>]
type s = FSharp.Data.UnitSystems.SI.UnitSymbols.s

module Effect =
    open System.Collections.Concurrent

    let internal cache = ConcurrentDictionary<Guid, EffectDelayState>()

    type internal IStateKeys =
        abstract member Key: Guid with get, set

    let internal getOrCreate (key: Guid) (f: Guid -> Effect<_, EffectDelayState, 'Err>) = eff {
        let mutable result = Unchecked.defaultof<EffectDelayState>

        if (not (cache.TryGetValue(key, &result))) then
            let! result' = f key
            result <- cache.GetOrAdd(key, result')

        return result
    }

    let internal createDelayState (baseDelay: TimeSpan) maxDelay (key: Guid) =
        mkEffect (fun provider ->
            let randomizer =
                match box provider with
                | :? IRandomProvider as p -> p.Effect
                | _ -> DefaultRandom(Random.Shared) :> IRandomGenerator

            let p =
                match box provider with
                | :? ITimeProvider as p -> p.Effect
                | _ -> TimeProvider.System

            let token =
                match box provider with
                | :? ICancellationProvider as p -> p.Effect.Token
                | _ -> CancellationToken.None
            match box provider with
            | :? IStateKeys as s -> s.Key <- key
            | _ -> ()

            ValueTask.FromResult(Ok(EffectDelayState(p, baseDelay, maxDelay, randomizer, token))))

    /// <summary>
    /// Takes a base delay and an effect, and returns an effect with a delay added at the end, if the provided effect returned false.
    /// If the effect is re-run, the delay is increased each time the original effect returns false. If the effect returns true,
    /// no delay is added, and the calculated delay is reset to 0.
    /// </summary>
    /// <param name="baseDelay"></param>
    /// <param name="effect"></param>
    let addDelay (baseDelay: float<s>) (effect: Effect<_, bool, _>) =
        let key = Guid.NewGuid()

        eff {
            let! state = getOrCreate key (createDelayState (TimeSpan.FromSeconds(float baseDelay)) (Nullable<_>()))

            match! effect with
            | true ->
                do state.Reset()
                return true
            | false ->
                do! state.Delay()
                return false
        }

    /// <summary>
    /// Takes a base delay and an effect, and returns an effect with a delay added at the end, if the provided effect returned false.
    /// If the effect is re-run, the delay is increased each time the original effect returns false. If the effect returns true,
    /// no delay is added, and the calculated delay is reset to 0.
    /// Then the bool is discarded and unit is returned.
    /// </summary>
    /// <param name="baseDelay"></param>
    /// <param name="effect"></param>
    let addDelay_ (baseDelay: float<s>) (effect: Effect<_, bool, _>) =
        let key = Guid.NewGuid()

        eff {
            let! state = getOrCreate key (createDelayState (TimeSpan.FromSeconds(float baseDelay)) (Nullable<_>()))

            match! effect with
            | true -> do state.Reset()
            | false -> do! state.Delay()
        }

    /// <summary>
    /// Takes a base delay and en effect, and returns an effect with a delay added at the end, if the provided effect returned false.
    /// If the effect is re-run, the delay is increased each time the original effect returns false. If the effect returns true,
    /// no delay is added, and the calculated delay is reset to 0. The delay till will not be longer than <paramref name="maxDelay"/>
    /// </summary>
    /// <param name="baseDelay"></param>
    /// <param name="maxDelay"></param>
    /// <param name="effect"></param>
    let addDelayWithMax (baseDelay: float<s>) (maxDelay: float<s>) (effect: Effect<_, bool, _>) =
        let key = Guid.NewGuid()

        eff {
            let! state =
                getOrCreate
                    key
                    (createDelayState
                        (TimeSpan.FromSeconds(float baseDelay))
                        (Nullable<_>(TimeSpan.FromSeconds(float maxDelay))))

            match! effect with
            | true ->
                do state.Reset()
                return true
            | false ->
                do! state.Delay()
                return false
        }

    /// <summary>
    /// Takes a base delay and an effect, and returns an effect with a delay added at the end, if the provided effect returned false.
    /// If the effect is re-run, the delay is increased each time the original effect returns false. If the effect returns true,
    /// no delay is added, and the calculated delay is reset to 0. The delay till will not be longer than <paramref name="maxDelay"/>
    /// Then the bool is discarded and unit is returned.
    /// </summary>
    /// <param name="baseDelay"></param>
    /// <param name="maxDelay"></param>
    /// <param name="effect"></param>
    let addDelayWithMax_ (baseDelay: float<s>) (maxDelay: float<s>) (effect: Effect<_, bool, _>) =
        let key = Guid.NewGuid()

        eff {
            let! state =
                getOrCreate
                    key
                    (createDelayState
                        (TimeSpan.FromSeconds(float baseDelay))
                        (Nullable<_>(TimeSpan.FromSeconds(float maxDelay))))

            match! effect with
            | true -> do state.Reset()
            | false -> do! state.Delay()
        }

    /// <summary>
    /// Takes a base delay and an effect, and returns an effect with a delay added at the end, if the provided effect returned with an error.
    /// If the effect is re-run, the delay is increased each time the original effect returns an error. If the effect returns Ok,
    /// no delay is added, and the calculated delay is reset to 0.
    /// </summary>
    /// <param name="baseDelay"></param>
    /// <param name="effect"></param>
    let addDelayOnError (baseDelay: float<s>) (effect: Effect<_, 'a, 'e>) =
        let key = Guid.NewGuid()

        eff {
            let! state = getOrCreate key (createDelayState (TimeSpan.FromSeconds(float baseDelay)) (Nullable<_>()))

            match! effect |> Effect.asResult with
            | Ok a ->
                do state.Reset()
                return! Ok a
            | Error err ->
                do! state.Delay()
                return! Error err
        }

    /// <summary>
    /// Takes a base delay and an effect, and returns an effect with a delay added at the end, if the provided effect returned with an error.
    /// If the effect is re-run, the delay is increased each time the original effect returns an error. If the effect returns Ok,
    /// no delay is added, and the calculated delay is reset to 0.
    /// </summary>
    /// <param name="baseDelay"></param>
    /// <param name="maxDelay"></param>
    /// <param name="effect"></param>
    let addDelayOnErrorWithMax (baseDelay: float<s>) (maxDelay: float<s>) (effect: Effect<_, 'a, 'e>) =
        let key = Guid.NewGuid()

        eff {
            let! state =
                getOrCreate
                    key
                    (createDelayState
                        (TimeSpan.FromSeconds(float baseDelay))
                        (Nullable<_>(TimeSpan.FromSeconds(float maxDelay))))

            match! effect |> Effect.asResult with
            | Ok a ->
                do state.Reset()
                return! Ok a
            | Error err ->
                do! state.Delay()
                return! Error err
        }

    let logError (log: ILoggerFactory -> 'e -> unit) (effect: Effect<'r, 'a, 'e>) =
        effect
        |> Effect.onError (fun e -> eff {
            let! fact = Effect.Create(fun (provider: #ILoggerFactory) -> provider :> ILoggerFactory)
            do log fact e
            return! Error e
        })

    /// <summary>
    /// Perpetually returns the provided effect, in case of error
    /// </summary>
    /// <param name="effect"></param>
    let retryForever (effect: Effect<_, 'a, _>) =
        eff {
            let mutable result = Unchecked.defaultof<'a>
            let mutable cont = true

            while cont do
                match! effect |> Effect.asResult with
                | Error _ ->
                    System.Diagnostics.Debug.Print("")
                    ()
                | Ok a ->
                    result <- a
                    cont <- false

            return result
        }
        |> Effect.changeError (fun _ -> Never(Unchecked.defaultof<_>))

    /// <summary>
    /// If the provided effect returns an error, tries to re-run it up to <paramref name="times"/> times.
    /// </summary>
    /// <param name="times"></param>
    /// <param name="effect"></param>
    let retryTimes times (effect: Effect<_, 'a, _>) = eff {
        let mutable result = Unchecked.defaultof<Result<'a, _>>
        let mutable cont = true
        let mutable tries = 0L

        while cont do
            match! effect |> Effect.asResult with
            | Error _ when tries < times -> tries <- tries + 1L
            | Error e ->
                result <- Error e
                cont <- false
            | Ok a ->
                result <- Ok a
                cont <- false

        return! result
    }

    /// <summary>
    /// Re-runs the provided effect until the provided token signals cancellation. If the provided effect fails, it returns with an error.
    /// </summary>
    /// <param name="token"></param>
    /// <param name="effect"></param>
    let repeatUntilCancellation token (effect: Effect<_, unit, _>) =
        Effect.repeatUntilCancellation token effect
        |> Effect.map (fun _ -> Never(Unchecked.defaultof<_>))

module private Examples =

    let adapt (token: CancellationToken) =
        Effect.logError (fun _ _ -> ())
        >> Effect.addDelay_ 1.<s>
        >> Effect.addDelayOnError 1.<s>
        >> Effect.retryForever
        >> Effect.repeatUntil (fun () -> token.IsCancellationRequested)


    let test (token: CancellationToken) (effect: Effect<_, bool, string>) = task {
        try
            match!
                effect
                |> Effect.logError (fun _ _ -> ())
                |> Effect.addDelay_ 1.<s>
                |> Effect.addDelayOnError 1.<s>
                |> Effect.retryForever
                |> Effect.repeatUntilCancellation token
                |> Effect.run Unchecked.defaultof<_>
            with
            | Forever -> ()
        with _exn ->
            ()

        return ()
    }
