namespace Orsak.Tests

open System
open System.Linq
open System.Threading
open System.Threading.Tasks

open Orsak
open Orsak.Resilience

open FSharp.Control
open Microsoft.Extensions.Time.Testing

open Xunit
open Swensen.Unquote

type internal DelayKey = Orsak.Resilience.Effect.DelayKey

/// A fake clock that records every delay requested of it, and completes each one by advancing its time by exactly
/// that delay, right after the timer is created. Fake time then equals the total time waited, so tests can compare
/// timestamps between attempts, and nothing waits in real time.
type SteppingTimeProvider() =
    inherit FakeTimeProvider()
    let delays = System.Collections.Concurrent.ConcurrentQueue<TimeSpan>()

    /// The delays requested, in order.
    member _.Delays = List.ofSeq delays

    member _.TotalObservedRequestedDelays = delays |> Seq.sumBy _.TotalMilliseconds

    override this.CreateTimer(callback: TimerCallback, state: obj, dueTime: TimeSpan, period: TimeSpan) =
        let timer = base.CreateTimer(callback, state, dueTime, period)

        if dueTime > TimeSpan.Zero && dueTime <> Timeout.InfiniteTimeSpan then
            delays.Enqueue dueTime
            // after CreateTimer has returned, so the timer is registered when the time passes
            ThreadPool.QueueUserWorkItem(fun _ -> this.Advance dueTime) |> ignore

        timer

type Runner(fakeTimeProvider: FakeTimeProvider) =
    let mutable key = Unchecked.defaultof<DelayKey>
    member internal _.Key = key

    interface Dummy with
        member _.ok() = task { return Ok() }

    interface ITimeProvider with
        member _.Clock = fakeTimeProvider

    interface IRandomProvider with
        member _.Effect = DefaultRandom(Random(11_000))

    interface Effect.IStateKeys with
        member _.Key
            with get () = key
            and set value = key <- value

type DelayTests() =
    let cache = Effect.cache

    [<Fact(Timeout = 10_000)>]
    let add_delay__adds_delay_on_false () = task {
        let provider = SteppingTimeProvider()
        let runner = Runner(provider)

        let! e =
            eff {
                do! Dummy.mkDummyEff ()
                return false
            }
            |> Effect.addDelay_ 2.0<s>
            |> Effect.repeatTimes 11
            |> Effect.run runner

        Ok() =! e

        match cache.TryGetValue(runner.Key) with
        | true, d -> 11L =! d.Attempt
        | _, _ -> Assert.Fail("Failed to get state.")

        1_835_902. =! provider.TotalObservedRequestedDelays
        test <@ provider.Delays.Length = 11 @>
        test <@ provider.Delays |> List.pairwise |> List.forall (fun (a, b) -> a < b) @>
        // the clock advances by exactly the delays taken
        test <@ provider.GetUtcNow() - provider.Start = TimeSpan.FromMilliseconds provider.TotalObservedRequestedDelays @>

        TimeSpan(days = 0, hours = 0, minutes = 30, seconds = 35, milliseconds = 902)
        =! TimeSpan.FromMilliseconds provider.TotalObservedRequestedDelays

        return ()
    }

    [<Fact(Timeout = 30_000)>]
    let delay_is_cleaned_up_once_the_effect_is_gone () = task {
        do! add_delay__adds_delay_on_false ()
        // continue on another stack: the effect may have completed on this one, whose frames keep it reachable
        do! Task.Yield()

        let deadline = DateTime.UtcNow.AddSeconds 10.

        while cache.Count() > 0 && DateTime.UtcNow < deadline do
            GC.Collect()
            GC.WaitForPendingFinalizers()
            GC.WaitForFullGCComplete() |> ignore
            GC.Collect()

        test <@ cache.Count() = 0 @>
    }

    [<Fact(Timeout = 10_000)>]
    let add_delayWithMax__adds_delay_on_false () = task {
        let provider = SteppingTimeProvider()
        let runner = Runner(provider)

        let! e =
            eff {
                do! Dummy.mkDummyEff ()
                return false
            }
            |> Effect.addDelayWithMax_ 2.0<s> 2.0<s>
            |> Effect.repeatTimes 11
            |> Effect.run runner

        Ok() =! e

        match cache.TryGetValue(runner.Key) with
        | true, d -> 11L =! d.Attempt
        | _, _ -> Assert.Fail("Failed to get state.")

        21615. =! provider.TotalObservedRequestedDelays
        test <@ provider.Delays.Length = 11 @>
        test <@ provider.Delays |> List.forall (fun d -> d <= TimeSpan.FromSeconds 2.) @>

        TimeSpan(days = 0, hours = 0, minutes = 0, seconds = 21, milliseconds = 615)
        =! TimeSpan.FromMilliseconds provider.TotalObservedRequestedDelays


        return ()
    }

    [<Fact(Timeout = 10_000)>]
    let add_delay__continues_on_true () = task {
        let provider = SteppingTimeProvider()
        let runner = Runner(provider)

        let! e =
            eff {
                do! Dummy.mkDummyEff ()
                return true
            }
            |> Effect.addDelay_ 2.0<s>
            |> Effect.repeatTimes 10
            |> Effect.run runner

        Ok() =! e

        match cache.TryGetValue(runner.Key) with
        | true, d -> 0L =! d.Attempt
        | _, _ -> Assert.Fail("Failed to get state.")

        0. =! provider.TotalObservedRequestedDelays
        test <@ provider.Delays.IsEmpty @>
        TimeSpan.Zero =! TimeSpan.FromMilliseconds provider.TotalObservedRequestedDelays

        return ()
    }

    [<Fact(Timeout = 10_000)>]
    let addDelayOnError_adds_delay_on_error () = task {
        let provider = SteppingTimeProvider()
        let runner = Runner(provider)

        let! e =
            eff {
                do! Dummy.mkDummyEff ()
                return! Error "Expected Error"
            }
            |> Effect.addDelayOnError 2.0<s>
            |> Effect.retryTimes 10L
            |> Effect.run runner

        Result<unit, _>.Error("Expected Error") =! e

        match cache.TryGetValue(runner.Key) with
        | true, d -> 11L =! d.Attempt
        | _, _ -> Assert.Fail("Failed to get state.")

        1_835_902. =! provider.TotalObservedRequestedDelays
        test <@ provider.Delays.Length = 11 @>
        test <@ provider.Delays |> List.pairwise |> List.forall (fun (a, b) -> a < b) @>

        TimeSpan(days = 0, hours = 0, minutes = 30, seconds = 35, milliseconds = 902)
        =! TimeSpan.FromMilliseconds provider.TotalObservedRequestedDelays

    }

    [<Fact(Timeout = 10_000)>]
    let addDelayOnError_adds_delay_on_error_with_recovery () = task {
        let provider = SteppingTimeProvider()
        let runner = Runner(provider)
        let mutable counter = 0

        let! e =
            eff {
                do! Dummy.mkDummyEff ()

                if counter < 10 then
                    counter <- counter + 1
                    return! Error "Expected Error"
                else
                    return ()
            }
            |> Effect.addDelayOnError 2.0<s>
            |> Effect.retryForever
            |> Effect.run runner

        Ok() =! e
        10L =! counter

        match cache.TryGetValue(runner.Key) with
        | true, d -> 0L =! d.Attempt
        | _, _ -> Assert.Fail("Failed to get state.")

        1_060_354. =! provider.TotalObservedRequestedDelays
        test <@ provider.Delays.Length = 10 @>

        TimeSpan(days = 0, hours = 0, minutes = 17, seconds = 40, milliseconds = 354)
        =! TimeSpan.FromMilliseconds provider.TotalObservedRequestedDelays

        return ()
    }

    [<Fact(Timeout = 10_000)>]
    let addDelayOnErrorWithMax_adds_delay_on_error_with_recovery () = task {
        let provider = SteppingTimeProvider()
        let runner = Runner(provider)
        let mutable counter = 0

        let! e =
            eff {
                do! Dummy.mkDummyEff ()

                if counter < 10 then
                    counter <- counter + 1
                    return! Error "Expected Error"
                else
                    return ()
            }
            |> Effect.addDelayOnErrorWithMax 2.0<s> 2.0<s>
            |> Effect.retryForever
            |> Effect.run runner

        Ok() =! e
        10L =! counter

        match cache.TryGetValue(runner.Key) with
        | true, d -> 0L =! d.Attempt
        | _, _ -> Assert.Fail("Failed to get state.")

        19_615. =! provider.TotalObservedRequestedDelays
        test <@ provider.Delays.Length = 10 @>
        test <@ provider.Delays |> List.forall (fun d -> d <= TimeSpan.FromSeconds 2.) @>

        return ()
    }

    [<Fact(Timeout = 10_000)>]
    let retryForever_retries_at_least_10_000_times () = task {
        let provider = SteppingTimeProvider()
        let runner = Runner(provider)
        let mutable counter = 0L

        let! e =
            eff {
                do! Dummy.mkDummyEff ()

                if counter < 10_000L then
                    counter <- counter + 1L
                    return! Error "Expected Error"
                else
                    return ()
            }
            |> Effect.retryForever
            |> Effect.run runner

        Ok() =! e
    }

/// An environment whose logger factory records what Effect.logError is given.
type LoggingEnv() =
    let logged = System.Collections.Concurrent.ConcurrentQueue<obj * string>()
    member _.Logged = List.ofSeq logged
    member this.Log (factory: Microsoft.Extensions.Logging.ILoggerFactory) (error: string) = logged.Enqueue(box factory, error)

    interface Microsoft.Extensions.Logging.ILoggerFactory with
        member _.CreateLogger(_) = Microsoft.Extensions.Logging.Abstractions.NullLogger.Instance
        member _.AddProvider(_) = ()
        member _.Dispose() = ()

/// An environment with a clock that only moves when told to, and a cancellation source for the delays.
type CancellableEnv(clock: FakeTimeProvider, source: CancellationTokenSource) =
    interface ITimeProvider with
        member _.Clock = clock

    interface IRandomProvider with
        member _.Effect = DefaultRandom(Random(11_000))

    interface ICancellationProvider with
        member _.Source = source

/// The functions of Orsak.Resilience that had no tests.
type ResilienceFunctionTests() =

    /// An effect that returns each of the results in turn.
    let returning (results: bool list) =
        let remaining = System.Collections.Concurrent.ConcurrentQueue results

        eff {
            match remaining.TryDequeue() with
            | true, result -> return result
            | false, _ -> return failwith "no more results"
        }

    [<Fact(Timeout = 10_000)>]
    let addDelay_returns_the_result_and_delays_after_false () = task {
        let provider = SteppingTimeProvider()
        let effect = returning [ false; true; false ] |> Effect.addDelay 2.0<s>

        let! results =
            eff {
                let! a = effect
                let! b = effect
                let! c = effect
                return [ a; b; c ]
            }
            |> Effect.run (Runner provider)

        Ok [ false; true; false ] =! results
        2 =! provider.Delays.Length
    }

    [<Fact(Timeout = 10_000)>]
    let addDelayWithMax_returns_the_result_and_caps_the_delay () = task {
        let provider = SteppingTimeProvider()
        let effect = returning (List.replicate 11 false) |> Effect.addDelayWithMax 2.0<s> 3.0<s>

        let! results =
            eff {
                let results = ResizeArray()

                for _ in 1..11 do
                    let! result = effect
                    results.Add result

                return List.ofSeq results
            }
            |> Effect.run (Runner provider)

        Ok(List.replicate 11 false) =! results
        test <@ provider.Delays.Length = 11 @>
        test <@ provider.Delays |> List.forall (fun d -> d <= TimeSpan.FromSeconds 3.) @>
    }

    [<Fact(Timeout = 10_000)>]
    let addDelayWithMax__resets_the_delay_after_true () = task {
        let provider = SteppingTimeProvider()

        let! result =
            returning [ false; false; false; true; false ]
            |> Effect.addDelayWithMax_ 2.0<s> 60.0<s>
            |> Effect.repeatTimes 5
            |> Effect.run (Runner provider)

        Ok() =! result

        match provider.Delays with
        | [ _; _; third; afterReset ] -> test <@ afterReset < third @>
        | delays -> failwith $"expected 4 delays, got %A{delays}"
    }

    [<Fact(Timeout = 10_000)>]
    let retryTimes_succeeds_when_a_retry_does () = task {
        let mutable runs = 0

        let effect: Effect<unit, int, string> = eff {
            runs <- runs + 1

            if runs < 3 then
                return! Error "not yet"
            else
                return runs
        }

        let! result = effect |> Effect.retryTimes 5L |> Effect.run ()
        Ok 3 =! result
        3 =! runs
    }

    [<Fact(Timeout = 10_000)>]
    let logError_logs_the_error_and_fails_with_it () = task {
        let env = LoggingEnv()
        let effect: Effect<LoggingEnv, unit, string> = eff { return! Error "boom" }

        let! result = effect |> Effect.logError env.Log |> Effect.run env

        Error "boom" =! result
        [ box env, "boom" ] =! env.Logged
    }

    [<Fact(Timeout = 10_000)>]
    let logError_logs_nothing_when_the_effect_succeeds () = task {
        let env = LoggingEnv()
        let effect: Effect<LoggingEnv, int, string> = eff { return 42 }

        let! result = effect |> Effect.logError env.Log |> Effect.run env

        Ok 42 =! result
        test <@ env.Logged.IsEmpty @>
    }

    /// Runs the effect in a loop that never ends by itself.
    let loopForever (step: Effect<unit, unit, string>) =
        forever {
            while true do
                do! step
        }

    [<Fact(Timeout = 10_000)>]
    let forever_fails_with_the_error_that_ends_its_loop () = task {
        let mutable runs = 0

        let step: Effect<unit, unit, string> = eff {
            runs <- runs + 1

            if runs = 3 then
                return! Error "third run failed"
        }

        let! result = loopForever step |> Effect.run ()

        test <@ result = Error "third run failed" @>
        3 =! runs
    }

    [<Fact(Timeout = 10_000)>]
    let repeatUntilCancellation_ends_with_Forever_when_cancelled () = task {
        use source = new CancellationTokenSource()
        let mutable runs = 0

        let step: Effect<unit, unit, string> = eff {
            runs <- runs + 1

            if runs = 3 then
                source.Cancel()
        }

        let! result =
            step
            |> Effect.retryForever
            |> Effect.repeatUntilCancellation source.Token
            |> Effect.run ()

        match result with
        | Forever -> 3 =! runs
    }

    [<Fact(Timeout = 10_000)>]
    let repeatUntilCancellation_fails_with_the_first_error () = task {
        use source = new CancellationTokenSource()
        let mutable runs = 0

        let step: Effect<unit, unit, string> = eff {
            runs <- runs + 1

            if runs = 2 then
                return! Error "second run failed"
        }

        let! result = step |> Effect.repeatUntilCancellation source.Token |> Effect.run ()
        test <@ result = Error "second run failed" @>
        2 =! runs
    }

    [<Fact(Timeout = 10_000)>]
    let a_delay_is_cancelled_through_the_environment () = task {
        // a clock that never moves on its own, so the delay would never end
        let clock = FakeTimeProvider()
        use source = new CancellationTokenSource()
        let failing: Effect<CancellableEnv, unit, string> = eff { return! Error "failed" }

        let running =
            (failing |> Effect.addDelayOnError 10.0<s> |> Effect.run (CancellableEnv(clock, source))).AsTask()

        source.Cancel()

        let! _ =
            Assert.ThrowsAnyAsync<OperationCanceledException>(fun () -> running.WaitAsync(TimeSpan.FromSeconds 10.))

        ()
    }

    [<Fact(Timeout = 10_000)>]
    let delays_use_the_system_clock_and_random_without_providers () = task {
        let clock = Diagnostics.Stopwatch.StartNew()
        let failing: Effect<unit, unit, string> = eff { return! Error "failed" }

        let! result =
            failing
            |> Effect.addDelayOnError 0.001<s>
            |> Effect.retryTimes 3L
            |> Effect.run ()

        Error "failed" =! result
        test <@ clock.Elapsed < TimeSpan.FromSeconds 5. @>
    }

    [<Fact>]
    let getDelay_is_capped_at_maxDelay () =
        let mutable prev = 0.
        let delay = Delay.getDelay 5000L &prev (TimeSpan.FromSeconds 1.) (Nullable(TimeSpan.FromSeconds 10.)) (DefaultRandom(Random 1))
        TimeSpan.FromSeconds 10. =! delay

    [<Fact>]
    let getDelay_without_maxDelay_is_capped_at_about_50_days () =
        let mutable prev = 0.
        let delay = Delay.getDelay 5000L &prev (TimeSpan.FromSeconds 1.) (Nullable()) (DefaultRandom(Random 1))
        TimeSpan.FromTicks 42949672940000L =! delay

    [<Fact>]
    let getDelay_is_never_negative_nor_above_the_cap () =
        let mutable prev = 0.
        let random = DefaultRandom(Random 1)
        let cap = TimeSpan.FromTicks 42949672940000L

        for attempt in 0L .. 200L do
            let delay = Delay.getDelay attempt &prev (TimeSpan.FromSeconds 1.) (Nullable()) random
            test <@ delay >= TimeSpan.Zero && delay <= cap @>
