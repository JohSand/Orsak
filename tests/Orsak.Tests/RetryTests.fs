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

    [<Fact>]
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

    [<Fact>]
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

    [<Fact>]
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

    [<Fact>]
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

    [<Fact>]
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

    [<Fact>]
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

    [<Fact>]
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

    [<Fact>]
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
