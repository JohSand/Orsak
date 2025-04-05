namespace Orsak.Tests

open System
open System.Threading
open System.Threading.Tasks

open Orsak
open Orsak.Resilience

open FSharp.Control
open Microsoft.Extensions.Time.Testing

open Xunit

type DelayInspectingFakeTimeProvider() =
    inherit FakeTimeProvider()
    member val TotalObservedRequestedDelays = 0. with get, set

    override this.CreateTimer(callback: TimerCallback, state: obj, dueTime: TimeSpan, period: TimeSpan) =
        this.TotalObservedRequestedDelays <- this.TotalObservedRequestedDelays + dueTime.TotalMilliseconds
        base.CreateTimer(callback, state, dueTime, period)

type Runner(fakeTimeProvider: FakeTimeProvider) =
    let mutable key = Unchecked.defaultof<Guid>
    member _.Key = key
    interface Dummy with
        member _.ok() = task { return Ok() }

    interface ITimeProvider with
        member _.Clock = fakeTimeProvider

    interface IRandomProvider with
        member _.Random = DefaultRandom(Random(11_000))

    interface Effect.IStateKeys with
        member _.Key
            with get () = key
            and set value = key <- value

type TimeAdvance(fakeTimeProvider: FakeTimeProvider) =
    let ctx = new CancellationTokenSource()

    let backGround = backgroundTask {
        do! Task.Yield()

        while not ctx.Token.IsCancellationRequested do
            fakeTimeProvider.Advance(TimeSpan.FromSeconds 10.)

    }

    interface IAsyncDisposable with
        member this.DisposeAsync() = vtask {
            ctx.Cancel()
            do! backGround
            ctx.Dispose()
        }

type DelayTests() =
    let cache = Effect.cache

    [<Fact>]
    let ``add delay_ properly adds delay on false`` () = task {
        let provider = DelayInspectingFakeTimeProvider()
        use _t = TimeAdvance(provider)
        let runner = Runner(provider)

        let! e =
            eff {
                do! Dummy.mkDummyEff ()
                return false
            }
            |> Effect.addDelay_ 2.0<s>
            |> Effect.repeatTimes 11
            |> Effect.run runner

        Assert.Equal(Ok(), e)

        match cache.TryGetValue(runner.Key) with
        | true, d -> Assert.Equal(11L, d.Attempt)
        | _, _ -> Assert.Fail("Failed to get state.")

        Assert.Equal(1_835_902., provider.TotalObservedRequestedDelays)

        Assert.Equal(
            TimeSpan(days = 0, hours = 0, minutes = 30, seconds = 35, milliseconds = 902),
            TimeSpan.FromMilliseconds provider.TotalObservedRequestedDelays
        )

        return ()
    }

    [<Fact>]
    let ``add delayWithMax_ properly adds delay on false`` () = task {
        let provider = DelayInspectingFakeTimeProvider()
        use _t = TimeAdvance(provider)
        let runner = Runner(provider)

        let! e =
            eff {
                do! Dummy.mkDummyEff ()
                return false
            }
            |> Effect.addDelayWithMax_ 2.0<s> 2.0<s>
            |> Effect.repeatTimes 11
            |> Effect.run runner

        Assert.Equal(Ok(), e)

        match cache.TryGetValue(runner.Key) with
        | true, d -> Assert.Equal(11L, d.Attempt)
        | _, _ -> Assert.Fail("Failed to get state.")

        Assert.Equal(21615., provider.TotalObservedRequestedDelays)

        Assert.Equal(
            TimeSpan(days = 0, hours = 0, minutes = 0, seconds = 21, milliseconds = 615),
            TimeSpan.FromMilliseconds provider.TotalObservedRequestedDelays
        )

        return ()
    }

    [<Fact>]
    let ``add delay_ properly continues on true`` () = task {
        let provider = DelayInspectingFakeTimeProvider()
        use _t = TimeAdvance(provider)
        let runner = Runner(provider)

        let! e =
            eff {
                do! Dummy.mkDummyEff ()
                return true
            }
            |> Effect.addDelay_ 2.0<s>
            |> Effect.repeatTimes 10
            |> Effect.run runner

        Assert.Equal(Ok(), e)

        match cache.TryGetValue(runner.Key) with
        | true, d -> Assert.Equal(0L, d.Attempt)
        | _, _ -> Assert.Fail("Failed to get state.")

        Assert.Equal(0., provider.TotalObservedRequestedDelays)

        Assert.Equal(TimeSpan.Zero, TimeSpan.FromMilliseconds provider.TotalObservedRequestedDelays)

        return ()
    }

    [<Fact>]
    let ``add addDelayOnError properly adds delay on Error`` () = task {
        let provider = DelayInspectingFakeTimeProvider()
        use _t = TimeAdvance(provider)
        let runner = Runner(provider)

        let! e =
            eff {
                do! Dummy.mkDummyEff ()
                return! Error "Expected Error"
            }
            |> Effect.addDelayOnError 2.0<s>
            |> Effect.retryTimes 10L
            |> Effect.run runner

        Assert.Equal(Result<unit, _>.Error "Expected Error", e)

        match cache.TryGetValue(runner.Key) with
        | true, d -> Assert.Equal(11L, d.Attempt)
        | _, _ -> Assert.Fail("Failed to get state.")

        Assert.Equal(1_835_902., provider.TotalObservedRequestedDelays)

        Assert.Equal(
            TimeSpan(days = 0, hours = 0, minutes = 30, seconds = 35, milliseconds = 902),
            TimeSpan.FromMilliseconds provider.TotalObservedRequestedDelays
        )
    }

    [<Fact>]
    let ``add addDelayOnError properly adds delay on Error with recovery`` () = task {
        let provider = DelayInspectingFakeTimeProvider()
        use _t = TimeAdvance(provider)
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

        Assert.Equal(Ok(), e)
        Assert.Equal(10L, counter)

        match cache.TryGetValue(runner.Key) with
        | true, d -> Assert.Equal(0L, d.Attempt)
        | _, _ -> Assert.Fail("Failed to get state.")

        Assert.Equal(1_060_354., provider.TotalObservedRequestedDelays)

        Assert.Equal(
            TimeSpan(days = 0, hours = 0, minutes = 17, seconds = 40, milliseconds = 354),
            TimeSpan.FromMilliseconds provider.TotalObservedRequestedDelays
        )

        return ()
    }

    [<Fact>]
    let ``add addDelayOnErrorWithMax properly adds delay on Error with recovery`` () = task {
        let provider = DelayInspectingFakeTimeProvider()
        use _t = TimeAdvance(provider)
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

        Assert.Equal(Ok(), e)
        Assert.Equal(10L, counter)

        match cache.TryGetValue(runner.Key) with
        | true, d -> Assert.Equal(0L, d.Attempt)
        | _, _ -> Assert.Fail("Failed to get state.")

        Assert.Equal(19_615., provider.TotalObservedRequestedDelays)

        return ()
    }

    [<Fact>]
    let ``retryForever retries at least 10_000 times`` () = task {
        let provider = DelayInspectingFakeTimeProvider()
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

        Assert.Equal(Ok(), e)
    }
