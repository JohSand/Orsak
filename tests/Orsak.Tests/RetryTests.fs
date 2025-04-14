namespace Orsak.Tests

open System
open System.Threading
open System.Threading.Tasks

open Orsak
open Orsak.Resilience

open FSharp.Control
open Microsoft.Extensions.Time.Testing

open Xunit
open Swensen.Unquote
open System.Runtime.InteropServices

type RunnerType =
    inherit IProvide<Dummy>
    inherit IProvide<TimeProvider>
    inherit IProvide<IRandomGenerator>

type DelayInspectingFakeTimeProvider() =
    inherit FakeTimeProvider()
    member val TotalObservedRequestedDelays = 0. with get, set

    override this.CreateTimer(callback: TimerCallback, state: obj, dueTime: TimeSpan, period: TimeSpan) =
        this.TotalObservedRequestedDelays <- this.TotalObservedRequestedDelays + dueTime.TotalMilliseconds
        base.CreateTimer(callback, state, dueTime, period)

type Runner(fakeTimeProvider: TimeProvider) =
    let mutable key = Unchecked.defaultof<Guid>
    member _.Key = key
    interface RunnerType

    interface IProvide<Dummy> with
        member _.Effect = DummyImp()

    interface IProvide<TimeProvider> with
        member _.Effect = fakeTimeProvider

    interface IProvide<IRandomGenerator> with
        member _.Effect = DefaultRandom(Random(11_000))

    interface Effect.IStateKeys with
        member _.Key
            with get () = key
            and set value = key <- value

type EffectContext<'a, 'b, 'c>(a: 'a, b: 'b, c: 'c) =
    member _.A = a
    member _.B = b
    member _.C = c
    member _.Create<'t>(t: 't) = t

type EffectContext<'a, 'b>(a: 'a, b: 'b) =
    member _.A = a
    member _.B = b
    member _.Create<'t>(t: 't) = EffectContext<'a, 'b, 't>(a, b, t)

type EffectContext<'a>(a: 'a) =
    member _.Create<'t>(t: 't) = EffectContext<'a, 't>(a, t)

[<Struct>]
type EffectContext =
    member _.Create<'t>(t: 't) = EffectContext<'t>(t)

type GenContext<'a, 'b, 'c when 'a: (member Create: 'b -> 'c)> = 'a


type RandomGeneratorExtractor =
    //2 + 3 + 4 + 5 + 6 upto 15?
    static member Extract(_: RandomGeneratorExtractor, e: EffectContext<IRandomGenerator, 'b>) = e.A
    static member Extract(_: RandomGeneratorExtractor, e: EffectContext<'a, IRandomGenerator>) = e.B
    //thrice
    static member Extract(_: RandomGeneratorExtractor, e: EffectContext<IRandomGenerator, 'b, 'c>) = e.A
    static member Extract(_: RandomGeneratorExtractor, e: EffectContext<'a, IRandomGenerator, 'c>) = e.B
    static member Extract(_: RandomGeneratorExtractor, e: EffectContext<'a, 'b, IRandomGenerator>) = e.C
    //only generate the clusters, for which the effect is part of a runner of that size.
    //so if the effect is not a part of a runner with 4 effects, we don't need to generate an extractor of size 4.

type DummyExtractor =
    //twice
    static member Extract(_: DummyExtractor, e: EffectContext<Dummy, 'b>) = e.A
    static member Extract(_: DummyExtractor, e: EffectContext<'a, Dummy>) = e.B
    //thrice
    static member Extract(_: DummyExtractor, e: EffectContext<Dummy, 'b, 'c>) = e.A
    static member Extract(_: DummyExtractor, e: EffectContext<'a, Dummy, 'c>) = e.B
    static member Extract(_: DummyExtractor, e: EffectContext<'a, 'b, Dummy>) = e.C

type TimeProviderExtractor =
    //twice
    static member Extract(_: TimeProviderExtractor, e: EffectContext<TimeProvider, 'b>) = e.A
    static member Extract(_: TimeProviderExtractor, e: EffectContext<'a, TimeProvider>) = e.B
    //thrice
    static member Extract(_: TimeProviderExtractor, e: EffectContext<TimeProvider, 'b, 'c>) = e.A
    static member Extract(_: TimeProviderExtractor, e: EffectContext<'a, TimeProvider, 'c>) = e.B
    static member Extract(_: TimeProviderExtractor, e: EffectContext<'a, 'b, TimeProvider>) = e.C


[<AutoOpen>]
module Raz =
    //just the one
    let inline extract a b =
        ((^a or ^b): (static member Extract: ^a * ^b -> 'c) (a, b))

    //one per registered effect
    let inline (|ExtractRandomGenerator|) (a) : IRandomGenerator =
        extract (Unchecked.defaultof<RandomGeneratorExtractor>) a

    let inline (|ExtractDummy|) (a) : Dummy =
        extract (Unchecked.defaultof<DummyExtractor>) a

    let inline (|ExtractTimeProvider|) (a) : TimeProvider =
        extract (Unchecked.defaultof<TimeProviderExtractor>) a


type Builder() =
    member _.Yield(_: unit) = EffectContext()

    //one per registered effect
    [<CustomOperation("fromEffect")>]
    member inline _.FromEffect(c: GenContext<_, _, _>, p: Dummy) = c.Create(p)

    [<CustomOperation("fromEffect")>]
    member inline _.FromEffect(c: GenContext<_, _, _>, p: TimeProvider) = c.Create(p)

    [<CustomOperation("fromEffect")>]
    member inline _.FromEffect(c: GenContext<_, _, _>, p: IRandomGenerator) = c.Create(p)

    //one per desired runner.
    member inline _.Run(a) =
        match a with
        | ExtractRandomGenerator(a) & ExtractDummy(c) & ExtractTimeProvider(t) -> (a, c, t)

    //Ensure we have exactly one runner, for these arguments.

    //can be used to create specific error message.
    member _.Run(timeProvider: 'a, [<Optional; DefaultParameterValue(0)>]a: int) = timeProvider




module Tst =
    let bu = Builder()

    let asd123 (f: FakeTimeProvider) = bu {
        fromEffect (DummyImp())
        fromEffect (DefaultRandom(Random(11_000)))
        //fromEffect (f)
    }

//let asd124 (f: FakeTimeProvider) = bu {
//        fromEffect (f)
//        fromEffect (DefaultRandom(Random(11_000)))
//        //fromEffect (DummyImp())
//    }


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

//type DelayTests() =
//    let cache = Effect.cache

//    [<Fact>]
//    let ``add delay_ properly adds delay on false`` () = task {
//        let provider = DelayInspectingFakeTimeProvider()
//        use _t = TimeAdvance(provider)
//        let runner = Tst.asd(provider)

//        let! e =
//            eff {
//                do! Dummy.mkDummyEff ()
//                return false
//            }
//            |> Effect.addDelay_ 2.0<s>
//            |> Effect.repeatTimes 11
//            |> Effect.run runner

//        Ok() =! e

//        match cache.TryGetValue(runner.Key) with
//        | true, d -> 11L =! d.Attempt
//        | _, _ -> Assert.Fail("Failed to get state.")

//        1_835_902. =! provider.TotalObservedRequestedDelays

//        TimeSpan(days = 0, hours = 0, minutes = 30, seconds = 35, milliseconds = 902)
//        =! TimeSpan.FromMilliseconds provider.TotalObservedRequestedDelays

//        return ()
//    }

//    [<Fact>]
//    let ``add delayWithMax_ properly adds delay on false`` () = task {
//        let provider = DelayInspectingFakeTimeProvider()
//        use _t = TimeAdvance(provider)
//        let runner = Runner(provider)

//        let! e =
//            eff {
//                do! Dummy.mkDummyEff ()
//                return false
//            }
//            |> Effect.addDelayWithMax_ 2.0<s> 2.0<s>
//            |> Effect.repeatTimes 11
//            |> Effect.run runner

//        Ok() =! e

//        match cache.TryGetValue(runner.Key) with
//        | true, d -> 11L =! d.Attempt
//        | _, _ -> Assert.Fail("Failed to get state.")

//        21615. =! provider.TotalObservedRequestedDelays

//        TimeSpan(days = 0, hours = 0, minutes = 0, seconds = 21, milliseconds = 615)
//        =! TimeSpan.FromMilliseconds provider.TotalObservedRequestedDelays


//        return ()
//    }

//    [<Fact>]
//    let ``add delay_ properly continues on true`` () = task {
//        let provider = DelayInspectingFakeTimeProvider()
//        use _t = TimeAdvance(provider)
//        let runner = Runner(provider)

//        let! e =
//            eff {
//                do! Dummy.mkDummyEff ()
//                return true
//            }
//            |> Effect.addDelay_ 2.0<s>
//            |> Effect.repeatTimes 10
//            |> Effect.run runner

//        Ok() =! e

//        match cache.TryGetValue(runner.Key) with
//        | true, d -> 0L =! d.Attempt
//        | _, _ -> Assert.Fail("Failed to get state.")

//        0. =! provider.TotalObservedRequestedDelays
//        TimeSpan.Zero =! TimeSpan.FromMilliseconds provider.TotalObservedRequestedDelays

//        return ()
//    }

//    [<Fact>]
//    let ``add addDelayOnError properly adds delay on Error`` () = task {
//        let provider = DelayInspectingFakeTimeProvider()
//        use _t = TimeAdvance(provider)
//        let runner = Runner(provider)

//        let! e =
//            eff {
//                do! Dummy.mkDummyEff ()
//                return! Error "Expected Error"
//            }
//            |> Effect.addDelayOnError 2.0<s>
//            |> Effect.retryTimes 10L
//            |> Effect.run runner

//        Result<unit, _>.Error("Expected Error") =! e

//        match cache.TryGetValue(runner.Key) with
//        | true, d -> 11L =! d.Attempt
//        | _, _ -> Assert.Fail("Failed to get state.")

//        1_835_902. =! provider.TotalObservedRequestedDelays

//        TimeSpan(days = 0, hours = 0, minutes = 30, seconds = 35, milliseconds = 902)
//        =! TimeSpan.FromMilliseconds provider.TotalObservedRequestedDelays

//    }

//    [<Fact>]
//    let ``add addDelayOnError properly adds delay on Error with recovery`` () = task {
//        let provider = DelayInspectingFakeTimeProvider()
//        use _t = TimeAdvance(provider)
//        let runner = Runner(provider)
//        let mutable counter = 0

//        let! e =
//            eff {
//                do! Dummy.mkDummyEff ()

//                if counter < 10 then
//                    counter <- counter + 1
//                    return! Error "Expected Error"
//                else
//                    return ()
//            }
//            |> Effect.addDelayOnError 2.0<s>
//            |> Effect.retryForever
//            |> Effect.run runner

//        Ok() =! e
//        10L =! counter

//        match cache.TryGetValue(runner.Key) with
//        | true, d -> 0L =! d.Attempt
//        | _, _ -> Assert.Fail("Failed to get state.")

//        1_060_354. =! provider.TotalObservedRequestedDelays

//        TimeSpan(days = 0, hours = 0, minutes = 17, seconds = 40, milliseconds = 354)
//        =! TimeSpan.FromMilliseconds provider.TotalObservedRequestedDelays

//        return ()
//    }

//    [<Fact>]
//    let ``add addDelayOnErrorWithMax properly adds delay on Error with recovery`` () = task {
//        let provider = DelayInspectingFakeTimeProvider()
//        use _t = TimeAdvance(provider)
//        let runner = Runner(provider)
//        let mutable counter = 0

//        let! e =
//            eff {
//                do! Dummy.mkDummyEff ()

//                if counter < 10 then
//                    counter <- counter + 1
//                    return! Error "Expected Error"
//                else
//                    return ()
//            }
//            |> Effect.addDelayOnErrorWithMax 2.0<s> 2.0<s>
//            |> Effect.retryForever
//            |> Effect.run runner

//        Ok() =! e
//        10L =! counter

//        match cache.TryGetValue(runner.Key) with
//        | true, d -> 0L =! d.Attempt
//        | _, _ -> Assert.Fail("Failed to get state.")

//        19_615. =! provider.TotalObservedRequestedDelays

//        return ()
//    }

//    [<Fact>]
//    let ``retryForever retries at least 10_000 times`` () = task {
//        let provider = DelayInspectingFakeTimeProvider()
//        let runner = Runner(provider)
//        let mutable counter = 0L

//        let! e =
//            eff {
//                do! Dummy.mkDummyEff ()

//                if counter < 10_000L then
//                    counter <- counter + 1L
//                    return! Error "Expected Error"
//                else
//                    return ()
//            }
//            |> Effect.retryForever
//            |> Effect.run runner

//        Ok() =! e
//    }
