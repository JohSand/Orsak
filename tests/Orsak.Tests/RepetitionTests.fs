namespace Orsak.Tests

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Diagnostics
open System.Threading
open System.Threading.Tasks
open FSharp.Control
open Orsak
open Xunit
open Swensen.Unquote

/// The core retry and repeat functions of the Effect module that had no tests.
module RepetitionTests =

    /// An effect that fails with each of the errors in turn, and then succeeds with the number of runs.
    let failingWith (errors: string list) =
        let remaining = ConcurrentQueue errors
        let runs = ref 0

        let effect = eff {
            runs.Value <- runs.Value + 1

            match remaining.TryDequeue() with
            | true, error -> return! Error error
            | false, _ -> return runs.Value
        }

        effect, (fun () -> runs.Value)

    [<Fact(Timeout = 10_000)>]
    let ``forever runs the effect again until it succeeds`` () = task {
        let effect, runs = failingWith [ "a"; "b"; "c" ]
        let! result = effect |> Effect.forever |> Effect.run ()
        Ok 4 =! result
        4 =! runs ()
    }

    /// An effect that fails synchronously the given number of times, then succeeds with the stack depth it ran at.
    let failingSynchronously times =
        let mutable remaining = times

        mkEffect (fun () ->
            if remaining > 0 then
                remaining <- remaining - 1
                ValueTask<_>(Error "again")
            else
                ValueTask<_>(Ok(StackTrace(false).FrameCount)))

    /// How much deeper the stack is after 1000 failed runs than after 10, retrying with retry.
    let stackGrowth (retry: Effect<unit, int, string> -> Effect<unit, int, string>) = task {
        let! after10 = failingSynchronously 10 |> retry |> Effect.runOrFail ()
        let! after1000 = failingSynchronously 1000 |> retry |> Effect.runOrFail ()
        return after1000 - after10
    }

    [<Fact(Timeout = 10_000)>]
    let ``forever doesn't grow the stack while the effect keeps failing synchronously`` () = task {
        let! growth = stackGrowth Effect.forever
        Assert.True(growth <= 10, $"The stack grew by {growth} frames over 990 more failed runs")
    }

    [<Fact(Timeout = 10_000)>]
    let ``forever runs a synchronously failing effect a million times`` () = task {
        let! result = failingSynchronously 1_000_000 |> Effect.forever |> Effect.run ()
        Assert.True(Result.isOk result)
    }

    [<Fact(Timeout = 10_000)>]
    let ``retryWhile doesn't grow the stack while the effect keeps failing synchronously`` () = task {
        let! growth = stackGrowth (Effect.retryWhile (fun _ -> true))
        Assert.True(growth <= 10, $"The stack grew by {growth} frames over 990 more failed runs")
    }

    [<Fact(Timeout = 10_000)>]
    let ``retryWhile retries while the error passes the test`` () = task {
        let effect, runs = failingWith [ "transient"; "transient" ]
        let! result = effect |> Effect.retryWhile (fun e -> e = "transient") |> Effect.run ()
        Ok 3 =! result
        3 =! runs ()
    }

    [<Fact(Timeout = 10_000)>]
    let ``retryWhile fails with the first error that does not pass`` () = task {
        let effect, runs = failingWith [ "transient"; "fatal"; "transient" ]
        let! result = effect |> Effect.retryWhile (fun e -> e = "transient") |> Effect.run ()
        Error "fatal" =! result
        2 =! runs ()
    }

    [<Fact(Timeout = 10_000)>]
    let ``race returns the result of the first to finish`` () = task {
        let slow = eff {
            do! Task.Delay(TimeSpan.FromSeconds 5.)
            return "slow"
        }

        let fast: Effect<unit, string, string> = eff { return "fast" }
        let! result = Effect.race slow fast |> Effect.run ()
        Ok "fast" =! result
    }

    [<Fact(Timeout = 10_000)>]
    let ``race fails when the first to finish fails`` () = task {
        let slow = eff {
            do! Task.Delay(TimeSpan.FromSeconds 5.)
            return "slow"
        }

        let failing: Effect<unit, string, string> = eff { return! Error "failed first" }
        let! result = Effect.race slow failing |> Effect.run ()
        Error "failed first" =! result
    }

    [<Fact(Timeout = 10_000)>]
    let ``repeatUntil checks the condition before every run`` () = task {
        let mutable runs = 0
        let effect: Effect<unit, unit, string> = eff { runs <- runs + 1 }
        let! result = effect |> Effect.repeatUntil (fun () -> runs = 3) |> Effect.run ()
        Ok() =! result
        3 =! runs
    }

    [<Fact(Timeout = 10_000)>]
    let ``repeatUntil does not run the effect when the condition holds from the start`` () = task {
        let mutable runs = 0
        let effect: Effect<unit, unit, string> = eff { runs <- runs + 1 }
        let! result = effect |> Effect.repeatUntil (fun () -> true) |> Effect.run ()
        Ok() =! result
        0 =! runs
    }

    [<Fact(Timeout = 10_000)>]
    let ``repeatUntil stops at the first failure`` () = task {
        let mutable runs = 0

        let effect: Effect<unit, unit, string> = eff {
            runs <- runs + 1

            if runs = 2 then
                return! Error "second run failed"
        }

        let! result = effect |> Effect.repeatUntil (fun () -> runs = 10) |> Effect.run ()
        Error "second run failed" =! result
        2 =! runs
    }

    [<Fact(Timeout = 10_000)>]
    let ``repeatUntilCancellation repeats until the token is cancelled`` () = task {
        use source = new CancellationTokenSource()
        let mutable runs = 0

        let effect: Effect<unit, unit, string> = eff {
            runs <- runs + 1

            if runs = 3 then
                source.Cancel()
        }

        let! result = effect |> Effect.repeatUntilCancellation source.Token |> Effect.run ()
        Ok() =! result
        3 =! runs
    }

    [<Fact(Timeout = 10_000)>]
    let ``repeatForever repeats until the effect fails`` () = task {
        let mutable runs = 0

        let effect: Effect<unit, unit, string> = eff {
            runs <- runs + 1

            if runs = 5 then
                return! Error "fifth run failed"
        }

        let! result = effect |> Effect.repeatForever |> Effect.run ()
        Error "fifth run failed" =! result
        5 =! runs
    }

    [<Fact(Timeout = 10_000)>]
    let ``fanOut gives every item to one worker in turn`` () = task {
        let processed = ConcurrentBag<int * int>()
        let mutable workers = 0

        let work (items: IAsyncEnumerable<int>) : Effect<unit, unit, string> =
            let worker = Interlocked.Increment(&workers)

            eff {
                for item in items do
                    processed.Add((worker, item))
            }

        let! result = TaskSeq.ofList [ 1..100 ] |> Effect.fanOut 4 work |> Effect.run ()

        Ok() =! result
        4 =! workers
        // every item exactly once
        [ 1..100 ] =! (processed |> Seq.map snd |> Seq.sort |> List.ofSeq)
        // in turn, so each worker gets a quarter
        test <@ processed |> Seq.countBy fst |> Seq.forall (fun (_, count) -> count = 25) @>
    }

    [<Fact(Timeout = 10_000)>]
    let ``retry does not run a successful effect again`` () = task {
        let effect, runs = failingWith []
        let! result = effect |> Effect.retry |> Effect.run ()
        Ok 1 =! result
        1 =! runs ()
    }

    [<Fact(Timeout = 10_000)>]
    let ``timeout returns the result when the effect is fast enough`` () = task {
        let fast: Effect<unit, int, string> = eff { return 42 }
        let! result = fast |> Effect.timeout (TimeSpan.FromSeconds 5.) "timed out" |> Effect.run ()
        Ok 42 =! result
    }

    [<Fact(Timeout = 10_000)>]
    let ``timeout fails with the effects own error when it fails in time`` () = task {
        let failing: Effect<unit, int, string> = eff { return! Error "own error" }
        let! result = failing |> Effect.timeout (TimeSpan.FromSeconds 5.) "timed out" |> Effect.run ()
        Error "own error" =! result
    }

    [<Fact(Timeout = 10_000)>]
    let ``withCancellation returns when the effect completes first`` () = task {
        use source = new CancellationTokenSource()
        let mutable ran = false
        let effect: Effect<unit, unit, string> = eff { ran <- true }
        let! result = effect |> Effect.withCancellation source.Token |> Effect.run ()
        Ok() =! result
        true =! ran
    }

    [<Fact(Timeout = 10_000)>]
    let ``withCancellation fails with the effects error when it fails first`` () = task {
        use source = new CancellationTokenSource()
        let failing: Effect<unit, unit, string> = eff { return! Error "failed first" }
        let! result = failing |> Effect.withCancellation source.Token |> Effect.run ()
        Error "failed first" =! result
    }

    [<Fact(Timeout = 10_000)>]
    let ``repeatTimes zero does not run the effect`` () = task {
        let mutable runs = 0
        let effect: Effect<unit, unit, string> = eff { runs <- runs + 1 }
        let! result = effect |> Effect.repeatTimes 0 |> Effect.run ()
        Ok() =! result
        0 =! runs
    }

    [<Fact(Timeout = 10_000)>]
    let ``repeatTimes stops at the first failure`` () = task {
        let mutable runs = 0

        let effect: Effect<unit, unit, string> = eff {
            runs <- runs + 1

            if runs = 2 then
                return! Error "second run failed"
        }

        let! result = effect |> Effect.repeatTimes 5 |> Effect.run ()
        Error "second run failed" =! result
        2 =! runs
    }

    [<Fact(Timeout = 10_000)>]
    let ``repeatWhileTrue runs once when the effect returns false`` () = task {
        let mutable runs = 0

        let effect: Effect<unit, bool, string> = eff {
            runs <- runs + 1
            return false
        }

        let! result = effect |> Effect.repeatWhileTrue |> Effect.run ()
        Ok() =! result
        1 =! runs
    }

    [<Fact(Timeout = 10_000)>]
    let ``repeatWhileTrue stops at the first failure`` () = task {
        let mutable runs = 0

        let effect: Effect<unit, bool, string> = eff {
            runs <- runs + 1

            if runs = 3 then
                return! Error "third run failed"
            else
                return true
        }

        let! result = effect |> Effect.repeatWhileTrue |> Effect.run ()
        Error "third run failed" =! result
        3 =! runs
    }

    [<Fact(Timeout = 10_000)>]
    let ``fanOut completes for an empty source`` () = task {
        let mutable items = 0

        let work (source: IAsyncEnumerable<int>) : Effect<unit, unit, string> = eff {
            for _ in source do
                Interlocked.Increment(&items) |> ignore
        }

        let! result = TaskSeq.empty<int> |> Effect.fanOut 3 work |> Effect.run ()
        Ok() =! result
        0 =! items
    }

    [<Fact(Timeout = 10_000)>]
    let ``fanOut fails with the error of a worker`` () = task {
        // which worker gets which items depends on the order the workers start in, so fail on an item instead
        let work (source: IAsyncEnumerable<int>) : Effect<unit, unit, string> = eff {
            for item in source do
                if item = 2 then
                    return! Error $"failed on {item}"
        }

        // far more items than a worker's channel holds (10), which the failed worker never reads
        let! result = TaskSeq.ofList [ 1..1000 ] |> Effect.fanOut 2 work |> Effect.run ()
        Error "failed on 2" =! result
    }

    [<Fact(Timeout = 10_000)>]
    let ``fanOut stops the other workers when one fails`` () = task {
        let mutable processed = 0

        let work (source: IAsyncEnumerable<int>) : Effect<unit, unit, string> = eff {
            for item in source do
                if item = 50 then
                    return! Error "failed on 50"

                Interlocked.Increment(&processed) |> ignore
        }

        // a source without end: only stopping the writer and the other workers ends the effect
        let! result = TaskSeq.initInfinite id |> Effect.fanOut 3 work |> Effect.run ()

        Error "failed on 50" =! result
        // a bounded number: the writer gets at most a channel's capacity (10) plus one ahead of the failed worker, so
        // no more than about 50 + 3 * 11 items are handed out before the workers are stopped
        test <@ processed < 100 @>
    }

    /// A source that produces the given items, and then waits for more, until its enumeration is cancelled.
    let itemsThenWaiting (items: int list) =
        { new IAsyncEnumerable<int> with
            member _.GetAsyncEnumerator(token) =
                let remaining = Queue items
                let mutable current = 0

                { new IAsyncEnumerator<int> with
                    member _.Current = current

                    member _.MoveNextAsync() =
                        if remaining.Count > 0 then
                            current <- remaining.Dequeue()
                            ValueTask<bool>(true)
                        else
                            ValueTask<bool>(task {
                                do! Task.Delay(Timeout.Infinite, token)
                                return false
                            })

                    member _.DisposeAsync() = ValueTask.CompletedTask
                }
        }

    [<Fact(Timeout = 10_000)>]
    let ``fanOut stops waiting for the source when a worker fails`` () = task {
        let work (source: IAsyncEnumerable<int>) : Effect<unit, unit, string> = eff {
            for item in source do
                if item = 2 then
                    return! Error "failed on 2"
        }

        let! result = itemsThenWaiting [ 1; 2; 3 ] |> Effect.fanOut 2 work |> Effect.run ()
        Error "failed on 2" =! result
    }

    [<Fact(Timeout = 10_000)>]
    let ``fanOut discards the items of workers that return early`` () = task {
        let mutable processed = 0

        // takes its first item, and returns without reading the rest
        let work (source: IAsyncEnumerable<int>) : Effect<unit, unit, string> = eff {
            let items = source.GetAsyncEnumerator()
            let! hasItem = items.MoveNextAsync()

            if hasItem then
                Interlocked.Increment(&processed) |> ignore
        }

        let! result = TaskSeq.ofList [ 1..1000 ] |> Effect.fanOut 2 work |> Effect.run ()

        Ok() =! result
        2 =! processed
    }

    [<Fact(Timeout = 10_000)>]
    let ``fanOut rethrows the exception of a worker`` () = task {
        let work (source: IAsyncEnumerable<int>) : Effect<unit, unit, string> = eff {
            for item in source do
                if item = 2 then
                    raise (InvalidOperationException "worker 2 threw")
        }

        let! error =
            Assert.ThrowsAnyAsync<InvalidOperationException>(fun () ->
                TaskSeq.initInfinite id |> Effect.fanOut 2 work |> Effect.run () |> _.AsTask())

        "worker 2 threw" =! error.Message
    }
