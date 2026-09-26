namespace Orsak.Tests

open System
open System.Collections.Concurrent
open System.Collections.Generic
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
    let forever_runs_the_effect_again_until_it_succeeds () = task {
        let effect, runs = failingWith [ "a"; "b"; "c" ]
        let! result = effect |> Effect.forever |> Effect.run ()
        Ok 4 =! result
        4 =! runs ()
    }

    [<Fact(Timeout = 10_000)>]
    let retryWhile_retries_while_the_error_passes_the_test () = task {
        let effect, runs = failingWith [ "transient"; "transient" ]
        let! result = effect |> Effect.retryWhile (fun e -> e = "transient") |> Effect.run ()
        Ok 3 =! result
        3 =! runs ()
    }

    [<Fact(Timeout = 10_000)>]
    let retryWhile_fails_with_the_first_error_that_does_not_pass () = task {
        let effect, runs = failingWith [ "transient"; "fatal"; "transient" ]
        let! result = effect |> Effect.retryWhile (fun e -> e = "transient") |> Effect.run ()
        Error "fatal" =! result
        2 =! runs ()
    }

    [<Fact(Timeout = 10_000)>]
    let race_returns_the_result_of_the_first_to_finish () = task {
        let slow = eff {
            do! Task.Delay(TimeSpan.FromSeconds 5.)
            return "slow"
        }

        let fast: Effect<unit, string, string> = eff { return "fast" }
        let! result = Effect.race slow fast |> Effect.run ()
        Ok "fast" =! result
    }

    [<Fact(Timeout = 10_000)>]
    let race_fails_when_the_first_to_finish_fails () = task {
        let slow = eff {
            do! Task.Delay(TimeSpan.FromSeconds 5.)
            return "slow"
        }

        let failing: Effect<unit, string, string> = eff { return! Error "failed first" }
        let! result = Effect.race slow failing |> Effect.run ()
        Error "failed first" =! result
    }

    [<Fact(Timeout = 10_000)>]
    let repeatUntil_checks_the_condition_before_every_run () = task {
        let mutable runs = 0
        let effect: Effect<unit, unit, string> = eff { runs <- runs + 1 }
        let! result = effect |> Effect.repeatUntil (fun () -> runs = 3) |> Effect.run ()
        Ok() =! result
        3 =! runs
    }

    [<Fact(Timeout = 10_000)>]
    let repeatUntil_does_not_run_the_effect_when_the_condition_holds_from_the_start () = task {
        let mutable runs = 0
        let effect: Effect<unit, unit, string> = eff { runs <- runs + 1 }
        let! result = effect |> Effect.repeatUntil (fun () -> true) |> Effect.run ()
        Ok() =! result
        0 =! runs
    }

    [<Fact(Timeout = 10_000)>]
    let repeatUntil_stops_at_the_first_failure () = task {
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
    let repeatUntilCancellation_repeats_until_the_token_is_cancelled () = task {
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
    let repeatForever_repeats_until_the_effect_fails () = task {
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
    let fanOut_gives_every_item_to_one_worker_in_turn () = task {
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
