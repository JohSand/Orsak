module LimitedParallelTests

open System
open System.Threading
open System.Threading.Tasks
open FSharp.Control
open Orsak
open Xunit
open Swensen.Unquote

/// Effects that record how many run at once, and which have started.
type Tracker() =
    let mutable running = 0
    let mutable mostRunning = 0
    let started = System.Collections.Concurrent.ConcurrentQueue<int>()

    member _.MostRunning = mostRunning
    member _.Started = started |> Seq.sort |> List.ofSeq

    /// An effect that takes a little while, and then returns `result`.
    member _.Effect(index: int, result: Result<int, string>) : Effect<unit, int, string> =
        mkEffect (fun () -> vtask {
            started.Enqueue index
            let now = Interlocked.Increment &running

            lock started (fun () -> mostRunning <- max mostRunning now)
            do! Task.Delay 20
            Interlocked.Decrement &running |> ignore
            return result
        })

let run (e: Effect<unit, 'a, string>) = e |> Effect.run ()

[<Fact(Timeout = 10_000)>]
let parLimited_returns_the_results_in_the_order_the_effects_were_given () = task {
    let tracker = Tracker()
    let! result = [ for i in 1..10 -> tracker.Effect(i, Ok i) ] |> Effect.parLimited 3 |> run
    result =! Ok [ 1..10 ]
}

[<Fact(Timeout = 10_000)>]
let parLimited_runs_at_most_the_limit_at_a_time () = task {
    let tracker = Tracker()
    let! result = [ for i in 1..20 -> tracker.Effect(i, Ok i) ] |> Effect.parLimited 3 |> run
    test <@ Result.isOk result @>
    tracker.MostRunning =! 3
}

[<Fact(Timeout = 10_000)>]
let parLimited_with_a_limit_of_one_runs_the_effects_one_at_a_time () = task {
    let tracker = Tracker()
    let! _ = [ for i in 1..5 -> tracker.Effect(i, Ok i) ] |> Effect.parLimited 1 |> run
    tracker.MostRunning =! 1
}

[<Fact(Timeout = 10_000)>]
let parLimited_with_a_limit_above_the_number_of_effects_runs_them_all_at_once () = task {
    let tracker = Tracker()
    let! result = [ for i in 1..4 -> tracker.Effect(i, Ok i) ] |> Effect.parLimited 100 |> run
    result =! Ok [ 1..4 ]
    tracker.MostRunning =! 4
}

[<Fact(Timeout = 10_000)>]
let parLimited_of_no_effects_succeeds_with_an_empty_list () = task {
    let! result = ([]: Effect<unit, int, string> list) |> Effect.parLimited 3 |> run
    result =! Ok []
}

[<Fact(Timeout = 10_000)>]
let parLimited_fails_with_the_first_error_in_the_order_the_effects_were_given () = task {
    let tracker = Tracker()

    let effects = [
        tracker.Effect(1, Ok 1)
        tracker.Effect(2, Error "second")
        tracker.Effect(3, Error "third")
    ]

    // all three run at once, so the third may fail before the second
    let! result = effects |> Effect.parLimited 3 |> run
    result =! Error "second"
}

[<Fact(Timeout = 10_000)>]
let parLimited_starts_no_more_effects_once_one_has_failed () = task {
    let tracker = Tracker()
    let effects = [ for i in 1..10 -> tracker.Effect(i, (if i = 3 then Error "third" else Ok i)) ]
    let! result = effects |> Effect.parLimited 1 |> run
    result =! Error "third"
    tracker.Started =! [ 1; 2; 3 ]
}

[<Fact(Timeout = 10_000)>]
let parLimited_rethrows_an_exception_and_starts_no_more_effects () = task {
    let tracker = Tracker()

    let throwing: Effect<unit, int, string> = eff {
        failwith "thrown"
        return 0
    }

    let effects = [ tracker.Effect(1, Ok 1); throwing; tracker.Effect(3, Ok 3) ]
    // two at a time: the other worker, still running the first effect, must not go on to the third
    let! thrown = Assert.ThrowsAnyAsync<exn>(fun () -> (effects |> Effect.parLimited 2 |> run).AsTask() :> Task)
    thrown.Message =! "thrown"
    tracker.Started =! [ 1 ]
}

[<Fact>]
let parLimited_rejects_a_limit_below_one () =
    Assert.Throws<ArgumentOutOfRangeException>(fun () ->
        ([]: Effect<unit, int, string> list) |> Effect.parLimited 0 |> ignore)
    |> ignore

[<Fact(Timeout = 10_000)>]
let mapParLimited_applies_the_function_to_each_effect_and_runs_at_most_the_limit_at_a_time () = task {
    let tracker = Tracker()

    let! result =
        Effect.mapParLimited 2 [ for i in 1..6 -> tracker.Effect(i, Ok i) ] (Effect.map (fun i -> i * 10))
        |> run

    result =! Ok [ 10..10..60 ]
    tracker.MostRunning =! 2
}

[<Fact(Timeout = 10_000)>]
let parLimited_starts_the_next_effect_as_soon_as_fewer_than_the_limit_are_running () = task {
    let releaseFirst = TaskCompletionSource()
    let thirdStarted = TaskCompletionSource()

    let first = mkEffect (fun () -> vtask {
        do! releaseFirst.Task
        return Ok 1
    })

    let second = mkEffect (fun () -> vtask {
        do! Task.Delay 10
        return Ok 2
    })

    let third = mkEffect (fun () -> vtask {
        thirdStarted.SetResult()
        return Ok 3
    })

    let running = ([ first; second; third ] |> Effect.parLimited 2 |> run).AsTask()

    // the third starts when the second completes, while the first is still running
    do! thirdStarted.Task.WaitAsync(TimeSpan.FromSeconds 5.)
    test <@ not running.IsCompleted @>

    releaseFirst.SetResult()
    let! result = running
    result =! Ok [ 1; 2; 3 ]
}
