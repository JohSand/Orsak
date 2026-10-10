module ScalingEffectJobTests

open System
open System.Collections.Concurrent
open System.Diagnostics
open System.Threading
open System.Threading.Tasks
open FSharp.Control
open Microsoft.Extensions.Logging
open Orsak
open Orsak.AspNetCore
open Xunit
open Swensen.Unquote

/// The environment of one worker, which remembers the token that stops it.
type WorkerEnv(token: CancellationToken) =
    member _.Token = token

/// Records the messages logged.
type RecordingLogger() =
    let messages = ConcurrentQueue<string>()
    member _.Messages = List.ofSeq messages

    interface ILogger with
        member _.BeginScope _ = null
        member _.IsEnabled _ = true

        member _.Log(_level, _eventId, state, exn, formatter) =
            messages.Enqueue(formatter.Invoke(state, exn))

/// A job with short intervals, whose workload comes from `workload`, given the number of the check.
type TestJob(logger, work, workload: int -> WorkerLoad, ?restartDelay, ?maxRestartDelay) =
    inherit
        ScalingEffectJob<WorkerEnv, unit, string>(
            "test job",
            logger,
            (),
            work,
            (fun token -> WorkerEnv token),
            maxExtraWorkers = 2,
            monitorInterval = TimeSpan.FromMilliseconds 20.,
            restartDelay = defaultArg restartDelay (TimeSpan.FromMilliseconds 10.),
            maxRestartDelay = defaultArg maxRestartDelay (TimeSpan.FromMilliseconds 40.)
        )

    let mutable checks = 0

    override _.GetWorkload _ =
        checks <- checks + 1
        Task.FromResult(workload checks)

/// A job whose workers record their environments.
type Workers() =
    let environments = ConcurrentQueue<WorkerEnv>()
    let mutable runs = 0

    member _.Environments = List.ofSeq environments
    member _.Runs = runs

    /// The work of a worker: records its environment, and then each run of `run`.
    member _.Work(run: int -> Result<unit, string>) =
        fun () ->
            mkEffect (fun (env: WorkerEnv) -> vtask {
                if not (environments |> Seq.exists (fun e -> obj.ReferenceEquals(e, env))) then
                    environments.Enqueue env

                let n = Interlocked.Increment &runs
                do! Task.Delay(1, env.Token)
                return run n
            })

let succeed _ = Ok()

let eventually (condition: unit -> bool) = task {
    let clock = Stopwatch.StartNew()

    while not (condition ()) do
        if clock.Elapsed > TimeSpan.FromSeconds 5. then
            failwith "The condition was not met within 5 seconds"

        do! Task.Delay 5
}

let start (job: TestJob) = task { do! job.StartAsync CancellationToken.None }
let stop (job: TestJob) = task { do! job.StopAsync CancellationToken.None }

[<Fact(Timeout = 10_000)>]
let ``a worker runs its effect again and again until stopped`` () = task {
    let workers = Workers()
    use job = new TestJob(RecordingLogger(), workers.Work succeed, fun _ -> Sustained)
    do! start job
    do! eventually (fun () -> workers.Runs >= 3)
    do! stop job
    let runs = workers.Runs
    do! Task.Delay 50
    workers.Runs =! runs
}

[<Fact(Timeout = 10_000)>]
let ``a worker logs an exception and runs its effect again`` () = task {
    let workers = Workers()
    let logger = RecordingLogger()
    let work = workers.Work(fun n -> if n <= 2 then failwith $"run {n} threw" else Ok())
    use job = new TestJob(logger, work, fun _ -> Sustained)
    do! start job
    do! eventually (fun () -> workers.Runs >= 4)
    do! stop job

    test
        <@
            logger.Messages
            |> List.filter (fun m -> m.Contains "failed with an exception, and will run again")
            |> List.length = 2
        @>
}

[<Fact(Timeout = 10_000)>]
let ``a worker logs an error and runs its effect again`` () = task {
    let workers = Workers()
    let logger = RecordingLogger()
    let work = workers.Work(fun n -> if n = 1 then Error "out of stock" else Ok())
    use job = new TestJob(logger, work, fun _ -> Sustained)
    do! start job
    do! eventually (fun () -> workers.Runs >= 3)
    do! stop job
    test <@ logger.Messages |> List.exists (fun m -> m.Contains "test job failed with out of stock") @>
}

[<Fact(Timeout = 10_000)>]
let ``a failing worker waits before running its effect again`` () = task {
    let workers = Workers()
    let work = workers.Work(fun _ -> Error "failing")

    use job =
        new TestJob(
            RecordingLogger(),
            work,
            (fun _ -> Sustained),
            restartDelay = TimeSpan.FromMilliseconds 100.,
            maxRestartDelay = TimeSpan.FromMilliseconds 100.
        )

    do! start job
    do! Task.Delay 300
    do! stop job
    // at least 100 ms after each failure: without the wait it would have run hundreds of times
    test <@ workers.Runs >= 1 && workers.Runs <= 5 @>
}

[<Fact(Timeout = 10_000)>]
let ``a failing worker waits twice as long after each failure in a row`` () = task {
    let workers = Workers()
    let work = workers.Work(fun _ -> Error "failing")

    use job =
        new TestJob(
            RecordingLogger(),
            work,
            (fun _ -> Sustained),
            restartDelay = TimeSpan.FromMilliseconds 20.,
            maxRestartDelay = TimeSpan.FromSeconds 10.
        )

    do! start job
    do! Task.Delay 400
    do! stop job
    // waits of 20, 40, 80, 160 ms: about 5 runs in 400 ms, where a fixed 20 ms would give about 20
    test <@ workers.Runs >= 1 && workers.Runs <= 8 @>
}

[<Fact(Timeout = 10_000)>]
let ``a failing worker waits no longer than the max restart delay`` () = task {
    let workers = Workers()
    let work = workers.Work(fun _ -> Error "failing")

    use job =
        new TestJob(
            RecordingLogger(),
            work,
            (fun _ -> Sustained),
            restartDelay = TimeSpan.FromMilliseconds 10.,
            maxRestartDelay = TimeSpan.FromMilliseconds 10.
        )

    do! start job
    // waits of at most 10 ms: 12 runs take a fraction of a second, where doubling without the max would take
    // 10 ms * 2^11, about 20 seconds, well past the 5 seconds eventually waits. Waiting for the runs, rather than
    // counting them in a fixed time, keeps a slow machine from failing the test.
    do! eventually (fun () -> workers.Runs >= 12)
    do! stop job
}


[<Fact(Timeout = 10_000)>]
let ``the job adds a worker when its workload increases up to the maximum`` () = task {
    let workers = Workers()
    use job = new TestJob(RecordingLogger(), workers.Work succeed, fun _ -> Increased)
    do! start job
    do! eventually (fun () -> workers.Environments.Length = 3)
    // more checks, but no more workers: the baseline and 2 extra
    do! Task.Delay 100
    do! stop job
    workers.Environments.Length =! 3
}

[<Fact(Timeout = 10_000)>]
let ``the job stops an extra worker when its workload is low`` () = task {
    let workers = Workers()

    // more work until every worker has run, so no extra worker is stopped before it has recorded its environment
    use job =
        new TestJob(
            RecordingLogger(),
            workers.Work succeed,
            fun _ -> if workers.Environments.Length < 3 then Increased else Low
        )

    do! start job
    do! eventually (fun () -> workers.Environments.Length = 3)

    let stopped () =
        workers.Environments |> List.filter _.Token.IsCancellationRequested |> List.length

    // both extra workers are stopped, but not the baseline worker, whose token is only cancelled when the job
    // stops: the workers run in no particular order, so the baseline is told apart by still running
    do! eventually (fun () -> stopped () = 2)
    let runs = workers.Runs
    do! eventually (fun () -> workers.Runs >= runs + 3)
    stopped () =! 2
    do! stop job
}

[<Fact(Timeout = 10_000)>]
let ``the job logs an exception from getting its workload and checks again`` () = task {
    let workers = Workers()
    let logger = RecordingLogger()

    use job =
        new TestJob(logger, workers.Work succeed, fun check -> if check = 1 then failwith "unavailable" else Increased)

    do! start job
    do! eventually (fun () -> workers.Environments.Length >= 2)
    do! stop job
    test <@ logger.Messages |> List.exists (fun m -> m.Contains "failed to get its workload") @>
}

[<Fact(Timeout = 10_000)>]
let ``stopping the job stops all its workers`` () = task {
    let workers = Workers()
    use job = new TestJob(RecordingLogger(), workers.Work succeed, fun _ -> Increased)
    do! start job
    do! eventually (fun () -> workers.Environments.Length = 3)
    do! stop job
    test <@ workers.Environments |> List.forall _.Token.IsCancellationRequested @>
    test <@ job.ExecuteTask.IsCompleted @>
}
