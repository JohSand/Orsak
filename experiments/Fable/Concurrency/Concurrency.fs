/// What Effect's concurrency functions can be built on, in Fable 5.27's library, run on Node.js. A call that Fable's
/// library doesn't have compiles, but the module fails to load, so the ones that don't exist are behind #if, and
/// compiled with --define: Task.WhenAll (WHENALL), Task.WhenAny (WHENANY), and the three-argument
/// ArgumentOutOfRangeException (ARGUMENT_OUT_OF_RANGE).
module Concurrency

open System
open System.Threading
open System.Threading.Tasks

let check name (f: unit -> Task<string>) = task {
    try
        let! r = f ()
        printfn $"ok    {name}: {r}"
    with ex ->
        printfn $"FAIL  {name}: {ex.Message}"
}

let sleep (ms: int) (v: int) = task {
    do! Task.Delay ms
    return v
}

task {
    do! check "Task.Delay with a TimeSpan" (fun () -> task {
        do! Task.Delay(TimeSpan.FromMilliseconds 5.)
        return "done"
    })

    do! check "Task.Delay with a token cancelled while it waits" (fun () -> task {
        use cts = new CancellationTokenSource()
        let t = Task.Delay(10_000, cts.Token)
        cts.Cancel()

        try
            do! t
            return "completed, not cancelled"
        with ex ->
            return $"cancelled: {ex.Message}"
    })

    do! check "TaskCompletionSource, set twice" (fun () -> task {
        let tcs = TaskCompletionSource<int>()
        tcs.SetResult 3
        tcs.SetResult 4
        let! v = tcs.Task
        return $"{v}, the second is ignored"
    })

    do! check "CancellationToken.Register" (fun () -> task {
        use cts = new CancellationTokenSource()
        let mutable called = false
        use _ = cts.Token.Register(fun () -> called <- true)
        cts.Cancel()
        return string called
    })

    do! check "ArgumentOutOfRangeException(name, message)" (fun () -> task {
        try
            raise (ArgumentOutOfRangeException("limit", "At least one."))
            return "not raised"
        with :? ArgumentOutOfRangeException as e ->
            return $"caught: {e.Message}"
    })

    do! check "awaiting started tasks in turn, instead of Task.WhenAll" (fun () -> task {
        let started = [| sleep 20 1; sleep 10 2 |]
        let! a = started[0]
        let! b = started[1]
        return $"{a}, {b}"
    })

#if WHENALL
    do! check "Task.WhenAll" (fun () -> task {
        let! r = Task.WhenAll [| sleep 20 1; sleep 10 2 |]
        return $"%A{r}"
    })
#endif
#if WHENANY
    do! check "Task.WhenAny" (fun () -> task {
        let! winner = Task.WhenAny(sleep 30 1, sleep 5 2)
        let! v = winner
        return string v
    })
#endif
#if ARGUMENT_OUT_OF_RANGE
    do! check "ArgumentOutOfRangeException(name, value, message)" (fun () -> task {
        raise (ArgumentOutOfRangeException("limit", 0, "At least one."))
        return "not raised"
    })
#endif
}
|> ignore
