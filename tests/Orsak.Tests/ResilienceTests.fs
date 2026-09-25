namespace Orsak.Tests

open System.Net
open System.Threading.Tasks
open FSharp.Control
open Orsak

open Xunit

type Dummy =
    abstract member ok: unit -> Task<Result<unit, string>>

type DummyImp() =
    interface Dummy with
        member _.ok() = task { return Ok() }

module Dummy =
    let mkDummyEff () : Effect<_, unit, string> =
        mkEffect (fun (d: #Dummy) ->
            ValueTask<_>(
                task = task {
                    //some level of effect resumption is needed...
                    //Task.Yield is not enough.
                    //Task.Delay can do it, but it sometimes just hangs.
                    let! x = Dns.GetHostEntryAsync("google.com")
                    return! d.ok ()
                }
            ))
    //indirection is required to repro
    let indirection () = eff { return! mkDummyEff () }

    let run (e: Effect<_, _, _>) : Task = backgroundTask {
        do! Task.Yield()
        let dummy = DummyImp()
        do! e.RunOrFail(dummy)
    }

module ResilienceTests =

    [<Fact>]
    let ``repeating effects work sequentially`` () = task {
        let e = Dummy.indirection () |> Effect.repeatTimes 1000
        do! Dummy.run e
        
        return ()
    }

    [<Fact>]
    let ``repeating effects works when running concurrently if effects are created`` () = task {
        do!
            Task.WhenAll(
                Dummy.indirection () |> Effect.repeatTimes 1000 |> Dummy.run,
                Dummy.indirection () |> Effect.repeatTimes 1000 |> Dummy.run
            )
        
        return ()
    }


    /// A web endpoint without route parameters runs one shared effect value for every request, on many
    /// threads at once, so parallel runs of the same value must not share state (e.g. a state machine).
    let runInParallel (shared: Effect<unit, int, string>) = task {
        // Each run starts on its own thread pool work item, so runs start on several threads at once.
        // Shared state shows up as runs that never complete, so wait with a timeout rather than hang.
        let! results =
            Task
                .WhenAll([| for _ in 1..5000 -> Task.Run<Result<int, string>>(fun () -> (Effect.run () shared).AsTask()) |])
                .WaitAsync(System.TimeSpan.FromSeconds 10.)
        Assert.All(results, fun r -> Assert.Equal(Ok 42, r))
    }

    [<Fact>]
    let ``a single synchronous effect value can be run in parallel`` () = runInParallel (eff { return 42 })

    [<Fact>]
    let ``a single asynchronous effect value can be run in parallel`` () =
        runInParallel (eff {
            do! Task.Delay 1
            let! a = eff { return 20 }
            do! Task.Yield()
            return a + 22
        })
