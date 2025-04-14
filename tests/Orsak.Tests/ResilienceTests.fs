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
        mkEffect (fun (d: #IProvide<Dummy>) ->
            ValueTask<_>(
                task = task {
                    //some level of effect resumption is needed...
                    //Task.Yield is not enough.
                    //Task.Delay can do it, but it sometimes just hangs.
                    let! x = Dns.GetHostEntryAsync("google.com")
                    return! d.Effect.ok ()
                }
            ))
    //indirection is required to repro
    let indirection () = eff { return! mkDummyEff () }

    let run (e: Effect<_, _, string>) : Task = backgroundTask {
        do! Task.Yield()

        do! e.RunOrFail(Runner.createFrom<Dummy> (DummyImp()))
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
