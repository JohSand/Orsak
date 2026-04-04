namespace Orsak.Tests

open System.Threading
open System.Threading.Tasks
open FSharp.Control
open Orsak
open Xunit

// Helpers (run, expectError, takeAndRelease, releaseAndTake) come from
// the [<AutoOpen>] Helpers module in Tests.fs.

module WhenAllTests =

    // -----------------------------------------------------------------------
    // Basic correctness
    // -----------------------------------------------------------------------

    [<Fact>]
    let ``empty sequence returns empty array`` () =
        // Type annotation disambiguates the element type.
        (Effect.whenAll [] : Effect<unit, int array, string>)
        |> run
        |> Task.map (fun result -> Assert.Equal<int seq>([||], result))

    [<Fact>]
    let ``all succeed - results are in original index order`` () = task {
        let! result =
            Effect.whenAll [
                eff { return 1 }
                eff { return 2 }
                eff { return 3 }
            ]
            |> run

        Assert.Equal<int seq>([| 1; 2; 3 |], result)
    }

    [<Fact>]
    let ``single failure propagates error`` () =
        (Effect.whenAll [ eff { return! Error "fail" } ] : Effect<unit, int array, string>)
        |> expectError "fail"
        |> run

    // -----------------------------------------------------------------------
    // Error ordering
    // -----------------------------------------------------------------------

    [<Fact>]
    let ``when multiple effects fail the first error by index is returned`` () =
        // The result-collection loop in WhenAll stops at the first Error in index order.
        Effect.whenAll [
            eff { return 0 }
            eff { return! Error "error at index 1" }
            eff { return! Error "error at index 2" }
        ]
        |> expectError "error at index 1"
        |> run

    [<Fact>]
    let ``error at index 0 is returned when first effect fails`` () =
        Effect.whenAll [
            eff { return! Error "first" }
            eff { return 2 }
        ]
        |> expectError "first"
        |> run

    [<Fact>]
    let ``error at last index is returned when only the last effect fails`` () =
        Effect.whenAll [
            eff { return 1 }
            eff { return 2 }
            eff { return! Error "last" }
        ]
        |> expectError "last"
        |> run

    // -----------------------------------------------------------------------
    // Concurrency
    // -----------------------------------------------------------------------

    [<Fact>]
    let ``all effects start concurrently`` () = task {
        // takeAndRelease ss1 ss2: waits for ss1, then signals ss2.
        // releaseAndTake ss1 ss2: signals ss1, then waits for ss2.
        //
        // If both effects run concurrently:
        //   e1 blocks on ss1; e2 releases ss1, unblocking e1;
        //   e1 releases ss2, unblocking e2. Both complete. ✓
        //
        // If effects run sequentially:
        //   e1 starts and blocks on ss1 forever — e2 never starts. DEADLOCK. ✗
        let ss1 = new SemaphoreSlim(0, 1)
        let ss2 = new SemaphoreSlim(0, 1)

        let! _ =
            Effect.whenAll [
                takeAndRelease ss1 ss2
                releaseAndTake ss1 ss2
            ]
            |> run

        ()
    }

    [<Fact>]
    let ``results are in original index order when effects complete in different order`` () = task {
        // CreateWaiters starts all ValueTasks before any awaiting begins.
        // Even though effect[2] completes before effect[0], result[0] must
        // hold effect[0]'s value and result[2] must hold effect[2]'s value.
        let tcs0 = new TaskCompletionSource<int>()
        let tcs2 = new TaskCompletionSource<int>()

        let delayed (tcs: TaskCompletionSource<int>) : Effect<unit, int, string> =
            mkEffect (fun _ ->
                ValueTask<Result<int, string>>(task {
                    let! v = tcs.Task
                    return Ok v
                }))

        let runTask =
            Effect.whenAll [
                delayed tcs0     // index 0 — completes last
                eff { return 2 } // index 1 — completes immediately
                delayed tcs2     // index 2 — completes first
            ]
            |> run

        // Complete in reverse index order.
        tcs2.SetResult(30)
        tcs0.SetResult(10)

        let! result = runTask
        Assert.Equal<int seq>([| 10; 2; 30 |], result)
    }

    // -----------------------------------------------------------------------
    // Input type paths (CreateWaiters optimises for array, IReadOnlyList,
    // IReadOnlyCollection, and falls back to a general seq path)
    // -----------------------------------------------------------------------

    [<Fact>]
    let ``works with F# array input`` () = task {
        // Exercises the explicit (Effect array) branch in CreateWaiters.
        let effects : Effect<unit, int, string> array = [|
            eff { return 1 }
            eff { return 2 }
            eff { return 3 }
        |]

        let! result = Effect.whenAll effects |> run
        Assert.Equal<int seq>([| 1; 2; 3 |], result)
    }

    [<Fact>]
    let ``works with lazy seq input`` () = task {
        // The general seq fallback branch in CreateWaiters does not use ArrayPool
        // and materialises the awaiters into a fresh array (rented = false).
        let effects : Effect<unit, int, string> seq =
            seq {
                eff { return 1 }
                eff { return 2 }
                eff { return 3 }
            }

        let! result = Effect.whenAll effects |> run
        Assert.Equal<int seq>([| 1; 2; 3 |], result)
    }
