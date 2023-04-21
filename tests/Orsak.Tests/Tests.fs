namespace Orsak.Tests

open Orsak
open System.Diagnostics
open Xunit
open Swensen.Unquote
open System.Threading
open System.Threading.Tasks
open System.Threading.Channels
open System

[<AutoOpen>]
module Helpers =

    let waiting (ss: SemaphoreSlim) = eff {
        do! ss.WaitAsync().WaitAsync(TimeSpan.FromMilliseconds 1000)

        try
            return ()
        finally
            ss.Release(1) |> ignore
    }

    let takeAndRelease (ss1: SemaphoreSlim) (ss2: SemaphoreSlim) = eff {
        do! ss1.WaitAsync().WaitAsync(TimeSpan.FromMilliseconds 1000)

        ss2.Release 1 |> ignore

        try
            return ()
        finally
            ss1.Release(1) |> ignore
    }

    let releaseAndTake (ss1: SemaphoreSlim) (ss2: SemaphoreSlim) : Effect<_, _, string> = eff {
        ss1.Release 1 |> ignore

        do! ss2.WaitAsync().WaitAsync(TimeSpan.FromMilliseconds 1000)

        try
            return ()
        finally
            ss2.Release(1) |> ignore
    }

    let run (e: Effect<unit, 'b, string>) = e.RunOrFail()

    let expectError (error: 'err) (e: Effect<unit, _, 'err>) =
        e
        |> Effect.map (fun v -> failwith $"Got value %A{v} when expecting error %A{error}")
        |> Effect.tryRecover (fun es ->
            if es = error then
                Ok()
            else
                failwith $"Got error %O{es} when expecting error %O{error}")

module BuilderTests =


    [<Fact>]
    let ``Overload resolution for nested tupled results works`` () =
        let a1 () : Result<((_ * _) * _), _> = Ok((1, 2), 3)

        let a2 (a, b, c) = eff { return (a, b, c) }

        eff {
            let! ((a, b), c) = a1 ()
            return! a2 (a, b, c)
        }
        |> run

    [<Fact>]
    let ``Builder should propagate errors`` () =
        eff {
            let! _unused = Error "Expected error"

            return ()
        }
        |> expectError "Expected error"
        |> run

    [<Fact>]
    let ``Builder should support while`` () =
        run
        <| eff {
            let mutable runme = true

            while runme do
                runme <- false

            return runme =! false
        }

    [<Fact>]
    let ``Builder should support for with IEnumerable`` () =
        run
        <| eff {
            let mutable i = 0

            for j in 1..5 do
                i <- i + j

            return i =! 15
        }

    [<Fact>]
    let ``Builder should support for with tuple`` () =
        run
        <| eff {
            let mutable i = 0

            for j in struct (1, 2, 3, 4, 5, 6, 7) do
                i <- i + j

            return i =! 28
        }

    [<Fact>]
    let ``Builder should support for with IAsyncEnumerable`` () =
        run
        <| eff {
            let chan = Channel.CreateBounded(5)

            for i in 1..5 do
                chan.Writer.TryWrite i |> ignore

            chan.Writer.Complete()

            let mutable i = 0

            for j in chan.Reader.ReadAllAsync() do
                i <- i + j

            return i =! 15
        }

    [<Fact>]
    let ``Builder should support while with ValueTask<bool>`` () =
        eff {
            let chan = Channel.CreateBounded(5)

            for i in 1..5 do
                chan.Writer.TryWrite i |> ignore

            let mutable i = 0

            while chan.Reader.WaitToReadAsync() do
                let mutable j = 0

                while chan.Reader.TryRead &j do
                    i <- i + j

                chan.Writer.Complete()

            return i =! 15
        }
        |> run


    [<Fact>]
    let ``Builder should support and! in an asynchronous fashion`` () =
        eff {
            use lock1 = new SemaphoreSlim(1)
            use lock2 = new SemaphoreSlim(2)
            do lock1.Wait()
            do lock2.Wait()

            let! () = takeAndRelease lock1 lock2
            and! () = releaseAndTake lock1 lock2
            return ()
        }
        |> run

    [<Fact>]
    let ``And! should work with different return types`` () =
        eff {
            let! i = eff { return 1 }
            and! j = eff { return 1m }
            let result = int j + i
            Assert.Equal(2, result)
            return ()
        }
        |> run

    [<Fact>]
    let ``And! should allow result to be bound after`` () =
        eff {
            let! i = eff { return 1 }
            and! j = eff { return 1m }
            let result = int j + i
            Assert.Equal(2, result)
            do! Ok()
            return ()
        }
        |> run

    [<Fact>]
    let ``And! should combine errors`` () =
        eff {
            let! () = eff { return! Error "1" }
            and! () = eff { return! Error "2" }
            return ()
        }
        |> expectError "12"
        |> run

    [<Fact>]
    let ``And! 3 should combine errors`` () =
        eff {
            let! () = eff { return! Error "1" }
            and! () = eff { return! Error "2" }
            and! () = eff { return! Error "3" }
            return ()
        }
        |> expectError "123"
        |> run

    [<Fact>]
    let ``And! 4 should combine errors`` () =
        eff {
            let! () = eff { return! Error "1" }
            and! () = eff { return! Error "2" }
            and! () = eff { return! Error "3" }
            and! () = eff { return! Error "4" }
            return ()
        }
        |> expectError "1234"
        |> run

    [<Fact>]
    let ``And! 5 should combine errors`` () =
        eff {
            let! () = eff { return! Error "1" }
            and! () = eff { return! Error "2" }
            and! () = eff { return! Error "3" }
            and! () = eff { return! Error "4" }
            and! () = eff { return! Error "5" }
            return ()
        }
        |> expectError "12345"
        |> run

    [<Fact>]
    let ``And! 5 only should combine errors`` () =
        eff {
            let! () = eff { return! Error "1" }
            and! i = eff { return 1 }
            and! () = eff { return! Error "3" }
            and! j = eff { return 1 }
            and! () = eff { return! Error "5" }
            return ()
        }
        |> expectError "135"
        |> run


    [<Fact>]
    let ``And! works with different types`` () =
        eff {
            let! i = eff { return 1 }
            and! j = eff { return 1m }
            let result = int j + i
            Assert.Equal(2, result)
            return ()
        }
        |> run

    [<Fact>]
    let ``And! 3 works with different types`` () =
        eff {
            let! i = eff { return 1 }
            and! j = eff { return 1m }
            and! k = eff { return 1L }
            let result = int64 j + int64 i + k
            Assert.Equal(3L, result)
            return ()
        }
        |> run

    [<Fact>]
    let ``And! 4 works with different types`` () =
        eff {
            let! i = eff { return 1 }
            and! j = eff { return 1m }
            and! k = eff { return 1L }
            and! l = eff { return 1uy }
            let result = int64 j + int64 i + k + int64 l
            Assert.Equal(4L, result)
            return ()
        }
        |> run

    [<Fact>]
    let ``And! 5 works with different types`` () =
        eff {
            let! i = eff { return 1 }
            and! j = eff { return 1m }
            and! k = eff { return 1L }
            and! l = eff { return 1uy }
            and! m = eff { return 1.0 }
            let result = int64 j + int64 i + k + int64 l + int64 m
            Assert.Equal(5L, result)
            return ()
        }
        |> run


    [<Fact>]
    let ``And! works with 3 values`` () =
        eff {
            use lock1 = new SemaphoreSlim(1)
            use lock2 = new SemaphoreSlim(2)
            do lock1.Wait()
            do lock2.Wait()

            let! () = takeAndRelease lock1 lock2
            and! () = waiting lock1
            and! () = releaseAndTake lock1 lock2
            return ()
        }
        |> run

    [<Fact>]
    let ``And! works with 4 values`` () =
        eff {
            use lock1 = new SemaphoreSlim(1)
            use lock2 = new SemaphoreSlim(2)
            do lock1.Wait()
            do lock2.Wait()

            let! () = takeAndRelease lock1 lock2
            and! () = waiting lock1
            and! () = waiting lock1
            and! () = releaseAndTake lock1 lock2
            return ()
        }
        |> run

    [<Fact>]
    let ``And! works with 5 values`` () =
        eff {
            use lock1 = new SemaphoreSlim(1)
            use lock2 = new SemaphoreSlim(2)
            do lock1.Wait()
            do lock2.Wait()

            let! () = takeAndRelease lock1 lock2
            and! () = waiting lock1
            and! () = waiting lock1
            and! () = waiting lock1
            and! () = releaseAndTake lock1 lock2
            return ()
        }
        |> run

    let rand = new System.Random()

    [<Theory>]
    [<Repeat.Repeat(10)>]
    let ``And! order is not specified`` (_) =
        eff {
            use lock1 = new SemaphoreSlim(1)
            use lock2 = new SemaphoreSlim(2)
            do lock1.Wait()
            do lock2.Wait()
            let arr = Array.zeroCreate<Effect<_, _, _>> 5
            arr[0] <- takeAndRelease lock1 lock2
            arr[1] <- releaseAndTake lock1 lock2
            arr[2] <- waiting lock1
            arr[3] <- waiting lock1
            arr[4] <- waiting lock1


            let swap x y (a: 'a[]) =
                let tmp = a[x]
                a[x] <- a[y]
                a[y] <- tmp

            Array.iteri (fun i _ -> arr |> swap i (rand.Next(i, Array.length arr))) arr

            let! () = arr[0]
            and! () = arr[1]
            and! () = arr[2]
            and! () = arr[3]
            and! () = arr[4]
            return ()
        }
        |> run

    [<Fact>]
    let ``Builder should support asyncdisposable`` () =
        eff {
            let p = System.IO.Path.Combine(Environment.CurrentDirectory, "Orsak.xml")
            use f = System.IO.File.Open(p, System.IO.FileMode.Open)
            let e = f.ReadByte()
            return e >! 0
        }
        |> run

    [<Fact>]
    let ``Builder should support tryfinally`` () =
        eff {
            let p = System.IO.Path.Combine(Environment.CurrentDirectory, "Orsak.xml")
            let f = System.IO.File.Open(p, System.IO.FileMode.Open)

            try
                let buf = Memory(Array.zeroCreate 1)
                let! e = f.ReadAsync buf
                return e >! 0
            finally
                (f :> IDisposable).Dispose()
        }
        |> run

    let private functionThatThrows () = task {
        do! Task.Delay(100)
        printf "before"
        invalidArg "A" "B"
        printf "there is no after"
    }

    [<Fact>]
    let ``Builder should support trywith`` () =
        eff {
            try
                do! functionThatThrows ()
                return ()
            with e ->
                let d = e.Demystify()
                let _trace = e.StackTrace
                let _trace2 = d.StackTrace
                return ()
        }
        |> run


    [<Fact>]
    let ``while-Builder should abort on error`` () =
        let inlineEffect i = eff {
            if i <> 2 then
                return! Ok()
            else
                return! Error "Expected error"
        }

        eff {
            let mutable counter = 0

            while counter < 5 do
                do! inlineEffect counter
                counter <- counter + 1

            failwith $"Loop continued past 2, got to %i{counter}"
            return ()
        }
        |> expectError "Expected error"
        |> run


    [<Fact>]
    let ``for-Builder should abort on error`` () =
        let inlineEffect i = eff {
            if i = 2 then
                return! Error "Expected error"
            else
                return! Ok()

        }

        (eff {
            let mutable counter = 1

            for j in struct (1, 2, 3, 4, 5) do
                do! inlineEffect j
                counter <- counter + 1

            failwithf "Loop continued past 2, got to %i" counter
            return ()
        })
        |> expectError "Expected error"
        |> run

module CombinatorTests =
    ///Setup so that sequential execution hangs, will fail with timeout
    ///Parallel execution will make progress
    let parTest (lock1: SemaphoreSlim) (lock2: SemaphoreSlim) =
        do lock1.Wait()
        do lock2.Wait()

        [|
            yield takeAndRelease lock1 lock2
            for _ in 1..5 do
                yield waiting lock1

            yield releaseAndTake lock1 lock2
        |]

    ///Setup so that the effect must be run sequentially
    let sequenceTest (lock1: SemaphoreSlim) = [|
        for _ in 1..5 do
            eff {
                Assert.Equal(2, lock1.CurrentCount)
                do! lock1.WaitAsync()

                try
                    do! Task.Delay(10)
                finally
                    lock1.Release(1) |> ignore

                Assert.Equal(2, lock1.CurrentCount)
            }
    |]

    [<Fact>]
    [<Trait("Category", "Par")>]
    let parCombinatorTest () = task {
        use lock1 = new SemaphoreSlim(1)
        use lock2 = new SemaphoreSlim(2)

        do! parTest lock1 lock2 |> Effect.par |> Effect.map ignore |> run
    }

    let inline par2 s =
        eff.Run(eff.WhenAll(s))

    [<Fact>]
    [<Trait("Category", "Par")>]
    let parCombinatorTest2 () =  task {
        use lock1 = new SemaphoreSlim(1)
        use lock2 = new SemaphoreSlim(2)

        do! parTest lock1 lock2 |> par2 |> Effect.map ignore |> run
    }

    [<Fact>]
    [<Trait("Category", "Par")>]
    let parCombinatorTest3 () =  task {
        do!
            [
                eff {
                    do! Task.Delay 100
                }
                eff {
                    do! Task.Delay 100
                }
                eff {
                    do! Task.Delay 100
                }
                eff {
                    do! Task.Delay 100
                }
                eff {
                    do! Task.Delay 100
                }
                eff {
                    do! Task.Delay 100
                }
                eff {
                    do! Task.Delay 100
                }
            ]

            |> par2 |> Effect.map ignore |> run
    }

    [<Fact>]
    [<Trait("Category", "Par")>]
    let parCombinatorTest4 () =  task {
        do!
            [
                eff {
                    do! Task.Delay 100
                }
                eff {
                    do! Task.Delay 100
                }
                eff {
                    do! Task.Delay 100
                }
                eff {
                    do! Task.Delay 100
                }
                eff {
                    do! Task.Delay 100
                }
                eff {
                    do! Task.Delay 100
                }
                eff {
                    do! Task.Delay 100
                }
            ]

            |> Effect.par |> Effect.map ignore |> run
    }

    [<Fact>]
    [<Trait("Category", "Par")>]
    let sequenceFailsParTest () = task {
        use lock1 = new SemaphoreSlim(1)
        use lock2 = new SemaphoreSlim(2)
        let effects = parTest lock1 lock2 |> Effect.sequence

        let! _ = Assert.ThrowsAsync<TimeoutException>(fun () -> run effects)
        return ()
    }

    [<Fact>]
    [<Trait("Category", "Par")>]
    let sequenceCombinatorTest () = task {
        use lock1 = new SemaphoreSlim(2)

        do! sequenceTest lock1 |> Effect.sequence |> Effect.map ignore |> run
    }

    [<Fact>]
    [<Trait("Category", "Par")>]
    let parFailsSequenceTest () = task {
        use lock1 = new SemaphoreSlim(2)
        let effects = sequenceTest lock1 |> Effect.par

        let! _ = Assert.ThrowsAsync<Xunit.Sdk.EqualException>(fun () -> run effects)
        return ()
    }

    [<Fact>]
    [<Trait("Category", "Par")>]
    let parFailsSequenceTest2 () = task {
        use lock1 = new SemaphoreSlim(2)
        let effects = sequenceTest lock1 |> par2

        let! _ = Assert.ThrowsAsync<Xunit.Sdk.EqualException>(fun () -> run effects)
        return ()
    }

    [<Fact>]
    let changeErrorTest () =
        eff {
            do! Task.Yield()
            return! Error 1
        }
        |> Effect.changeError string
        |> expectError "1"

    [<Fact>]
    let changeErrorOnOkTest () =
        eff {
            do! Task.Yield()
            return ()
        }
        |> Effect.changeError (fun (i: int) -> string i)
        |> run

    [<Fact>]
    let timeOutTests () =
        eff {
            do! Task.Delay(TimeSpan.FromSeconds 10)
            return ()
        }
        |> Effect.timeout (TimeSpan.FromMicroseconds 5) "timeout"
        |> expectError "timeout"


    [<Fact>]
    let withCancellationTests () = task {
        use cts = new CancellationTokenSource(TimeSpan.FromMicroseconds 5)

        do!
            eff {
                do! Task.Yield()

                while true do
                    ()

                return ()
            }
            |> Effect.withCancellation cts.Token
            |> run
    }

module EffSeqTests =
    [<Fact>]
    let yieldValuesWorks () =
        effList {
            1
            2
        }
        |> fun es -> eff {
            let! e = es
            e =! [ 1; 2 ]
            return ()
        }
        |> run

    [<Fact>]
    let letYieldValuesWorks () =
        let mutable i = 0

        let incrementer () = eff {
            i <- i + 1
            return i
        }

        effList {
            let! i = incrementer ()
            i
            let! j = incrementer ()
            j
        }
        |> fun es -> eff {
            let! e = es
            e =! [ 1; 2 ]
            return ()
        }
        |> run

module Laws =
    let (===) (e1: Effect<_, _, _>) (e2: Effect<_, _, _>) = task {
        let! r1 = run e1
        let! r2 = run e2
        return r1 =! r2
    }

    let h a = Effect.Create(fun () -> a + 1)

    let (>>=) m f = Effect.bind f m

    [<Fact>]
    let ``Right identity`` () =
        let e = Effect.ret 1
        e >>= Effect.ret === e

    [<Fact>]
    let ``Left identity`` () = Effect.ret 1 >>= h === h 1

    [<Fact>]
    let Associativity () =
        let m = Effect.ret 1
        let g a = Effect.Create(fun () -> a + 2)
        ((m >>= g) >>= h) === (m >>= (fun x -> g x >>= h))


module CreationTests =
    open FSharp.Control

    [<Fact>]
    let ``Has working overload for Task<Result<_,_>>`` () =
        Effect.Create(fun () -> task { return Ok() }).RunOrFail()

    [<Fact>]
    let ``Has working overload for Task<_>`` () =
        Effect.Create(fun () -> task { return () }).RunOrFail()

    [<Fact>]
    let ``Has working overload for ValueTask<Result<_,_>>`` () =
        Effect.Create(fun () -> vtask { return Ok() }).RunOrFail()

    [<Fact>]
    let ``Has working overload for ValueTask<_>`` () =
        Effect.Create(fun () -> vtask { return () }).RunOrFail()

    [<Fact>]
    let ``Has working overload for Async<Result<_,_>>`` () =
        Effect.Create(fun () -> async { return Ok() }).RunOrFail()

    [<Fact>]
    let ``Has working overload for Async<_>`` () =
        Effect.Create(fun () -> async { return () }).RunOrFail()

    [<Fact>]
    let ``Has working overload for Result<_,_>`` () =
        Effect.Create(fun () -> Ok()).RunOrFail()

    [<Fact>]
    let ``Has working overload for 'a`` () = Effect.Create(fun () -> ()).RunOrFail()
