namespace Orsak.Tests

open Orsak
open Orsak.Extensions
open System.Diagnostics
open Xunit
open Swensen.Unquote
open System.Threading
open System.Threading.Tasks
open System.Threading.Channels
open System

[<AutoOpen>]
module Helpers =
   
    let waiting (ss: SemaphoreSlim) =
        eff {
            do! ss.WaitAsync().WaitAsync(TimeSpan.FromMilliseconds 100)
            try
                return ()
            finally
                ss.Release(1) |> ignore
        }

    let takeAndRelease (ss1: SemaphoreSlim) (ss2: SemaphoreSlim) =        
        eff {
            do! ss1.WaitAsync().WaitAsync(TimeSpan.FromMilliseconds 100)
            ss2.Release 1 |> ignore
            try
                return ()
            finally
                ss1.Release(1) |> ignore
        }

    let releaseAndTake (ss1: SemaphoreSlim) (ss2: SemaphoreSlim) : Effect<_,_, string> =        
        eff {
            ss1.Release 1 |> ignore
            do! ss2.WaitAsync().WaitAsync(TimeSpan.FromMilliseconds 100)
            try
                return ()
            finally
                ss2.Release(1) |> ignore
        }

    let run(e: Effect<unit, 'b, string >) = 
        e.RunOrFail()

    let expectError (error: 'err) (e: Effect<unit,_,'err>) =
        e
        |> Effect.map (fun v -> failwith $"Got value %A{v} when expecting error %A{error}")
        |> Effect.tryRecover (fun es -> if es = error then Ok () else failwith $"Got error %O{es} when expecting error %O{error}")

module BuilderTests =

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
        run <| eff {
            let mutable runme = true
            while runme do
                runme <- false

            return runme =! false
        }

    [<Fact>]
    let ``Builder should support for with IEnumerable`` () =
        run <| eff {
            let mutable i = 0
            for j in 1..5 do
                i <- i + j

            return i =! 15
        }

    [<Fact>]
    let ``Builder should support for with tuple`` () =
        run <| eff {
            let mutable i = 0
            for j in struct (1, 2, 3, 4, 5, 6, 7) do
                i <- i + j

            return i =! 28
        }

    [<Fact>]
    let ``Builder should support for with IAsyncEnumerable`` () =
        run <| eff {
            let chan = Channel.CreateBounded(5)
            for i in 1..5 do chan.Writer.TryWrite i |> ignore
            chan.Writer.Complete()

            let mutable i = 0
            for j in chan.Reader.ReadAllAsync() do
                i <- i + j

            return i =! 15
        }

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
    let ``And! should combine errors`` () =
        eff {
            let! () = eff { return! Error "1" }
            and! () = eff { return! Error "2" }
            return ()     
        }
        |> expectError "12"
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
    let ``And! works with 5 values`` () =
        run <| eff {
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

    [<Fact>]
    let ``Builder should support asyncdisposable`` () =
        run <| eff {
            let p = System.IO.Path.Combine(Environment.CurrentDirectory, "Orsak.xml")
            use f = System.IO.File.Open(p, System.IO.FileMode.Open)
            let e = f.ReadByte()
            return e >! 0
        }

    [<Fact>]
    let ``Builder should support tryfinally`` () =
        run <| eff {
            let p = System.IO.Path.Combine(Environment.CurrentDirectory, "Orsak.xml")
            let f = System.IO.File.Open(p, System.IO.FileMode.Open)
            try
                let buf = Memory(Array.zeroCreate 1)
                let! e = f.ReadAsync buf
                return e >! 0
            finally
                (f :> IDisposable).Dispose()
        }

    let private functionThatThrows () =
        task {
            do! Task.Delay(100)
            printf "before"
            invalidArg "A" "B"
            printf "there is no after"
        }

    [<Fact>]
    let ``Builder should support trywith`` () =
        run <| eff {
            try
                do! functionThatThrows()
                return ()
            with
            | e -> 
                let d = e.Demystify()
                let _trace = e.StackTrace
                let _trace2 = d.StackTrace
                return ()
        }


    [<Fact>]
    let ``while-Builder should abort on error`` () =
        let inlineEffect i =
            eff {
                if i <> 2 then
                    return! Ok ()
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
        let inlineEffect i =
            eff {
                if i = 2 then
                    return! Error "Expected error"
                else
                    return! Ok ()

            }
        (
        eff {
            let mutable counter = 1
            for j in struct (1, 2, 3, 4, 5) do
                do! inlineEffect j
                counter <- counter + 1
            
            failwithf "Loop continued past 2, got to %i" counter
            return ()
        }
        )
        |> expectError "Expected error"
        |> run

module CombinatorTests =
    ///Setup so that sequencial execution hangs, will fail with timeout
    ///Parrallel execution will make progress
    let parTest (lock1: SemaphoreSlim) (lock2: SemaphoreSlim) =
        do lock1.Wait()
        do lock2.Wait()
        [| 
            yield takeAndRelease lock1 lock2
            for _ in 1..5 do
                yield waiting lock1

            yield releaseAndTake lock1 lock2
        |]  

    ///Setup so that the effect must be run sequencially
    let sequenceTest (lock1: SemaphoreSlim) =
        [|
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
    let parCombinatorTest () =       
        task {
            use lock1 = new SemaphoreSlim(1)
            use lock2 = new SemaphoreSlim(2)
            do! 
                parTest lock1 lock2
                |> Effect.par
                |> Effect.map ignore
                |> run
        }

    [<Fact>]
    let sequenceFailsParTest () =       
        task {
            use lock1 = new SemaphoreSlim(1)
            use lock2 = new SemaphoreSlim(2)
            let effects = 
                parTest lock1 lock2
                |> Effect.sequence

            let! _ = Assert.ThrowsAsync<TimeoutException>(fun () -> run effects)
            return ()
        }

    [<Fact>]
    let sequenceCombinatorTest () =
        task {
            use lock1 = new SemaphoreSlim(2)
            do!
                sequenceTest lock1
                |> Effect.sequence   
                |> Effect.map ignore
                |> run
        }

    [<Fact>]
    let parFailsSequenceTest () =
        task {
            use lock1 = new SemaphoreSlim(2)
            let effects = 
                sequenceTest lock1 
                |> Effect.par   
                
            let! _ = Assert.ThrowsAsync<Xunit.Sdk.EqualException>(fun () -> run effects)
            return ()
        }

module EffSeqTests =
    [<Fact>]
    let yieldValuesWorks () =        
        effSeq {
            1
            2
        }
        |> fun es -> 
            eff {
                let! e = es
                e =! [ 1; 2 ]
                return ()
            }
        |> run

    [<Fact>]
    let letYieldValuesWorks () = 
        let mutable i = 0
        let incrementer () =
            eff {
                i <- i + 1
                return i
            }
        effSeq {
            let! i = incrementer ()
            i
            let! j = incrementer ()
            j 
        }
        |> fun es -> 
            eff {
                let! e = es
                e =! [ 1; 2 ]
                return ()
            }
        |> run

module Laws =
    let (===) (e1: Effect<_,_,_>) (e2: Effect<_,_,_>) = task {
        let! r1 = run e1
        let! r2 = run e2
        return r1 =! r2
    }

    let h a =
        Effect.Create( fun () -> a + 1)

    let (>>=) m f = Effect.bind f m

    [<Fact>]
    let ``Right identity`` () = 
        let e = Effect.ret 1
        e >>= Effect.ret === e

    [<Fact>]
    let ``Left identity`` () = 
        Effect.ret 1 >>= h === h 1

    [<Fact>]
    let Associativity () = 
        let m = Effect.ret 1
        let g a =
            Effect.Create( fun () -> a + 2)
        ((m >>= g) >>= h) === (m >>= (fun x -> g x >>= h))