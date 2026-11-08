/// The behavior of eff and Effect.Create that the .NET and Fable implementations share.
module Orsak.Fable.Tests.EffTests

open System
open System.Threading.Tasks
open Orsak
open Orsak.Fable.Tests.Runner

type IClock =
    abstract Now: unit -> int

type Clock(now: int) =
    interface IClock with
        member _.Now() = now

let now () : Effect<#IClock, int, string> =
    Effect.Create(fun (clock: #IClock) -> clock.Now())

let fail (error: string) : Effect<unit, int, string> = Effect.Error error

let one () : Effect<unit, int, string> = eff { return 1 }

let runUnit (e: Effect<unit, 'a, string>) = task {
    let! result = Effect.run () e
    return result
}

type Resource() =
    member val Disposed = false with get, set

    interface IDisposable with
        member this.Dispose() = this.Disposed <- true

let tests = [
    test "eff returns a value" (fun () -> task {
        let! result = runUnit (eff { return 1 })
        equal (Ok 1) result
    })

    test "eff binds an effect" (fun () -> task {
        let! result = runUnit (eff {
            let! a = one ()
            let! b = one ()
            return a + b
        })

        equal (Ok 2) result
    })

    test "eff stops at the first error" (fun () -> task {
        let mutable reached = false

        let! result = runUnit (eff {
            let! _ = fail "first"
            reached <- true
            return! fail "second"
        })

        equal (Error "first") result
        isTrue "the effect after the error ran" (not reached)
    })

    test "eff binds a task" (fun () -> task {
        let! result = runUnit (eff {
            let! a = Task.FromResult 41
            return a + 1
        })

        equal (Ok 42) result
    })

    test "eff binds a task without a result" (fun () -> task {
        let! result = runUnit (eff {
            do! Task.Delay 1
            return 1
        })

        equal (Ok 1) result
    })

    test "eff binds an async" (fun () -> task {
        let! result = runUnit (eff {
            let! a = async { return 41 }
            return a + 1
        })

        equal (Ok 42) result
    })

    test "eff binds a result" (fun () -> task {
        let! ok = runUnit (eff {
            let! a = (Ok 41: Result<int, string>)
            return a + 1
        })

        let! error = runUnit (eff {
            let! a = (Error "no": Result<int, string>)
            return a + 1
        })

        equal (Ok 42) ok
        equal (Error "no") error
    })

    test "return! returns the result of an effect, a result, a task and an async" (fun () -> task {
        let! fromEffect = runUnit (eff { return! one () })
        let! fromResult = runUnit (eff { return! (Error "no": Result<int, string>) })
        let! fromTask = runUnit (eff { return! Task.FromResult 1 })
        let! fromAsync = runUnit (eff { return! async { return 1 } })
        equal (Ok 1) fromEffect
        equal (Error "no") fromResult
        equal (Ok 1) fromTask
        equal (Ok 1) fromAsync
    })

    test "return! of a task in a try block returns the task's value" (fun () -> task {
        let! result = runUnit (eff {
            try
                return! Task.FromResult 1
            with _ ->
                return 0
        })

        equal (Ok 1) result
    })

    test "if without else runs the effect only when the condition is true" (fun () -> task {
        let mutable runs = 0

        let count condition = eff {
            if condition then
                do! Effect.Create(fun () -> runs <- runs + 1)

            return runs
        }

        let! whenTrue = runUnit (count true)
        let! whenFalse = runUnit (count false)
        equal (Ok 1) whenTrue
        equal (Ok 1) whenFalse
    })

    test "while runs the body until the condition is false" (fun () -> task {
        let mutable i = 0

        let! result = runUnit (eff {
            while i < 3 do
                do! Effect.Create(fun () -> i <- i + 1)

            return i
        })

        equal (Ok 3) result
    })

    test "while stops at an error" (fun () -> task {
        let mutable i = 0

        let! result = runUnit (eff {
            while i < 3 do
                i <- i + 1

                if i = 2 then
                    let! _ = fail "at two"
                    ()

            return i
        })

        equal (Error "at two") result
        equal 2 i
    })

    test "a long while loop doesn't overflow the stack" (fun () -> task {
        let mutable i = 0

        let! result = runUnit (eff {
            while i < 100_000 do
                do! Effect.Create(fun () -> i <- i + 1)

            return i
        })

        equal (Ok 100_000) result
    })

    test "for runs the body for each item" (fun () -> task {
        let mutable sum = 0

        let! result = runUnit (eff {
            for x in [ 1; 2; 3 ] do
                do! Effect.Create(fun () -> sum <- sum + x)

            return sum
        })

        equal (Ok 6) result
    })

    test "for stops at an error" (fun () -> task {
        let mutable seen = []

        let! result = runUnit (eff {
            for x in [ 1; 2; 3 ] do
                seen <- x :: seen

                if x = 2 then
                    let! _ = fail "at two"
                    ()

            return 0
        })

        equal (Error "at two") result
        equal [ 2; 1 ] seen
    })

    test "try with catches an exception from a bound task" (fun () -> task {
        let! result = runUnit (eff {
            try
                // a task that throws, rather than Task.FromException, which Fable compiles to an import that its
                // library doesn't have
                let! _ = task { return (raise (InvalidOperationException "boom"): int) }
                return "not caught"
            with ex ->
                return ex.Message
        })

        equal (Ok "boom") result
    })

    test "try with catches an exception thrown by an effect's function" (fun () -> task {
        let! result = runUnit (eff {
            try
                do! Effect.Create(fun () -> (failwith "boom": unit))
                return "not caught"
            with ex ->
                return ex.Message
        })

        equal (Ok "boom") result
    })

    test "try finally runs the compensation after success and after an error" (fun () -> task {
        let mutable compensated = 0

        let withFinally (e: Effect<unit, int, string>) = eff {
            try
                return! e
            finally
                compensated <- compensated + 1
        }

        let! ok = runUnit (withFinally (one ()))
        let! error = runUnit (withFinally (fail "no"))
        equal (Ok 1) ok
        equal (Error "no") error
        equal 2 compensated
    })

    test "use disposes the resource when the effect ends" (fun () -> task {
        let resource = new Resource()

        let! result = runUnit (eff {
            use r = resource
            return r.Disposed
        })

        equal (Ok false) result
        isTrue "the resource wasn't disposed" resource.Disposed
    })

    test "an effect reads its environment through an interface" (fun () -> task {
        let! result = Effect.run (Clock 42) (now ())
        equal (Ok 42) result
    })

    test "an effect is cold, and runs each time it is run" (fun () -> task {
        let mutable runs = 0
        let counting = Effect.Create(fun () -> runs <- runs + 1)
        let! _ = runUnit counting
        let! _ = runUnit counting
        equal 2 runs
    })

    test "Effect.Create wraps a function returning a value, a result, a task or an async" (fun () -> task {
        let! value = runUnit (Effect.Create(fun () -> 1))
        let! result = runUnit (Effect.Create(fun () -> (Error "no": Result<int, string>)))
        let! fromTask = runUnit (Effect.Create(fun () -> Task.FromResult 1))
        let! fromAsync = runUnit (Effect.Create(fun () -> async { return 1 }))
        let! taskResult = runUnit (Effect.Create(fun () -> Task.FromResult(Ok 1: Result<int, string>)))
        let! asyncResult = runUnit (Effect.Create(fun () -> async { return (Error "no": Result<int, string>) }))
        equal (Ok 1) value
        equal (Error "no") result
        equal (Ok 1) fromTask
        equal (Ok 1) fromAsync
        equal (Ok 1) taskResult
        equal (Error "no") asyncResult
    })

    test "Effect.Error fails with the error" (fun () -> task {
        let! result = runUnit (fail "no")
        equal (Error "no") result
    })

    test "runOrFail returns the value, and raises the error as an exception" (fun () -> task {
        let! value = Effect.runOrFail () (one ())
        equal 1 value

        let mutable raised = false

        try
            let! _ = Effect.runOrFail () (fail "no")
            ()
        with _ ->
            raised <- true

        isTrue "the error wasn't raised" raised
    })
]
