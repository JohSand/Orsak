/// The behavior of the Effect module that the .NET and Fable implementations share.
module Orsak.Fable.Tests.EffectModuleTests

open System
open System.Threading
open System.Threading.Tasks
open Orsak
open Orsak.Fable.Tests.Runner

let value (a: 'a) : Effect<unit, 'a, string> = eff { return a }

let fail (error: string) : Effect<unit, 'a, string> = Effect.Error error

let runUnit (e: Effect<unit, 'a, string>) = task {
    let! result = Effect.run () e
    return result
}

/// An effect that fails the first <paramref name="failures"/> times it runs, and then returns how many runs it took.
let failingTimes failures =
    let mutable runs = 0

    let e = eff {
        runs <- runs + 1

        if runs <= failures then
            return! fail $"run {runs}"
        else
            return runs
    }

    e

/// An effect that never completes: it waits on a task nothing completes, rather than a timer, which would keep
/// Node.js running after the tests.
let never () : Effect<unit, int, string> =
    let pending = TaskCompletionSource<int>()
    Effect.Create(fun () -> pending.Task)

let afterDelay (ms: int) (a: int) : Effect<unit, int, string> = eff {
    do! Task.Delay ms
    return a
}

let tests = [
    test "Effect.ret, Effect.map, Effect.bind and Effect.join" (fun () -> task {
        let! mapped = runUnit (Effect.ret 1 |> Effect.map (fun a -> a + 1))
        let! bound = runUnit (value 1 |> Effect.bind (fun a -> value (a + 2)))
        let! joined = runUnit (Effect.join (value (value 4)))
        let! map2 = runUnit (Effect.map2 (+) (value 2) (value 3))
        equal (Ok 2) mapped
        equal (Ok 3) bound
        equal (Ok 4) joined
        equal (Ok 5) map2
    })

    test "Effect.recover replaces an error with a value" (fun () -> task {
        let! recovered = runUnit (fail "no" |> Effect.recover (fun (e: string) -> e.Length))
        let! untouched = runUnit (value 1 |> Effect.recover (fun _ -> 0))
        equal (Ok 2) recovered
        equal (Ok 1) untouched
    })

    test "Effect.tryRecover and Effect.onError may recover from an error" (fun () -> task {
        let! recovered = runUnit (fail "no" |> Effect.tryRecover (fun _ -> Ok 1))
        let! stillFailed = runUnit (fail "no" |> Effect.tryRecover (fun e -> Error(e + "!")))
        let! viaEffect = runUnit (fail "no" |> Effect.onError (fun _ -> value 2))
        equal (Ok 1) recovered
        equal (Error "no!") stillFailed
        equal (Ok 2) viaEffect
    })

    test "Effect.asResult binds an error as a value" (fun () -> task {
        let! result = runUnit (fail "no" |> Effect.asResult)
        equal (Ok(Error "no": Result<int, string>)) result
    })

    test "Effect.changeError maps the error, and Effect.bindNewError combines two error types" (fun () -> task {
        let! changed = Effect.run () (fail "no" |> Effect.changeError (fun e -> e.Length))
        equal (Error 2: Result<int, int>) changed

        let numberError: Effect<unit, int, int> = Effect.Error 3
        let! combined = Effect.run () (Effect.bindNewError (value 1) (fun _ -> numberError))
        equal (Error(Choice2Of2 3): Result<int, Choice<string, int>>) combined
    })

    test "Effect.forever runs the effect until it succeeds" (fun () -> task {
        let! result = runUnit (failingTimes 2 |> Effect.forever)
        equal (Ok 3) result
    })

    test "Effect.retry runs the effect once more after a failure" (fun () -> task {
        let! once = runUnit (failingTimes 1 |> Effect.retry)
        let! twice = runUnit (failingTimes 2 |> Effect.retry)
        equal (Ok 2) once
        equal (Error "run 2") twice
    })

    test "Effect.retryIf retries only after an error the condition is true for" (fun () -> task {
        let! retried = runUnit (failingTimes 1 |> Effect.retryIf (fun e -> e = "run 1"))
        let! notRetried = runUnit (failingTimes 1 |> Effect.retryIf (fun _ -> false))
        equal (Ok 2) retried
        equal (Error "run 1") notRetried
    })

    test "Effect.retryWhile and Effect.retryTimes retry until they may not" (fun () -> task {
        let! whileTrue = runUnit (failingTimes 3 |> Effect.retryWhile (fun _ -> true))
        let! withinTimes = runUnit (failingTimes 2 |> Effect.retryTimes 2)
        let! beyondTimes = runUnit (failingTimes 3 |> Effect.retryTimes 2)
        equal (Ok 4) whileTrue
        equal (Ok 3) withinTimes
        equal (Error "run 3") beyondTimes
    })

    test "Effect.repeatWhileTrue repeats while the effect returns true" (fun () -> task {
        let mutable runs = 0

        let! result =
            runUnit (
                Effect.repeatWhileTrue (eff {
                    runs <- runs + 1
                    return runs < 3
                })
            )

        equal (Ok()) result
        equal 3 runs
    })

    test "Effect.repeatTimes, Effect.repeatUntil and Effect.repeatForever stop at the first failure" (fun () -> task {
        let mutable runs = 0
        let counting = Effect.Create(fun () -> runs <- runs + 1)

        let! _ = runUnit (Effect.repeatTimes 3 counting)
        equal 3 runs

        runs <- 0
        let! _ = runUnit (Effect.repeatUntil (fun () -> runs = 2) counting)
        equal 2 runs

        let failingAtThree = eff {
            runs <- runs + 1

            if runs = 3 then
                return! fail "at three"
        }

        runs <- 0
        let! forever = runUnit (Effect.repeatForever failingAtThree)
        equal (Error "at three") forever
    })

    test "Effect.repeatUntilCancellation stops once the token is cancelled" (fun () -> task {
        use cts = new CancellationTokenSource()
        let mutable runs = 0

        let counting = Effect.Create(fun () ->
            runs <- runs + 1

            if runs = 3 then
                cts.Cancel())

        let! _ = runUnit (Effect.repeatUntilCancellation cts.Token counting)
        equal 3 runs
    })

    test "Effect.resolveEnvironment and Effect.withEnvironment run an effect in another environment" (fun () -> task {
        let needsInt: Effect<int, int, string> = Effect.Create(fun (n: int) -> n * 2)
        let! resolved = runUnit (Effect.resolveEnvironment 21 needsInt)
        let! derived = Effect.run "abc" (needsInt |> Effect.withEnvironment (fun (s: string) -> s.Length))
        equal (Ok 42) resolved
        equal (Ok 6) derived
    })

    test "Effect.joinResult, Effect.resultJoin, Effect.joinTask and Effect.taskJoin flatten" (fun () -> task {
        let! joinResult = runUnit (Effect.joinResult (value (Ok 1)))
        let! resultJoin = runUnit (Effect.resultJoin (Ok(value 2)))
        let! joinTask = runUnit (Effect.joinTask (value (Task.FromResult 3)))
        let! taskJoin = runUnit (Effect.taskJoin (Task.FromResult(value 4)))
        equal (Ok 1) joinResult
        equal (Ok 2) resultJoin
        equal (Ok 3) joinTask
        equal (Ok 4) taskJoin
    })

    test "Effect.zip combines the results, and both errors" (fun () -> task {
        let! both = runUnit (Effect.zip (value 1) (value 2) (fun struct (a, b) -> a + b))
        let! oneError = runUnit (Effect.zip (value 1) (fail "b") (fun struct (a, b) -> a + b))
        let! twoErrors = runUnit (Effect.zip (fail "a") (fail "b") (fun struct (a: int, b: int) -> a + b))
        equal (Ok 3) both
        equal (Error "b") oneError
        equal (Error "ab") twoErrors
    })

    test "and! starts the effects together, and combines their errors in order" (fun () -> task {
        let! result = runUnit (eff {
            let! a = afterDelay 20 1
            and! b = afterDelay 10 2
            and! c = afterDelay 1 3
            return [ a; b; c ]
        })

        let! errors = runUnit (eff {
            let! _ = fail "a"
            and! _ = value 1
            and! _ = fail "c"
            return 0
        })

        equal (Ok [ 1; 2; 3 ]) result
        equal (Error "ac") errors
    })

    test "Effect.whenAll, Effect.par and Effect.mapPar return the results in order" (fun () -> task {
        let effects = [ afterDelay 20 1; afterDelay 1 2; afterDelay 10 3 ]
        let! whenAll = runUnit (Effect.whenAll effects)
        let! par = runUnit (Effect.par effects)
        let! mapPar = runUnit (Effect.mapPar effects (Effect.map (fun a -> a * 10)))
        equal (Ok [| 1; 2; 3 |]) whenAll
        equal (Ok [ 1; 2; 3 ]) par
        equal (Ok [ 10; 20; 30 ]) mapPar
    })

    test "Effect.whenAll fails with the first error in order" (fun () -> task {
        let! result = runUnit (Effect.whenAll [ value 1; fail "second"; fail "third" ])
        equal (Error "second") result
    })

    test "Effect.par_ runs every effect" (fun () -> task {
        let mutable runs = 0
        let counting = Effect.Create(fun () -> runs <- runs + 1)
        let! result = runUnit (Effect.par_ [ counting; counting; counting ])
        equal (Ok()) result
        equal 3 runs
    })

    test "Effect.parLimited runs at most limit effects at a time, and returns the results in order" (fun () -> task {
        let gate = obj ()
        let mutable running = 0
        let mutable most = 0

        let tracked a = eff {
            lock gate (fun () ->
                running <- running + 1
                most <- max most running)

            do! Task.Delay 5
            lock gate (fun () -> running <- running - 1)
            return a
        }

        let! result = runUnit (Effect.parLimited 2 [ for i in 1..6 -> tracked i ])
        equal (Ok [ 1..6 ]) result
        isTrue $"{most} effects ran at once" (most <= 2)
    })

    test "Effect.parLimited fails with the first error, and Effect.mapParLimited maps each effect" (fun () -> task {
        let! failed = runUnit (Effect.parLimited 2 [ value 1; fail "second"; value 3 ])
        let! mapped = runUnit (Effect.mapParLimited 2 [ value 1; value 2 ] (Effect.map string))
        equal (Error "second") failed
        equal (Ok [ "1"; "2" ]) mapped
    })

    test "Effect.parLimited refuses a limit below one" (fun () -> task {
        let mutable raised = false

        try
            Effect.parLimited 0 [ value 1 ] |> ignore
        with _ ->
            raised <- true

        isTrue "a limit of 0 was accepted" raised
    })

    test "Effect.traverse and Effect.sequence turn an array of effects into an effect of an array" (fun () -> task {
        let! traversed = runUnit (Effect.traverse (fun a -> a * 2) [| value 1; value 2 |])
        let! sequenced = runUnit (Effect.sequence [| value 1; value 2 |])
        let! failed = runUnit (Effect.sequence [| value 1; fail "no" |])
        equal (Ok [| 2; 4 |]) traversed
        equal (Ok [| 1; 2 |]) sequenced
        equal (Error "no") failed
    })

    test "Effect.timeout returns the effect's result when it is in time, and the error when it isn't" (fun () -> task {
        let! inTime = runUnit (afterDelay 1 1 |> Effect.timeout (TimeSpan.FromSeconds 5.) "timed out")
        let! failedInTime = runUnit (fail "no" |> Effect.timeout (TimeSpan.FromSeconds 5.) "timed out")
        let! late = runUnit (never () |> Effect.timeout (TimeSpan.FromMilliseconds 20.) "timed out")
        equal (Ok 1) inTime
        equal (Error "no") failedInTime
        equal (Error "timed out") late
    })

    test "Effect.withCancellation completes when the token is cancelled, without waiting for the effect" (fun () -> task {
        use cts = new CancellationTokenSource()
        let pending = TaskCompletionSource<unit>()
        let waiting: Effect<unit, unit, string> = Effect.Create(fun () -> pending.Task)
        let running = Effect.run () (waiting |> Effect.withCancellation cts.Token)
        cts.Cancel()
        let! result = running
        equal (Ok()) result
    })

    test "Effect.race returns the result of the effect that completes first" (fun () -> task {
        let! first = runUnit (Effect.race (afterDelay 1 1) (never ()))
        let! second = runUnit (Effect.race (never ()) (afterDelay 1 2))
        equal (Ok 1) first
        equal (Ok 2) second
    })

    test "the Effect operators map, bind and apply" (fun () -> task {
        let! mapped = runUnit (Effect.Map(value 1, fun a -> a + 1))
        let! bound = runUnit (value 1 >>= (fun a -> value (a + 2)))
        let! applied = runUnit (value (fun a b -> a + b) <*> value 1 <*> value 3)
        let! errors = runUnit (fail "a" <*> (fail "b": Effect<unit, int, string>))
        equal (Ok 2) mapped
        equal (Ok 3) bound
        equal (Ok 4) applied
        equal (Error "ab": Result<int, string>) errors
    })
]
