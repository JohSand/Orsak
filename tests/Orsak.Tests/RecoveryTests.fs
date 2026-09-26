module Orsak.Tests.RecoveryTests

open Orsak
open System.Diagnostics
open Xunit
open Swensen.Unquote
open System.Threading
open System.Threading.Tasks
open System.Threading.Channels
open System

let createFailingEffect timesToFail =
    let mutable failureCount = timesToFail

    eff {
        if failureCount = 0 then
            return ()
        else
            do! Task.Yield()
            failureCount <- failureCount - 1
            return! Error $"This is error nr {timesToFail - failureCount} out of {timesToFail}"
    }

[<Fact(Timeout = 10_000)>]
let retry_runs_the_effect_again () = task {
    let! result = createFailingEffect 1 |> Effect.retry |> Effect.run ()

    Ok() =! result
}

let my_yield () = eff { do! Task.Yield() }

[<Fact(Timeout = 10_000)>]
let effects_can_safely_be_run_multiple_times () = task {
    let mutable x = 0

    let theEffect = eff {
        x <- x + 1
        do! my_yield ()
        return x
    }

    let! result = theEffect |> Effect.run ()
    Ok 1 =! result

    let! result = theEffect |> Effect.run ()
    Ok 2 =! result
}

[<Fact(Timeout = 10_000)>]
let retry_runs_the_effect_again_only_once () = task {
    let! result = createFailingEffect 2 |> Effect.retry |> Effect.run ()

    Error "This is error nr 2 out of 2" =! result
}

[<Fact(Timeout = 10_000)>]
let retryTimes_runs_the_effect_up_to_the_requested_times () = task {
    let! result = createFailingEffect 2 |> Effect.retryTimes 3 |> Effect.run ()

    result =! Ok()
}

[<Fact(Timeout = 10_000)>]
let retryTimes_runs_the_effect_only_up_to_the_requested_times () = task {
    let! result = createFailingEffect 4 |> Effect.retryTimes 3 |> Effect.run ()

    result =! Error "This is error nr 4 out of 4"
}

[<Fact(Timeout = 10_000)>]
let retryIf_runs_the_effect_again_when_the_condition_matches () =
    let cond s = s = "This is error nr 1 out of 1"

    task {
        let! result = createFailingEffect 1 |> Effect.retryIf cond |> Effect.run ()

        result =! Ok()
    }

[<Fact(Timeout = 10_000)>]
let retryIf_does_not_run_the_effect_again_when_the_condition_does_not_match () = task {
    let! result =
        createFailingEffect 2
        |> Effect.retryIf ((=) "This is error nr 1 out of 2")
        |> Effect.run ()

    result =! Error "This is error nr 2 out of 2"
}

[<Fact(Timeout = 10_000)>]
let recover_is_never_run_on_success () = task {
    let! result =
        eff { do! Task.Yield() }
        |> Effect.recover (fun _ -> failwith "Never run")
        |> Effect.run ()

    Ok() =! result
}

[<Fact(Timeout = 10_000)>]
let recover_is_run_once_on_error () = task {
    let mutable counter = 0

    let! result =
        eff { return! Error "I am Error" }
        |> Effect.recover (fun _ -> counter <- counter + 1)
        |> Effect.run ()

    Ok() =! result
    counter =! 1
}

[<Fact(Timeout = 10_000)>]
let tryRecover_is_never_run_on_success () = task {
    let! result =
        eff { do! Task.Yield() }
        |> Effect.tryRecover (fun _ -> failwith "Never run")
        |> Effect.run ()

    Ok() =! result
}

[<Fact(Timeout = 10_000)>]
let tryRecover_is_run_once_on_error () = task {
    let mutable counter = 0

    let! result =
        eff { return! Error "I am Error" }
        |> Effect.tryRecover (fun _ ->
            counter <- counter + 1
            Ok())
        |> Effect.run ()

    Ok() =! result
    counter =! 1
}


[<Fact(Timeout = 10_000)>]
let onError_is_never_run_on_success () = task {
    let! result =
        eff { do! Task.Yield() }
        |> Effect.onError (fun _ -> failwith "Never run")
        |> Effect.run ()

    Ok() =! result
}

[<Fact(Timeout = 10_000)>]
let onError_is_run_once_on_error () = task {
    let mutable counter = 0

    let! result =
        eff {
            do! Task.Yield()

            return! Error "I am Error"
        }
        |> Effect.onError (fun _ -> eff {
            counter <- counter + 1
            return ()
        })
        |> Effect.run ()

    Ok() =! result
    counter =! 1
}
