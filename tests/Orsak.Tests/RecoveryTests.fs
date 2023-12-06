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

[<Fact>]
let ``the effect is executed again on retry`` () = task {
    let! result = createFailingEffect 1 |> Effect.retry |> Effect.run ()

    Ok() =! result
}

let my_yield () = eff { do! Task.Yield() }

[<Fact>]
let ``effects can safely be run multiple times`` () = task {
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

[<Fact>]
let ``the effect is only executed once again on retry`` () = task {
    let! result = createFailingEffect 2 |> Effect.retry |> Effect.run ()

    Error "This is error nr 2 out of 2" =! result
}

[<Fact>]
let ``the effect is executed up to the requested time on retryTimes`` () = task {
    let! result = createFailingEffect 2 |> Effect.retryTimes 3 |> Effect.run ()

    result =! Ok()
}

[<Fact>]
let ``the effect is executed only up to the requested time on retryTimes`` () = task {
    let! result = createFailingEffect 4 |> Effect.retryTimes 3 |> Effect.run ()

    result =! Error "This is error nr 4 out of 4"
}

[<Fact>]
let ``the effect is executed again on if cond is matched on retryIf`` () =
    let cond s = s = "This is error nr 1 out of 1"

    task {
        let! result = createFailingEffect 1 |> Effect.retryIf cond |> Effect.run ()

        result =! Ok()
    }

[<Fact>]
let ``the effect is not executed again on if cond is not matched on retryIf`` () = task {
    let! result =
        createFailingEffect 2
        |> Effect.retryIf ((=) "This is error nr 1 out of 2")
        |> Effect.run ()

    result =! Error "This is error nr 2 out of 2"
}

[<Fact>]
let ``recover is never run on success`` () = task {
    let! result =
        eff { do! Task.Yield() }
        |> Effect.recover (fun _ -> failwith "Never run")
        |> Effect.run ()

    Ok() =! result
}

[<Fact>]
let ``recover is run once on error`` () = task {
    let mutable counter = 0

    let! result =
        eff { return! Error "I am Error" }
        |> Effect.recover (fun _ -> counter <- counter + 1)
        |> Effect.run ()

    Ok() =! result
    counter =! 1
}

[<Fact>]
let ``tryRecovery is never run on success`` () = task {
    let! result =
        eff { do! Task.Yield() }
        |> Effect.tryRecover (fun _ -> failwith "Never run")
        |> Effect.run ()

    Ok() =! result
}

[<Fact>]
let ``tryRecover is run once on error`` () = task {
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


[<Fact>]
let ``onError is never run on success`` () = task {
    let! result =
        eff { do! Task.Yield() }
        |> Effect.onError (fun _ -> failwith "Never run")
        |> Effect.run ()

    Ok() =! result
}

[<Fact>]
let ``onError is run once on error`` () = task {
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
