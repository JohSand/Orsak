(**
---
title: Resilience
category: Guides
categoryindex: 3
index: 1
---
*)
(*** condition: prepare ***)
#r "nuget: Microsoft.Extensions.Logging.Abstractions, 8.0.0"
#r "../src/Orsak/bin/Release/net8.0/Orsak.dll"
(**
# Resilience

Calls to other systems fail: networks drop packets, services restart, databases time out. Since effects are values
that only run when asked to, running one again is just a matter of asking again, and Orsak comes with functions for
retrying, backing off, logging failures, timing out, and keeping background work alive. The functions in this guide
come from two places:

* the `Effect` module in `Orsak`, with the general-purpose ones, such as `Effect.retryWhile` and `Effect.timeout`;
* the `Effect` module in `Orsak.Resilience` (.NET 8 and later), with retrying, backoff with jitter, and error logging.

Opening both makes all of them available as `Effect.*`.

This page is a script: it runs with `dotnet fsi docs/resilience.fsx` once Orsak is built, and the output shown is
from such a run. Timings vary between runs, as the backoff is randomized.

## A flaky dependency

The examples use an inventory service, described by an interface in the usual way, with a provider interface and a
module creating the effect:
*)
open System
open System.Diagnostics
open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.Logging
open Orsak
open Orsak.Resilience

type StockError =
    | Unavailable
    | UnknownProduct of string
    | TimedOut

type IInventory =
    abstract GetStock: product: string -> Task<Result<int, StockError>>

type IInventoryProvider =
    abstract Inventory: IInventory

module Inventory =
    let getStock product =
        Effect.Create(fun (p: #IInventoryProvider) -> p.Inventory.GetStock product)

(**
The implementation used here is unavailable for its first `failures` calls, and then answers. It prints each call,
with the time since it was created, and can be slowed down to show timeouts:
*)
type FlakyInventory(failures: int, ?latency: TimeSpan) =
    let clock = Stopwatch.StartNew()
    let mutable calls = 0

    interface IInventory with
        member _.GetStock product = task {
            calls <- calls + 1
            printfn $"%5d{clock.ElapsedMilliseconds} ms  call {calls}"

            match latency with
            | Some latency -> do! Task.Delay latency
            | None -> ()

            if product <> "orsak-mug" then return Error(UnknownProduct product)
            elif calls <= failures then return Error Unavailable
            else return Ok 42
        }

(*** hide ***)
/// A console logger that writes synchronously, so its lines appear in order with the rest of the output.
type ConsoleLoggerFactory() =
    interface ILoggerFactory with
        member _.CreateLogger(category) =
            { new ILogger with
                member _.BeginScope(_) = { new IDisposable with member _.Dispose() = () }
                member _.IsEnabled(_) = true
                member _.Log(level, _, state, ex, format) =
                    printfn $"          {level}: {category}: {format.Invoke(state, ex)}"
            }

        member _.AddProvider(_) = ()
        member _.Dispose() = ()

(**
The environment provides the inventory, and implements `ILoggerFactory`, which `Effect.logError` uses. A real
application would use the logger factory from its host; this one writes to the console.
*)
type Env(inventory: IInventory) =
    let logging: ILoggerFactory = new ConsoleLoggerFactory()

    interface IInventoryProvider with
        member _.Inventory = inventory

    interface ILoggerFactory with
        member _.CreateLogger(category) = logging.CreateLogger(category)
        member _.AddProvider(provider) = logging.AddProvider(provider)
        member _.Dispose() = ()

/// Runs an effect with an inventory, and prints its result.
let runWith (inventory: IInventory) (effect: Effect<Env, 'a, 'e>) =
    let result = effect |> Effect.run (new Env(inventory))
    printfn $"          result: %A{result.AsTask().Result}"

(**
Without any resilience, the first failure is the result:
*)
Inventory.getStock "orsak-mug" |> runWith (FlakyInventory(failures = 2))
(**
```text
    1 ms  call 1
          result: Error Unavailable
```

## Retrying

`Effect.retryTimes` runs the effect again when it fails, up to the given number of times after the first attempt,
and fails with the last error if every attempt does:
*)
Inventory.getStock "orsak-mug"
|> Effect.retryTimes 3L
|> runWith (FlakyInventory(failures = 2))
(**
```text
    2 ms  call 1
    3 ms  call 2
    3 ms  call 3
          result: Ok 42
```

Not every error is worth retrying: an unknown product will stay unknown. `Effect.retryWhile` retries for as long as
the error passes a test, and fails with the first error that doesn't:
*)
let isTransient error =
    match error with
    | Unavailable
    | TimedOut -> true
    | UnknownProduct _ -> false

Inventory.getStock "orsak-cup"
|> Effect.retryWhile isTransient
|> runWith (FlakyInventory(failures = 2))
(**
```text
    0 ms  call 1
          result: Error (UnknownProduct "orsak-cup")
```

`Effect.retryWhile` has no limit, so it suits errors that are known to pass. Combine it with `Effect.retryTimes` to
also bound the attempts.

## Backing off

Retrying immediately puts more load on a service that is already struggling. `Effect.addDelayOnError` waits before
returning a failure, so that the next attempt comes later, and each consecutive failure waits longer: the delay grows
exponentially from the given base delay, with random jitter, so that many clients don't retry in lockstep. A success
resets it. The delays are in seconds:
*)
Inventory.getStock "orsak-mug"
|> Effect.addDelayOnError 0.05<s>
|> Effect.retryTimes 5L
|> runWith (FlakyInventory(failures = 4))
(**
```text
    5 ms  call 1
   62 ms  call 2
   93 ms  call 3
  203 ms  call 4
  329 ms  call 5
          result: Ok 42
```

`Effect.addDelayOnErrorWithMax` also caps the delay. The delays use the environment's `ITimeProvider`, if it
implements one, so that tests can run them on a fake clock, and likewise its `IRandomProvider` for the jitter and its
`ICancellationProvider` to cancel a delay.

## Logging failures

A retried failure is invisible from the outside, unless it is logged. `Effect.logError` logs the error when the
effect fails, with the environment's `ILoggerFactory`, and fails with the same error. Placed before the retry, it logs
every failed attempt:
*)
Inventory.getStock "orsak-mug"
|> Effect.logError (fun logging error ->
    logging.CreateLogger("Inventory").LogWarning("Stock lookup failed: {Error}", error))
|> Effect.addDelayOnError 0.05<s>
|> Effect.retryTimes 5L
|> runWith (FlakyInventory(failures = 2))
(**
```text
    0 ms  call 1
          Warning: Inventory: Stock lookup failed: Unavailable
   76 ms  call 2
          Warning: Inventory: Stock lookup failed: Unavailable
  154 ms  call 3
          result: Ok 42
```

## Timing out

`Effect.timeout` fails with the given error when the effect takes longer than allowed. The effect itself keeps
running in the background, as tasks can't be stopped from the outside; pass a cancellation token to the work to stop
it. A timeout is a transient error, so it combines with retrying. Here every call is too slow, so the attempts are
bounded with `Effect.retryTimes`; `Effect.retryWhile isTransient` would retry forever:
*)
Inventory.getStock "orsak-mug"
|> Effect.timeout (TimeSpan.FromMilliseconds 100.) TimedOut
|> Effect.retryTimes 2L
|> runWith (FlakyInventory(failures = 0, latency = TimeSpan.FromMilliseconds 300.))
(**
```text
    0 ms  call 1
  109 ms  call 2
  217 ms  call 3
          result: Error TimedOut
```

## Background work

A background worker, such as a queue consumer, should keep running: when there is no work, it should wait before
looking again, rather than spin, and when it fails, it should carry on. The `forever` computation expression marks an
effect that isn't meant to complete, and the functions below build one from an effect that does one unit of work.

The queue, and an effect that processes one message, returning whether there was one:
*)
type IQueue =
    abstract TryReceive: unit -> Task<string option>

type IQueueProvider =
    abstract Queue: IQueue

module Queue =
    let tryReceive () =
        Effect.Create(fun (p: #IQueueProvider) -> p.Queue.TryReceive())

let clock = Stopwatch.StartNew()

let processNext () = eff {
    match! Queue.tryReceive () with
    | Some message ->
        printfn $"%5d{clock.ElapsedMilliseconds} ms  processed {message}"
        return true
    | None ->
        printfn $"%5d{clock.ElapsedMilliseconds} ms  queue empty"
        return false
}

(**
`Effect.addDelayWithMax_` waits after the effect returns `false`, with the same growing delay as above, capped at
the maximum, and resets it when the effect returns `true`: the worker checks an idle queue less and less often, and
picks up speed again as soon as there is work. `Effect.retryForever` keeps it going when an attempt fails, and
`Effect.repeatUntilCancellation` repeats it until the token is cancelled:
*)
let worker (token: CancellationToken) =
    processNext ()
    |> Effect.addDelayWithMax_ 0.02<s> 0.3<s>
    |> Effect.retryForever
    |> Effect.repeatUntilCancellation token

(**
The result is an effect of `Never`, a type without values: it can only end through cancellation, or an exception.
The `Forever` pattern matches its result, without a case for success or failure:
*)
let messages = Collections.Concurrent.ConcurrentQueue [ "order-1"; "order-2"; "order-3" ]

type QueueEnv() =
    interface IQueueProvider with
        member _.Queue =
            { new IQueue with
                member _.TryReceive() =
                    match messages.TryDequeue() with
                    | true, message -> Task.FromResult(Some message)
                    | false, _ -> Task.FromResult None
            }

let stopAfter = new CancellationTokenSource(TimeSpan.FromSeconds 1.0)

match (worker stopAfter.Token |> Effect.run (QueueEnv())).AsTask().Result with
| Forever -> printfn "          worker stopped"
(**
```text
   10 ms  processed order-1
   11 ms  processed order-2
   11 ms  processed order-3
   11 ms  queue empty
   32 ms  queue empty
   62 ms  queue empty
  109 ms  queue empty
  186 ms  queue empty
  422 ms  queue empty
  624 ms  queue empty
  937 ms  queue empty
          worker stopped
```

In an ASP.NET Core application, `AddEffectWorker` from `Orsak.AspNetCore` hosts such a worker as a background service.
*)
