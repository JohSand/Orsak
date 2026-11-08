# Remoting: effects of a Fable client, run by a .NET server

The question: can a Fable client run the effects of a contract shared with a .NET server, unchanged, with the server
doing the work, and what should carry the calls.

The idea: the contract is an Orsak interface, `IOrderApi`, with its provider and the effects `[<GenEffects>]` would
generate (`OrderApi.place`, `OrderApi.history`). The server implements it by running effects in its own environment.
The client's environment implements it with a proxy that calls the server, so an effect such as

```fsharp
let orderAndReview order = eff {
    let! id = OrderApi.place order
    let! history = OrderApi.history order.Customer
    return id, Array.length history
}
```

runs on the client as it would against a fake in a test, while the work happens on the server.

## The spike

* `Shared/Contract.fs`: the contract, compiled by both, and an adapter between `IOrderApi` and
  [Fable.Remoting](https://github.com/Zaid-Ajaj/Fable.Remoting)'s record of functions, written by hand here. A
  generator would write it from the interface into the user's project, so Orsak needn't depend on Fable.Remoting.
* `Server/`: ASP.NET Core, serving the record with Fable.Remoting.AspNetCore 3.1. Its `IOrderApi` runs effects in the
  server's environment, an in-memory store the client never sees.
* `Client/`: compiled with Fable 5.27, with Fable.Remoting.Client 8. Its environment provides `IOrderApi` through the
  proxy Fable.Remoting builds, behind the adapter.
* `run.sh` starts the server, runs the client on Node.js against it, and stops the server:

```text
placed: Ok (OrderId 1,1)
empty: Error EmptyOrder
unknown: Error (UnknownProduct "plum")
unreachable: raised Http error (0) from server occured while making request to http://localhost:5180/api/Record/Place
```

## What works

* **The effects run unchanged:** `orderAndReview` makes two remote calls, and is the same code a test would run with a
  fake `IOrderApi`. Where an effect runs is the environment's choice.
* **Typed errors cross as values:** `Error EmptyOrder` and `Error (UnknownProduct "plum")`, with their data, as the
  contract's `Result` is serialized like any union.
* **A transport failure is an exception,** which passes through `Effect.run`: not one of the contract's errors, so it
  isn't in `'e`, as exceptions are for the unexpected.
* **The adapter is mechanical:** `Task` to `Async` and back, member by member, which a generator can write.

## What doesn't: Fable.Remoting on Fable 5

* **F# lists and options don't serialize on the client.** Fable.SimpleJson, which Fable.Remoting.Client uses, takes
  them for ordinary unions under Fable 5. Reading `Order list` from the server fails, as it expects the cases `Empty` and
  `Cons` rather than an array; writing a list fails with `Cannot find case undefined in union type`; and an option is
  written as `{"Some": "ring twice"}` rather than `"ring twice"`, which the server can't read (500). Without Orsak too:
  the proxy fails the same way on its own. For options, Fable.SimpleJson has an open pull request, #111, "Match
  against option types prior to union types"; lists aren't reported. The spike's contract uses arrays, and no options,
  to get past it.
* **The server packages require FSharp.Core 10.1.x exactly** (`[10.1.202, 10.2.0)`). An application can live with it.
  An Orsak package depending on them would hold every user to it, as Orsak only requires 8.0.300 or later. Outside
  the range, NuGet warns (NU1608), which a build treating warnings as errors fails on.
* **The client sends with `XMLHttpRequest`,** which browsers have and Node.js doesn't: running it on Node.js, as the
  tests do, needs a polyfill (`xhr2`, set up in `Client/run.js`).

## The choice it leaves

1. **Fable.Remoting, once Fable.SimpleJson handles lists and options on Fable 5.** Its fix is likely small: match
   lists and options before unions, as #111 does for options. Orsak would generate the adapter into the user's project,
   and depend on nothing. It waits on, or contributes to, a library whose pull requests stay open for months.
2. **A small protocol of Orsak's own:** a POST per interface member, sent with `fetch`, which Node.js has, and a
   serializer that agrees on both sides, such as Thoth.Json, with its encoders and decoders generated for the contract's
   types. More to build and maintain, but no third party on the wire, and no FSharp.Core pin.
3. **Restricting contracts to what serializes today,** arrays and no options. Not a real option for F# contracts.
