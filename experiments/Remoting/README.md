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
    return id, history |> List.map (fun o -> o.Note)
}
```

runs on the client as it would against a fake in a test, while the work happens on the server.

## Where it went

The spike became the packages Orsak.Remoting (`src/Orsak.Remoting`: the format, client and transports) and
Orsak.Remoting.AspNetCore (`src/Orsak.Remoting.AspNetCore`: `MapRemote`), with `docs/remoting.md` as their guide. Its
end-to-end run is `tests/Orsak.Remoting.EndToEnd/run.sh`, and its JSON checks are in
`tests/Orsak.Fable.Tests/RemotingTests.fs` and `tests/Orsak.Tests/RemotingTests.fs`. This README records the design
and the findings that led to it.

## The design

A protocol of Orsak's own, with the wire format pluggable, in plain F# that .NET and Fable both compile, and no
dependencies but Fable.Core, for `fetch`:

* **`IWireFormat`** (`Protocol.fs`): a content type, and the encoding and decoding of a call's arguments
  and result, given their types. Fable erases generic arguments at run time, so the types are passed, not found from
  the values. JSON is the first format; a binary one, such as MessagePack, would be another implementation.
* **`JsonFormat`** (`Json.fs`, `JsonCodec.fs`): F# values to a JSON tree by reflection, which .NET and Fable both have
  for F# types, and the tree to text by a writer and parser in plain F#, so both sides write the same text. A record
  is an object, a union case its name or an object of its name and fields, a list, array or tuple an array, an option
  its value or null; int64, decimal and Guid are strings, so JavaScript's numbers don't round them; numbers are written
  and read in the invariant culture. Lists and options are checked before unions, which they also are to reflection.
* **The transport**: a POST for each member, at `{prefix}/{interface}/{member}`, of its arguments as an array, answered
  by its result. `FetchTransport` on Fable, which Node.js has too, `HttpClientTransport` on .NET. A failure that isn't
  the contract's, an unreachable server or an error status, raises `RemoteCallException`.
* **`RemoteClient`**: what a proxy calls, with each member's name and types. `OrderApiProxy` in the end-to-end test's `Shared/Contract.fs`
  is the proxy, written by hand: a generator would write it from the interface.
* **`MapRemote`** (.NET only): maps each member of an interface to an endpoint by reflection,
  reads the arguments in the format of the request's content type, calls the implementation made for the request,
  awaits its task, and writes the result. A content type it has no format for is answered 415.

## Running it

The end-to-end test's `run.sh` starts the server, runs the client against it compiled with Fable on Node.js, and on .NET, which compiles the
same client, and compares what both print:

```text
== Fable, on Node.js
placed: Ok N, notes Some ring twice
without a note: Ok N
empty: Error EmptyOrder
unknown: Error UnknownProduct plum
unreachable: raised RemoteCallException
== .NET
placed: Ok N, notes Some ring twice
...
```

The JSON tests check the JSON for each kind of value, and the round trip back, with `dotnet run` and on Node.js:
the text is the same on both, which is what lets the two sides understand each other.

```text
{"Customer":"ada","Lines":[{"Product":"apple","Quantity":2}],"Note":"ring twice","Codes":[1,2]}
["Empty",{"Circle":1.5},{"Rect":[2,3]}]
["9007199254740993","1.25","6f9619ff-8b86-d011-b42d-00cf4fc964ff"]
```

## What it shows

* **The effects run unchanged:** `orderAndReview` makes two remote calls, and is the same code a test would run with a
  fake `IOrderApi`. Where an effect runs is the environment's choice.
* **Typed errors cross as values,** with their data, as the contract's `Result` is a union like any other.
* **A transport failure is an exception,** `RemoteCallException`, which passes through `Effect.run`: it isn't one of
  the contract's errors, and exceptions are for the unexpected.
* **Lists and options cross the wire,** both ways, on Fable 5, where Fable.Remoting fails (below).
* **Generic arguments must be known where a call is made.** On Fable, `typeof<'T>` in a function that isn't inline
  has no type behind it at run time, so the proxy passes concrete types: a reason to generate proxies.

## Left for later

The packages did three things the spike left: an exception in an implementation is logged and answered 500 with a body
of the server's own, members returning `Async` and `ValueTask` are awaited too, and `MapRemote` returns the endpoints'
group, so authorization applies to all of them. Still to do:

* **Generating the proxy** with Orsak.Myriad, from the interface, beside the effects `[<GenEffects>]` generates.
* **More types:** maps, sets, dates; and `Some None`, which an option written as its value or null can't tell from
  `None`.
* **Cancellation:** a client's token to `fetch`'s `AbortController`, and the request's `RequestAborted` on the server.
* **Versioning:** client and server deployed apart; snapshot tests of a contract's JSON would show a change.
* **Caching the reflection** per type, rather than reflecting on each call.

## Why not Fable.Remoting

The spike started with [Fable.Remoting](https://github.com/Zaid-Ajaj/Fable.Remoting) (client 8.0, server 6.1, on
Fable 5.27), behind an adapter a generator would write. The effects ran, and typed errors crossed, but:

* **F# lists and options don't serialize on the client.** Fable.SimpleJson, which its client uses, takes them for
  ordinary unions under Fable 5: reading `Order list` fails, as it expects the cases `Empty` and `Cons` rather than an
  array; writing a list fails with `Cannot find case undefined in union type`; and an option is written as
  `{"Some": "ring twice"}` rather than `"ring twice"`, which the server can't read. It fails without Orsak too. For
  options, Fable.SimpleJson has an open pull request, #111; lists aren't reported.
* **Its server packages require FSharp.Core 10.1.x exactly** (`[10.1.202, 10.2.0)`), which an Orsak package depending
  on them would impose on every user; Orsak requires 8.0.300 or later.
* **Its client sends with `XMLHttpRequest`,** which Node.js doesn't have, so running it there needs a polyfill.

`ReflectionProbe/` shows what reflection Fable has, which the JSON format is built on.
