---
title: Remoting
category: Guides
categoryindex: 3
index: 10
---

Remoting
========

Orsak.Remoting lets a client, compiled with [Fable](fable.html) or running on .NET, call the members of an interface
on a server. The interface is the contract, so the effects created from it run on the client unchanged, while the work
happens on the server. It is experimental.

## The contract

An interface describing a side effect, as for any effect, in a project the server and the client share:

```fsharp
type Order = { Customer: string; Lines: OrderLine list; Note: string option }

type PlaceError =
    | EmptyOrder
    | UnknownProduct of string

type IOrderApi =
    abstract Place: Order -> Task<Result<OrderId, PlaceError>>
    abstract History: customer: string -> Task<Order list>

type IOrderApiProvider =
    abstract OrderApi: IOrderApi

module OrderApi =
    let place order = Effect.Create(fun (p: #IOrderApiProvider) -> p.OrderApi.Place order)
    let history customer = Effect.Create(fun (p: #IOrderApiProvider) -> p.OrderApi.History customer)
```

An effect built from it doesn't know where it runs:

```fsharp
let orderAndReview order = eff {
    let! id = OrderApi.place order
    let! history = OrderApi.history order.Customer
    return id, history
}
```

## The server

Orsak.Remoting.AspNetCore maps each member of the interface to a POST endpoint, at `{prefix}/{interface}/{member}`,
and calls the implementation made for each request:

```fsharp
app.MapRemote<IOrderApi>("/api", [ JsonFormat.Instance ], fun ctx -> ServerOrderApi(environmentOf ctx))
   .RequireAuthorization()
```

`MapRemote` returns the endpoints' group, so authorization, CORS and other conventions apply to all of them. The
members may return a `Task`, a `ValueTask` or an `Async`, of a value or of none.

## The client

`[<GenRemote>]`, from Orsak.Remoting, on the contract's interface has Orsak.Myriad generate its proxy: the contract's
project references Orsak.Myriad, as for its other generators (see [Generating code](myriad.html)).


```fsharp
[<GenRemote>]
type IOrderApi =
    abstract Place: Order -> Task<Result<OrderId, PlaceError>>
    abstract History: customer: string -> Task<Order list>
```

The client's environment provides the interface with the proxy, which calls the server through a `RemoteClient`:

```fsharp
let client = RemoteClient("https://shop.example", "/api", JsonFormat.Instance, FetchTransport())

type ClientEnv() =
    let orderApi = OrderApiProxy.create client

    interface IOrderApiProvider with
        member _.OrderApi = orderApi

orderAndReview order |> Effect.run (ClientEnv())
```

`FetchTransport` sends with `fetch`, in the browser and on Node.js; on .NET, `HttpClientTransport` sends with an
`HttpClient`. The generated proxy makes the codecs of each member once, with its types, as Fable erases generic
arguments at run time, and calls them for each call:

```fsharp
module OrderApiProxy =
    let create (client: Orsak.Remoting.RemoteClient) : IOrderApi =
        let place = client.Member("IOrderApi", "Place", [| typeof<Order> |], typeof<Result<OrderId, PlaceError>>)
        let history = client.Member("IOrderApi", "History", [| typeof<string> |], typeof<Order list>)

        { new IOrderApi with
            member _.Place a = place.Task [| box a |]
            member _.History a = history.Task [| box a |] }
```

So a type the format can't write fails when the proxy is made, naming the type, and `MapRemote` fails the same way when
it maps the endpoints, as the server starts, rather than either failing on a call. A Fable project compiles the
generated code as it is: Myriad runs when .NET builds the contract's project, not when Fable compiles it.

## Errors

* **The contract's errors are values:** the server's `Error (UnknownProduct "plum")` is the client's, as the
  `Result` crosses the wire like any union.
* **A failed call raises `RemoteCallException`:** a server that can't be reached, or answers with an error status. It
  isn't one of the contract's errors, so it isn't in the effect's error type.
* **An exception on the server** is logged, and answered with status 500 and a body that doesn't say what it was.

## The JSON

`JsonFormat` writes F# types the same way on .NET and Fable:

| Type | JSON |
|---|---|
| Record | An object of its fields |
| Union case | Its name without fields, or an object of its name and its field, or an array of its fields |
| List, array, tuple | An array |
| Option | Its value, or `null` |
| `int`, `float` | A number, in the invariant culture |
| `int64`, `decimal`, `Guid` | A string, which JavaScript's numbers don't round |

Maps, sets and dates aren't supported yet, nor an option of an option: `Some None` would be written as `None` is.

## Other formats

A format is an `IWireFormat`: a content type, and the codecs it makes for a member's arguments and result, given their
types, `ArgumentsCodecFor` and `CodecFor`, each made once and used for every call. A binary format, such as MessagePack,
would be another implementation. The server serves the formats it is
given, choosing by the request's content type, and answers 415 to a content type it has no format for.
