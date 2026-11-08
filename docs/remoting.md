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

[The remoting sample](https://github.com/JohSand/Orsak/tree/main/samples/Remoting) is a todo list in the browser, kept
on the server, with all of what this page shows.

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

`JsonFormat` writes F# types the same way on .NET and Fable, as `application/vnd.orsak+json`:

| Type | JSON |
|---|---|
| Record | An object of its fields |
| Union case | Its name without fields, or an object of its name and its field, or an array of its fields |
| List, array, tuple | An array |
| Option | Its value, or `null` |
| `int`, `float` | A number, in the invariant culture |
| `int64`, `decimal`, `Guid` | A string, which JavaScript's numbers don't round |
| `byte array` | A base64 string |

The types may be public or not. Maps, sets and dates aren't supported yet, nor an option of an option: `Some None`
would be written as `None` is.

## Other formats

A format is made of two parts:

* **The mapping,** `WireMapping`, which is Orsak.Remoting's: it turns the values of F# types into `WireValue`s, the
  values every self-describing format has (nil, bools, ints, floats, strings, bytes, arrays and maps), as the table
  above shows for JSON. It is the same for every format, so a contract's types mean the same in each.
* **The encoding,** an `IWireEncoding`: a content type, and `WireValue`s to bytes and back.

`WireFormat(encoding)` puts them together, and `JsonFormat` is `WireFormat(JsonEncoding.Instance)`. So another format,
such as MessagePack or CBOR, is an encoding of a few dozen lines over a library you already use. The server serves the
formats it is given, choosing by the request's content type, and answers 415 to a content type it has no format for:

```fsharp
app.MapRemote<IOrderApi>("/api", [ JsonFormat.Instance; WireFormat(messagePack) ], fun ctx -> ServerOrderApi(environmentOf ctx))
```

An encoding of MessagePack, with MessagePack-CSharp. Its writer and reader are ref structs, which a closure can't hold,
so they're passed by reference to functions of a module:

```fsharp
let rec private write (writer: byref<MessagePackWriter>, value: WireValue) =
    match value with
    | WireValue.Nil -> writer.WriteNil()
    | WireValue.Bool b -> writer.Write b
    | WireValue.Int i -> writer.Write i
    | WireValue.Float f -> writer.Write f
    | WireValue.String s -> writer.Write s
    | WireValue.Bytes b -> writer.Write(ReadOnlySpan b)
    | WireValue.Array items ->
        writer.WriteArrayHeader items.Length

        for item in items do
            write (&writer, item)
    | WireValue.Map fields ->
        writer.WriteMapHeader fields.Length

        for name, field in fields do
            writer.Write name
            write (&writer, field)

let rec private read (reader: byref<MessagePackReader>) : WireValue =
    match reader.NextMessagePackType with
    | MessagePackType.Nil ->
        reader.ReadNil() |> ignore
        WireValue.Nil
    | MessagePackType.Boolean -> WireValue.Bool(reader.ReadBoolean())
    | MessagePackType.Integer -> WireValue.Int(reader.ReadInt32())
    | MessagePackType.Float -> WireValue.Float(reader.ReadDouble())
    | MessagePackType.String -> WireValue.String(reader.ReadString())
    | MessagePackType.Binary -> WireValue.Bytes(reader.ReadBytes().Value.ToArray())
    | MessagePackType.Array ->
        let items = ResizeArray()

        for _ in 1 .. reader.ReadArrayHeader() do
            items.Add(read &reader)

        WireValue.Array(List.ofSeq items)
    | MessagePackType.Map ->
        let fields = ResizeArray()

        for _ in 1 .. reader.ReadMapHeader() do
            let name = reader.ReadString()
            fields.Add((name, read &reader))

        WireValue.Map(List.ofSeq fields)
    | other -> failwith $"MessagePack's {other} isn't a WireValue"

let messagePack =
    { new IWireEncoding with
        member _.ContentType = "application/vnd.orsak+msgpack"

        member _.Write value =
            let buffer = ArrayBufferWriter<byte>()
            let mutable writer = MessagePackWriter(buffer)
            write (&writer, value)
            writer.Flush()
            buffer.WrittenSpan.ToArray()

        member _.Read body =
            let mutable reader = MessagePackReader(ReadOnlyMemory body)
            read &reader }
```

A Fable client needs an encoding that Fable compiles, in plain F# or over a JavaScript library.

## An encoding of the server's own

The client and the server needn't use the same encoding of a content type, only ones that read what the other writes.
A server may write Orsak.Remoting's JSON with System.Text.Json, say, while a Fable client uses `JsonEncoding`:

```fsharp
let systemTextJson =
    { new IWireEncoding with
        member _.ContentType = "application/vnd.orsak+json"

        member _.Write value =
            use stream = new MemoryStream()
            use writer = new Utf8JsonWriter(stream)

            let rec write value =
                match value with
                | WireValue.Nil -> writer.WriteNullValue()
                | WireValue.Bool b -> writer.WriteBooleanValue b
                | WireValue.Int i -> writer.WriteNumberValue i
                | WireValue.Float f -> writer.WriteNumberValue f
                | WireValue.String s -> writer.WriteStringValue s
                | WireValue.Bytes b -> writer.WriteBase64StringValue(ReadOnlySpan b)
                | WireValue.Array items ->
                    writer.WriteStartArray()
                    List.iter write items
                    writer.WriteEndArray()
                | WireValue.Map fields ->
                    writer.WriteStartObject()

                    for name, field in fields do
                        writer.WritePropertyName name
                        write field

                    writer.WriteEndObject()

            write value
            writer.Flush()
            stream.ToArray()

        member _.Read body =
            let rec read (e: JsonElement) =
                match e.ValueKind with
                | JsonValueKind.Null -> WireValue.Nil
                | JsonValueKind.True -> WireValue.Bool true
                | JsonValueKind.False -> WireValue.Bool false
                | JsonValueKind.Number ->
                    match e.TryGetInt32() with
                    | true, i -> WireValue.Int i
                    | _ -> WireValue.Float(e.GetDouble())
                | JsonValueKind.String -> WireValue.String(e.GetString())
                | JsonValueKind.Array -> WireValue.Array [ for item in e.EnumerateArray() -> read item ]
                | _ -> WireValue.Map [ for p in e.EnumerateObject() -> p.Name, read p.Value ]

            use document = JsonDocument.Parse(ReadOnlyMemory body)
            read document.RootElement }
```

The mapping reads leniently: JSON can't tell bytes from a base64 string, nor `2.0` from `2`, so it takes either where it
expects bytes or a float.

## Checking an encoding

`Conformance` checks an encoding, from a test of any framework, with values of every kind of type the mapping supports.
Each check returns its failures, empty if it passes:

```fsharp
[<Fact>]
let ``the MessagePack encoding passes the conformance checks`` () =
    Assert.Empty(Conformance.check messagePack)

[<Fact>]
let ``the server's JSON agrees with the client's`` () =
    Assert.Empty(Conformance.agree JsonEncoding.Instance systemTextJson)
```

`check` writes each value and reads it back. `agree` has each of two encodings read what the other writes, and checks
that their content types are the same. Both encodings above are tested this way in Orsak's own tests.

`IWireFormat` itself, the codecs of a member's arguments and result given their types, is the level below, for a format
with a mapping of its own.
