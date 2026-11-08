/// Encodings of Orsak.Remoting's values written over other libraries, as the Remoting guide shows them: System.Text.Json,
/// for the same JSON as Orsak.Remoting's own, and MessagePack-CSharp, for another format. Both are checked with
/// Conformance, and MessagePack is called through a server serving both formats.
module Orsak.Tests.WireEncodingTests

open System
open System.Buffers
open System.IO
open System.Text.Json
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.TestHost
open Microsoft.Extensions.Logging
open MessagePack
open Orsak.Remoting
open Orsak.Remoting.AspNetCore
open Orsak.Tests.RemotingTests
open Xunit

/// Orsak.Remoting's JSON, by System.Text.Json.
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

// MessagePackWriter and MessagePackReader are ref structs, which a closure can't hold, so they are passed by reference
// to functions of the module, rather than captured by local ones.
let rec private writeMessagePack (writer: byref<MessagePackWriter>, value: WireValue) =
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
            writeMessagePack (&writer, item)
    | WireValue.Map fields ->
        writer.WriteMapHeader fields.Length

        for name, field in fields do
            writer.Write name
            writeMessagePack (&writer, field)

let rec private readMessagePack (reader: byref<MessagePackReader>) : WireValue =
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
            items.Add(readMessagePack &reader)

        WireValue.Array(List.ofSeq items)
    | MessagePackType.Map ->
        let fields = ResizeArray()

        for _ in 1 .. reader.ReadMapHeader() do
            let name = reader.ReadString()
            fields.Add((name, readMessagePack &reader))

        WireValue.Map(List.ofSeq fields)
    | other -> failwith $"MessagePack's {other} isn't a WireValue"

/// MessagePack, by MessagePack-CSharp.
let messagePack =
    { new IWireEncoding with
        member _.ContentType = "application/vnd.orsak+msgpack"

        member _.Write value =
            let buffer = ArrayBufferWriter<byte>()
            let mutable writer = MessagePackWriter(buffer)
            writeMessagePack (&writer, value)
            writer.Flush()
            buffer.WrittenSpan.ToArray()

        member _.Read body =
            let mutable reader = MessagePackReader(ReadOnlyMemory body)
            readMessagePack &reader }

/// An encoding that loses a float's fraction, which the checks should find.
let lossy =
    let rec round value =
        match value with
        | WireValue.Float f -> WireValue.Float(Math.Round f)
        | WireValue.Array items -> WireValue.Array(List.map round items)
        | WireValue.Map fields -> WireValue.Map [ for name, field in fields -> name, round field ]
        | value -> value

    let json = JsonEncoding.Instance :> IWireEncoding

    { new IWireEncoding with
        member _.ContentType = json.ContentType
        member _.Write value = json.Write(round value)
        member _.Read body = json.Read body }

[<Fact>]
let ``JsonEncoding passes the conformance checks`` () =
    Assert.Empty(Conformance.check JsonEncoding.Instance)

[<Fact>]
let ``an encoding by System.Text.Json passes the conformance checks, and agrees with JsonEncoding`` () =
    Assert.Empty(Conformance.check systemTextJson)
    Assert.Empty(Conformance.agree JsonEncoding.Instance systemTextJson)

[<Fact>]
let ``an encoding by MessagePack passes the conformance checks`` () =
    Assert.Empty(Conformance.check messagePack)

[<Fact>]
let ``Conformance.check finds an encoding that loses a value`` () =
    let failures = Conformance.check lossy
    Assert.Contains(failures, fun f -> f.StartsWith "floats, with and without fractions")

[<Fact>]
let ``Conformance.agree finds encodings of different content types`` () =
    Assert.Contains(Conformance.agree JsonEncoding.Instance messagePack, fun f -> f.StartsWith "The content types differ")

[<Fact(Timeout = 10_000)>]
let ``a server serving JSON and MessagePack answers a client calling in MessagePack`` () = task {
    let builder = WebApplication.CreateBuilder()
    builder.WebHost.UseTestServer() |> ignore
    builder.Logging.ClearProviders() |> ignore
    use app = builder.Build()
    app.MapRemote<ITestApi>("/api", [ JsonFormat.Instance; WireFormat(messagePack) ], fun _ -> TestApi()) |> ignore
    do! app.StartAsync()
    let http = app.GetTestClient()
    let api = TestApiProxy.create (RemoteClient("http://localhost", "/api", WireFormat(messagePack), HttpClientTransport(http)))
    let! echoed = api.Echo apples
    let! placed = api.Place { apples with Lines = [ { Product = "plum"; Quantity = 1 } ] }
    Assert.Equal(apples, echoed)
    Assert.Equal<Result<int, PlaceError>>(Error(UnknownProduct "plum"), placed)
}
