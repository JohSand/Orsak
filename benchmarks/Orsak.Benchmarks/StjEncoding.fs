/// Orsak.Remoting's JSON by System.Text.Json, as a .NET server could read and write it while a Fable client uses
/// JsonEncoding: Utf8JsonWriter straight to UTF-8 bytes, and Utf8JsonReader without a document. It must agree with
/// JsonEncoding, which WireJson checks with Conformance before measuring.
module Orsak.Benchmarks.StjEncoding

open System
open System.Buffers
open System.Text.Encodings.Web
open System.Text.Json
open Orsak.Remoting

let private options =
    // non-ASCII as itself, as JsonEncoding writes it; WireValues are well formed, so nothing to validate
    JsonWriterOptions(Encoder = JavaScriptEncoder.UnsafeRelaxedJsonEscaping, SkipValidation = true)

let rec private write (writer: Utf8JsonWriter) value =
    match value with
    | WireValue.Nil -> writer.WriteNullValue()
    | WireValue.Bool b -> writer.WriteBooleanValue b
    | WireValue.Int i -> writer.WriteNumberValue i
    | WireValue.Float f -> writer.WriteNumberValue f
    | WireValue.String s -> writer.WriteStringValue s
    | WireValue.Bytes b -> writer.WriteBase64StringValue(ReadOnlySpan b)
    | WireValue.Array items ->
        writer.WriteStartArray()

        for item in items do
            write writer item

        writer.WriteEndArray()
    | WireValue.Map fields ->
        writer.WriteStartObject()

        for name, field in fields do
            writer.WritePropertyName name
            write writer field

        writer.WriteEndObject()

/// The value at the reader's token, which it leaves at the value's last token.
let rec private read (reader: byref<Utf8JsonReader>) : WireValue =
    match reader.TokenType with
    | JsonTokenType.Null -> WireValue.Nil
    | JsonTokenType.True -> WireValue.Bool true
    | JsonTokenType.False -> WireValue.Bool false
    | JsonTokenType.Number ->
        // as JsonEncoding reads it: a whole number that fits an int is an Int
        match reader.TryGetInt32() with
        | true, i -> WireValue.Int i
        | _ -> WireValue.Float(reader.GetDouble())
    | JsonTokenType.String -> WireValue.String(reader.GetString())
    | JsonTokenType.StartArray ->
        let items = ResizeArray()

        while reader.Read() && reader.TokenType <> JsonTokenType.EndArray do
            items.Add(read &reader)

        WireValue.Array(List.ofSeq items)
    | JsonTokenType.StartObject ->
        let fields = ResizeArray()

        while reader.Read() && reader.TokenType <> JsonTokenType.EndObject do
            let name = reader.GetString()
            reader.Read() |> ignore
            fields.Add((name, read &reader))

        WireValue.Map(List.ofSeq fields)
    | token -> failwith $"Unexpected {token} in JSON"

type StjEncoding() =
    interface IWireEncoding with
        member _.ContentType = "application/vnd.orsak+json"

        member _.Write value =
            let buffer = ArrayBufferWriter<byte>()
            use writer = new Utf8JsonWriter(buffer, options)
            write writer value
            writer.Flush()
            buffer.WrittenSpan.ToArray()

        member _.Read body =
            let mutable reader = Utf8JsonReader(ReadOnlySpan body)
            reader.Read() |> ignore
            read &reader
