/// Costs of Orsak.Remoting's wire format, for a few shapes of value: the mapping of F# values to WireValues and back,
/// and the whole JSON codec, values to bytes and back:
///
///     dotnet run -c Release --project benchmarks/Orsak.Benchmarks -- --filter "*Wire*" --affinity 1
///
/// On a CPU with performance and efficiency cores, pin to a performance core (--affinity), or results
/// vary between runs.
module Orsak.Benchmarks.WireBenchmarks

open System
open BenchmarkDotNet.Attributes
open Orsak.Remoting

type Line = { Product: string; Quantity: int; Price: decimal }

type Order = {
    Id: int
    Customer: string
    Lines: Line list
    Note: string option
    Placed: DateTimeOffset
}

let order id lines = {
    Id = id
    Customer = $"customer {id}"
    Lines = [ for i in 1..lines -> { Product = $"product {i}"; Quantity = i; Price = decimal i * 1.25m } ]
    Note = if id % 2 = 0 then Some "ring twice" else None
    Placed = DateTimeOffset(2026, 11, 14, 12, 0, 0, TimeSpan.Zero)
}

/// One shape of value: its mapping, its codec, and the value, as a WireValue and as bytes.
type Payload(name: string, t: Type, value: obj) =
    let mapping = WireMapping.compile t
    let codec = (JsonFormat.Instance :> IWireFormat).CodecFor t
    let wire = mapping.Encode value
    let bytes = codec.Encode value
    member _.Mapping = mapping
    member _.Codec = codec
    member _.Value = value
    member _.Wire = wire
    member _.Bytes = bytes
    override _.ToString() = name

[<MemoryDiagnoser>]
type WireFormats() =
    static member Payloads() : Payload seq = [
        // a call's usual result: a record of a few fields, with a short list in it
        Payload("one order", typeof<Order>, box (order 1 3))
        // many small values in one array
        Payload("1000 ints", typeof<int list>, box [ 1..1000 ])
        // many records, each with a list
        Payload("100 orders", typeof<Order list>, box [ for i in 1..100 -> order i 5 ])
    ]

    [<ParamsSource("Payloads")>]
    member val Payload = Unchecked.defaultof<Payload> with get, set

    /// F# value to WireValue.
    [<Benchmark>]
    member this.MapEncode() = this.Payload.Mapping.Encode this.Payload.Value

    /// WireValue to F# value.
    [<Benchmark>]
    member this.MapDecode() = this.Payload.Mapping.Decode this.Payload.Wire

    /// F# value to JSON bytes.
    [<Benchmark>]
    member this.Encode() = this.Payload.Codec.Encode this.Payload.Value

    /// JSON bytes to F# value.
    [<Benchmark>]
    member this.Decode() = this.Payload.Codec.Decode this.Payload.Bytes

/// The JSON step alone, WireValues to bytes and back, by Orsak.Remoting's JsonEncoding and by System.Text.Json.
[<MemoryDiagnoser>]
type WireJson() =
    let ours = JsonEncoding.Instance :> IWireEncoding
    let stj = StjEncoding.StjEncoding() :> IWireEncoding

    static member Payloads() = WireFormats.Payloads()

    [<ParamsSource("Payloads")>]
    member val Payload = Unchecked.defaultof<Payload> with get, set

    /// What's measured must be right: the STJ encoding passes the conformance checks and agrees with ours, and reads
    /// back each payload as ours wrote it, and ours as it wrote it.
    [<GlobalSetup>]
    member this.Setup() =
        match Conformance.check stj @ Conformance.agree ours stj with
        | [] -> ()
        | failures -> failwith (String.Join("\n", failures))

        let p = this.Payload

        if p.Mapping.Decode(stj.Read p.Bytes) <> p.Value || p.Mapping.Decode(ours.Read(stj.Write p.Wire)) <> p.Value then
            failwith $"The encodings don't agree on {p}"

    [<Benchmark>]
    member this.OursWrite() = ours.Write this.Payload.Wire

    [<Benchmark>]
    member this.StjWrite() = stj.Write this.Payload.Wire

    /// Both read the text JsonEncoding wrote, as a server would read a Fable client's.
    [<Benchmark>]
    member this.OursRead() = ours.Read this.Payload.Bytes

    [<Benchmark>]
    member this.StjRead() = stj.Read this.Payload.Bytes
