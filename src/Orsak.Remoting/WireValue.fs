namespace Orsak.Remoting

/// <summary>
/// A value as the self-describing formats have it, such as JSON, MessagePack and CBOR: what Orsak.Remoting turns the
/// values of F# types into, with <see cref="T:Orsak.Remoting.WireMapping"/>, and an
/// <see cref="T:Orsak.Remoting.IWireEncoding"/> turns into bytes.
/// </summary>
/// <remarks>
/// <c>Int</c> is 32-bit, which JavaScript's numbers hold exactly: an int64, a decimal or a Guid is a <c>String</c>. A
/// format that can't tell some of these apart, as JSON reads <c>Bytes</c> back as a base64 <c>String</c>, and a
/// <c>Float</c> without a fraction as an <c>Int</c>, is still read right, as the mapping accepts either.
/// </remarks>
[<RequireQualifiedAccess>]
type WireValue =
    | Nil
    | Bool of bool
    | Int of int
    | Float of float
    | String of string
    | Bytes of byte array
    | Array of WireValue list
    /// Named values, in order: a record's fields, or a union case's name and fields.
    | Map of (string * WireValue) list

/// <summary>
/// The bytes of a self-describing format: <see cref="T:Orsak.Remoting.WireValue"/>s to bytes and back. Given to a
/// <see cref="T:Orsak.Remoting.WireFormat"/>, it is a format a client and a server can use, with the values of F# types
/// mapped as for every other encoding.
/// </summary>
/// <remarks>
/// A client and a server may use different encodings of the same content type, such as Orsak.Remoting's own JSON in a
/// Fable client and System.Text.Json on the server, as long as each reads what the other writes:
/// <c>Conformance.check</c> and <c>Conformance.agree</c> test that.
/// </remarks>
type IWireEncoding =
    /// The content type of the requests and responses in this encoding, by which the server picks a format.
    abstract ContentType: string

    /// Writes a value.
    abstract Write: value: WireValue -> byte array

    /// Reads a value.
    abstract Read: body: byte array -> WireValue
