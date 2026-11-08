namespace Orsak.Remoting

// Values of any type, converted by reflection: obj is the type meant, wherever it is inferred
#nowarn "3559"

open System
open System.Text
open System.Threading.Tasks

/// <summary>Writes the values of one type to the wire, and reads them back, in one format.</summary>
type IWireCodec =
    /// Writes a value.
    abstract Encode: value: obj -> byte array

    /// Reads a value.
    abstract Decode: body: byte array -> obj

/// <summary>Writes the arguments of one member to the wire, and reads them back, in one format.</summary>
type IWireArgumentsCodec =
    /// Writes the arguments, in the member's order.
    abstract Encode: values: obj array -> byte array

    /// Reads the arguments, in the member's order.
    abstract Decode: body: byte array -> obj array

/// <summary>
/// How values cross the wire: what a remote call's arguments and result are written as, and read from. JSON is one;
/// a binary format, such as MessagePack, would be another.
/// </summary>
/// <remarks>
/// A format makes a codec for each member's types once, when a client or server is set up, and the codec converts every
/// value after: so a type a format doesn't support fails when it starts, not on its first call. The types are given,
/// as Fable erases generic arguments at run time, so a format can't find them from the values.
/// </remarks>
type IWireFormat =
    /// The content type of the requests and responses in this format, by which the server picks a format.
    abstract ContentType: string

    /// Makes the codec of values of a type, such as a member's result, or fails if the format can't write it.
    abstract CodecFor: t: Type -> IWireCodec

    /// Makes the codec of a member's arguments, of the given types, or fails if the format can't write one of them.
    abstract ArgumentsCodecFor: types: Type array -> IWireArgumentsCodec

/// <summary>
/// JSON, written by Orsak.Remoting's own codec, the same on .NET and Fable. A member's arguments are an array.
/// </summary>
type JsonFormat() =
    /// The format, which keeps no state.
    static member val Instance = JsonFormat()

    interface IWireFormat with
        member _.ContentType = "application/json"

        member _.CodecFor t =
            let codec = JsonCodec.compile t

            {
                new IWireCodec with
                    member _.Encode(value: obj) =
                        codec.Encode value |> Json.write |> Encoding.UTF8.GetBytes

                    member _.Decode body =
                        Json.parse (Encoding.UTF8.GetString body) |> codec.Decode
            }

        member _.ArgumentsCodecFor types =
            let codecs = types |> Array.map JsonCodec.compile

            {
                new IWireArgumentsCodec with
                    member _.Encode values =
                        JArray(List.ofArray (Array.map2 (fun (c: JsonCodec) (v: obj) -> c.Encode v) codecs values))
                        |> Json.write
                        |> Encoding.UTF8.GetBytes

                    member _.Decode body =
                        match Json.parse (Encoding.UTF8.GetString body) with
                        | JArray items when items.Length = codecs.Length ->
                            Array.map2 (fun (c: JsonCodec) item -> c.Decode item) codecs (Array.ofList items)
                        | json -> failwith $"Expected an array of {codecs.Length} arguments, but got {Json.write json}"
            }

/// Raised when a remote call fails, for a reason that isn't the contract's: the server is unreachable, or answers with
/// an error status. The contract's own errors are in its Result.
type RemoteCallException(message: string) =
    inherit Exception(message)

/// Sends a request and returns the body of a successful response, raising RemoteCallException otherwise.
type IRemoteTransport =
    abstract Post: url: string * contentType: string * body: byte array -> Task<byte array>

module Routes =
    /// The route of a member of a remote interface: {prefix}/{interface}/{member}.
    let ofMember (prefix: string) (interfaceName: string) (memberName: string) =
        $"{prefix.TrimEnd '/'}/{interfaceName}/{memberName}"

#if FABLE_COMPILER
open Fable.Core

/// Sends with fetch, which browsers and Node.js have. On Fable a Task is a promise, so fetch's are awaited as tasks.
type FetchTransport() =
    [<Emit("""fetch($0, { method: "POST", headers: { "Content-Type": $1 }, body: $2 })""")>]
    static member private Fetch(url: string, contentType: string, body: byte array) : Task<obj> = jsNative

    [<Emit("""$0.arrayBuffer().then(b => new Uint8Array(b))""")>]
    static member private Bytes(response: obj) : Task<byte array> = jsNative

    [<Emit("$0.status")>]
    static member private Status(response: obj) : int = jsNative

    interface IRemoteTransport with
        member _.Post(url, contentType, body) = task {
            let! response =
                task {
                    try
                        return! FetchTransport.Fetch(url, contentType, body)
                    with ex ->
                        return raise (RemoteCallException $"The request to {url} failed: {ex.Message}")
                }

            let! bytes = FetchTransport.Bytes response
            let status = FetchTransport.Status response

            if status < 200 || status > 299 then
                raise (RemoteCallException $"{url} answered {status}: {Encoding.UTF8.GetString bytes}")

            return bytes
        }
#else
open System.Net.Http
open System.Net.Http.Headers

/// Sends with an HttpClient.
type HttpClientTransport(client: HttpClient) =
    interface IRemoteTransport with
        member _.Post(url, contentType, body) = task {
            use content = new ByteArrayContent(body)
            content.Headers.ContentType <- MediaTypeHeaderValue.Parse contentType

            let! response =
                task {
                    try
                        return! client.PostAsync(url, content)
                    with :? HttpRequestException as ex ->
                        return raise (RemoteCallException $"The request to {url} failed: {ex.Message}")
                }

            let! bytes = response.Content.ReadAsByteArrayAsync()

            if not response.IsSuccessStatusCode then
                raise (RemoteCallException $"{url} answered {int response.StatusCode}: {Encoding.UTF8.GetString bytes}")

            return bytes
        }
#endif

/// <summary>
/// A member of a remote interface, with the codecs of its arguments and result, made once: what a proxy calls.
/// </summary>
type RemoteMember(url: string, format: IWireFormat, transport: IRemoteTransport, arguments: IWireArgumentsCodec, result: IWireCodec) =

    /// Calls the member, and returns its result as an object.
    member _.Call(args: obj array) : Task<obj> = task {
        let! body = transport.Post(url, format.ContentType, arguments.Encode args)
        return result.Decode body
    }

    /// Calls a member that returns a Task of a value.
    member this.Task<'T>(args: obj array) : Task<'T> = task {
        let! value = this.Call args
        return unbox<'T> value
    }

    /// Calls a member that returns a Task without a value.
    member this.UnitTask(args: obj array) : Task = this.Call args :> Task

    /// Calls a member that returns an Async.
    member this.Async<'T>(args: obj array) : Async<'T> = async {
        let! value = this.Call args |> Async.AwaitTask
        return unbox<'T> value
    }

#if !FABLE_COMPILER
    /// Calls a member that returns a ValueTask of a value.
    member this.ValueTask<'T>(args: obj array) : ValueTask<'T> = ValueTask<'T>(this.Task<'T> args)
#endif

/// <summary>
/// Calls the members of remote interfaces on a server, at <c>{baseUrl}{prefix}/{interface}/{member}</c>, in one format,
/// sent by one transport: what a proxy, such as one <c>[&lt;GenRemote&gt;]</c> generates, calls.
/// </summary>
/// <param name="baseUrl">The server's address, such as <c>https://shop.example</c></param>
/// <param name="prefix">The path the members' routes start with, as the server maps them, such as <c>/api</c></param>
/// <param name="format">The format of the calls</param>
/// <param name="transport">What sends them: <c>FetchTransport</c> on Fable, <c>HttpClientTransport</c> on .NET</param>
type RemoteClient(baseUrl: string, prefix: string, format: IWireFormat, transport: IRemoteTransport) =

    /// <summary>
    /// A member of a remote interface, with the codecs of its types, which fails if the format can't write one.
    /// </summary>
    /// <param name="interfaceName">The interface's name, as the server maps it</param>
    /// <param name="memberName">The member's name</param>
    /// <param name="argTypes">The types of the member's arguments, in order</param>
    /// <param name="resultType">The type of the member's result, unit for a task without one</param>
    member _.Member(interfaceName: string, memberName: string, argTypes: Type array, resultType: Type) =
        RemoteMember(
            baseUrl.TrimEnd '/' + Routes.ofMember prefix interfaceName memberName,
            format,
            transport,
            format.ArgumentsCodecFor argTypes,
            format.CodecFor resultType
        )

/// <summary>
/// Generates, with Orsak.Myriad, a proxy for the interface: a class implementing it by calling a
/// <see cref="T:Orsak.Remoting.RemoteClient"/>, named after the interface without its leading I, with Proxy, such as
/// <c>OrderApiProxy</c> for <c>IOrderApi</c>.
/// </summary>
[<AttributeUsage(AttributeTargets.Interface)>]
type GenRemoteAttribute() =
    inherit Attribute()

    /// Whether the code is appended to the file, for Myriad 1.0, which doesn't say so itself.
    member val Inline = false with get, set
