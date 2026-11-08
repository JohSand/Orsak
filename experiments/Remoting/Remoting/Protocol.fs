namespace Orsak.Remoting

open System
open System.Text
open System.Threading.Tasks

/// How values cross the wire: what a remote call's arguments and result are written as, and read from. JSON is one;
/// a binary format, such as MessagePack, would be another. The types are given, as Fable erases generic arguments at
/// run time, so a format can't find them from the values.
type IWireFormat =
    /// The content type of the requests and responses in this format, by which the server picks a format.
    abstract ContentType: string

    /// Writes the arguments of a call, of the given types.
    abstract EncodeArguments: values: obj array * types: Type array -> byte array

    /// Reads the arguments of a call, of the given types.
    abstract DecodeArguments: body: byte array * types: Type array -> obj array

    /// Writes the result of a call.
    abstract EncodeResult: value: obj * Type -> byte array

    /// Reads the result of a call.
    abstract DecodeResult: body: byte array * Type -> obj

/// JSON, written by Orsak.Remoting's own codec, the same on .NET and Fable: the arguments are an array.
type JsonFormat() =
    static member val Instance = JsonFormat()

    interface IWireFormat with
        member _.ContentType = "application/json"

        member _.EncodeArguments(values, types) =
            JArray(List.ofArray (Array.map2 JsonCodec.encode types values)) |> Json.write |> Encoding.UTF8.GetBytes

        member _.DecodeArguments(body, types) =
            match Json.parse (Encoding.UTF8.GetString body) with
            | JArray items when items.Length = types.Length -> Array.map2 JsonCodec.decode types (Array.ofList items)
            | json -> failwith $"Expected an array of {types.Length} arguments, but got {Json.write json}"

        member _.EncodeResult(value, t) =
            JsonCodec.encode t value |> Json.write |> Encoding.UTF8.GetBytes

        member _.DecodeResult(body, t) =
            Json.parse (Encoding.UTF8.GetString body) |> JsonCodec.decode t

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

/// Calls the members of remote interfaces: what a generated proxy calls, for each member, with its types.
type RemoteClient(baseUrl: string, prefix: string, format: IWireFormat, transport: IRemoteTransport) =
    member _.Call(interfaceName, memberName, args: obj array, argTypes: Type array, resultType: Type) : Task<obj> = task {
        let url = baseUrl.TrimEnd '/' + Routes.ofMember prefix interfaceName memberName
        let! body = transport.Post(url, format.ContentType, format.EncodeArguments(args, argTypes))
        return format.DecodeResult(body, resultType)
    }
