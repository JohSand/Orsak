namespace Orsak.Remoting

// Values of any type, converted by reflection: obj is the type meant, wherever it is inferred
#nowarn "3559"

open System
open System.Text
open System.Threading
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
/// A format of a self-describing encoding, such as JSON or MessagePack: the values of F# types mapped to
/// <see cref="T:Orsak.Remoting.WireValue"/>s, by <see cref="T:Orsak.Remoting.WireMapping"/>, which the encoding writes as
/// bytes. A member's arguments are an <c>Array</c>.
/// </summary>
/// <param name="encoding">The bytes of the values</param>
type WireFormat(encoding: IWireEncoding) =
    /// The encoding of the format's values.
    member _.Encoding = encoding

    interface IWireFormat with
        member _.ContentType = encoding.ContentType

        member _.CodecFor t =
            let mapping = WireMapping.compile t

            {
                new IWireCodec with
                    member _.Encode(value: obj) = mapping.Encode value |> encoding.Write
                    member _.Decode body = encoding.Read body |> mapping.Decode
            }

        member _.ArgumentsCodecFor types =
            let mappings = types |> Array.map WireMapping.compile

            {
                new IWireArgumentsCodec with
                    member _.Encode values =
                        WireValue.Array(List.ofArray (Array.map2 (fun (m: WireMapping) (v: obj) -> m.Encode v) mappings values))
                        |> encoding.Write

                    member _.Decode body =
                        match encoding.Read body with
                        | WireValue.Array items when items.Length = mappings.Length ->
                            Array.map2 (fun (m: WireMapping) item -> m.Decode item) mappings (Array.ofList items)
                        | value -> failwith $"Expected an array of {mappings.Length} arguments, but got %A{value}"
            }

/// <summary>
/// Orsak.Remoting's JSON, <see cref="T:Orsak.Remoting.JsonEncoding"/>, written the same way on .NET and Fable.
/// </summary>
type JsonFormat() =
    inherit WireFormat(JsonEncoding.Instance)

    /// The format, which keeps no state.
    static member val Instance = JsonFormat()

/// <summary>
/// Raised when a remote call fails, for a reason that isn't the contract's: the server is unreachable, or answers with
/// an error status, such as 401 to a caller it doesn't know. The contract's own errors are in its Result.
/// </summary>
/// <param name="message">What failed</param>
/// <param name="statusCode">The status the server answered with, or None if it couldn't be reached</param>
type RemoteCallException(message: string, statusCode: int option) =
    inherit Exception(message)

    /// <summary>Raised when the server couldn't be reached.</summary>
    new(message: string) = RemoteCallException(message, None)

    /// The status the server answered with, such as 401 or 403 from its authorization, or None if it couldn't be
    /// reached.
    member _.StatusCode = statusCode

/// <summary>
/// Sends a request and returns the body of a successful response, raising RemoteCallException otherwise, or
/// OperationCanceledException once the token is cancelled, which aborts the request.
/// </summary>
type IRemoteTransport =
    abstract Post: url: string * contentType: string * body: byte array * cancellationToken: CancellationToken -> Task<byte array>

    /// <summary>
    /// Sends a request and returns the body of a successful response as it comes, for a stream, raising
    /// RemoteCallException otherwise. The token aborts the request, and reading its body, until the body is disposed.
    /// </summary>
    abstract PostStream: url: string * contentType: string * body: byte array * cancellationToken: CancellationToken -> Task<IRemoteBody>

module Routes =
    /// The route of a member of a remote interface: {prefix}/{interface}/{member}.
    let ofMember (prefix: string) (interfaceName: string) (memberName: string) =
        $"{prefix.TrimEnd '/'}/{interfaceName}/{memberName}"

#if FABLE_COMPILER
open Fable.Core

/// <summary>What fetch sends of the browser's credentials, such as cookies: fetch's <c>credentials</c> option.</summary>
[<RequireQualifiedAccess>]
type FetchCredentials =
    /// To the page's own origin only, as fetch does by default.
    | SameOrigin
    /// To other origins too, for a server elsewhere that authenticates with cookies, whose CORS policy allows them.
    | Include
    /// Never.
    | Omit

/// <summary>
/// Sends with fetch, which browsers and Node.js have. On Fable a Task is a promise, so fetch's are awaited as tasks.
/// </summary>
/// <param name="headers">Headers to send with each request, asked for each one, so a token can change, such as
/// <c>fun () -> task { return [ "Authorization", $"Bearer {token}" ] }</c>; none if not given</param>
/// <param name="credentials">What fetch sends of the browser's credentials; to the page's own origin if not
/// given</param>
type FetchTransport(?headers: unit -> Task<(string * string) list>, ?credentials: FetchCredentials) =
    [<Emit("""fetch($0, { method: "POST", headers: Object.fromEntries($1), body: $2, credentials: $3, signal: $4.signal })""")>]
    static member private Fetch
        (url: string, headers: (string * string) array, body: byte array, credentials: string, abort: obj)
        : Task<obj> =
        jsNative

    [<Emit("""$0.arrayBuffer().then(b => new Uint8Array(b))""")>]
    static member private Bytes(response: obj) : Task<byte array> = jsNative

    [<Emit("$0.status")>]
    static member private Status(response: obj) : int = jsNative

    [<Emit("new AbortController()")>]
    static member private AbortController() : obj = jsNative

    [<Emit("$0.abort()")>]
    static member private Abort(controller: obj) : unit = jsNative

    [<Emit("$0.body.getReader()")>]
    static member private BodyReader(response: obj) : obj = jsNative

    // a chunk of the body, empty at its end
    [<Emit("$0.read().then(r => r.done ? new Uint8Array(0) : r.value)")>]
    static member private ReadChunk(reader: obj) : Task<byte array> = jsNative

    [<Emit("$0.cancel().catch(() => {})")>]
    static member private CancelReader(reader: obj) : unit = jsNative

    // Fails with OperationCanceledException if the token is cancelled. On Fable, ThrowIfCancellationRequested and
    // OperationCanceledException() raise a plain Exception, while a cancelled Task.Delay fails with its own
    // OperationCanceledException, which a type test sees as one
    static member private ThrowIfCancelled(token: CancellationToken) : Task<unit> = task {
        if token.IsCancellationRequested then
            do! Task.Delay(0, token)
    }

    member private _.Credentials =
        match defaultArg credentials FetchCredentials.SameOrigin with
        | FetchCredentials.SameOrigin -> "same-origin"
        | FetchCredentials.Include -> "include"
        | FetchCredentials.Omit -> "omit"

    // Fails with RemoteCallException, or OperationCanceledException if the token was cancelled, which aborted it
    static member private Failed (url: string) (cancellationToken: CancellationToken) (ex: exn) : Task<'a> = task {
        do! FetchTransport.ThrowIfCancelled cancellationToken
        return raise (RemoteCallException $"The request to {url} failed: {ex.Message}")
    }

    // Sends the request, and returns its response, and the registration of the token, which aborts it until disposed
    member private this.Start(url: string, contentType: string, body: byte array, cancellationToken: CancellationToken) = task {
        do! FetchTransport.ThrowIfCancelled cancellationToken

        let! extra =
            match headers with
            | Some headers -> headers ()
            | None -> task { return [] }

        // the content type last, so it is the format's whatever the headers have
        let sent = Array.ofList (extra @ [ "Content-Type", contentType ])

        // the token aborts the request, and reading its response
        let abort = FetchTransport.AbortController()
        let registration = cancellationToken.Register(fun () -> FetchTransport.Abort abort) :> IDisposable

        try
            let! response = FetchTransport.Fetch(url, sent, body, this.Credentials, abort)
            return response, registration
        with ex ->
            registration.Dispose()
            return! FetchTransport.Failed url cancellationToken ex
    }

    static member private ThrowIfFailed (url: string) (status: int) (bytes: byte array) =
        if status < 200 || status > 299 then
            raise (RemoteCallException($"{url} answered {status}: {Encoding.UTF8.GetString bytes}", Some status))

    interface IRemoteTransport with
        member this.Post(url, contentType, body, cancellationToken) = task {
            let! response, registration = this.Start(url, contentType, body, cancellationToken)
            use _ = registration

            let! bytes =
                task {
                    try
                        return! FetchTransport.Bytes response
                    with ex ->
                        return! FetchTransport.Failed url cancellationToken ex
                }

            FetchTransport.ThrowIfFailed url (FetchTransport.Status response) bytes
            return bytes
        }

        member this.PostStream(url, contentType, body, cancellationToken) = task {
            let! response, registration = this.Start(url, contentType, body, cancellationToken)
            let status = FetchTransport.Status response

            if status < 200 || status > 299 then
                use _ = registration
                let! bytes = FetchTransport.Bytes response
                FetchTransport.ThrowIfFailed url status bytes

            let reader = FetchTransport.BodyReader response

            return
                { new IRemoteBody with
                    member _.Read() = task {
                        try
                            return! FetchTransport.ReadChunk reader
                        with ex ->
                            return! FetchTransport.Failed url cancellationToken ex
                    }

                    member _.Dispose() =
                        FetchTransport.CancelReader reader
                        registration.Dispose()
                }
        }
#else
open System.Net.Http
open System.Net.Http.Headers

/// <summary>
/// Sends with an HttpClient. Credentials, such as a bearer token, go in its default headers, or in a handler of its
/// own, which can ask for a token on each request.
/// </summary>
type HttpClientTransport(client: HttpClient) =
    interface IRemoteTransport with
        member _.Post(url, contentType, body, cancellationToken) = task {
            use content = new ByteArrayContent(body)
            content.Headers.ContentType <- MediaTypeHeaderValue.Parse contentType

            // a cancelled token raises OperationCanceledException, which isn't an HttpRequestException
            let! response =
                task {
                    try
                        return! client.PostAsync(url, content, cancellationToken)
                    with :? HttpRequestException as ex ->
                        return raise (RemoteCallException $"The request to {url} failed: {ex.Message}")
                }

            let! bytes = response.Content.ReadAsByteArrayAsync(cancellationToken)
            HttpClientTransport.ThrowIfFailed url response bytes
            return bytes
        }

        member _.PostStream(url, contentType, body, cancellationToken) = task {
            let content = new ByteArrayContent(body)
            content.Headers.ContentType <- MediaTypeHeaderValue.Parse contentType
            use request = new HttpRequestMessage(HttpMethod.Post, url, Content = content)

            // the headers only, so the body is read as it comes
            let! response =
                task {
                    try
                        return! client.SendAsync(request, HttpCompletionOption.ResponseHeadersRead, cancellationToken)
                    with :? HttpRequestException as ex ->
                        return raise (RemoteCallException $"The request to {url} failed: {ex.Message}")
                }

            if not response.IsSuccessStatusCode then
                use _ = response
                let! bytes = response.Content.ReadAsByteArrayAsync(cancellationToken)
                HttpClientTransport.ThrowIfFailed url response bytes

            let! stream = response.Content.ReadAsStreamAsync(cancellationToken)
            let buffer = Array.zeroCreate<byte> 16384

            return
                { new IRemoteBody with
                    member _.Read() = task {
                        let! n =
                            task {
                                try
                                    return! stream.ReadAsync(buffer, 0, buffer.Length, cancellationToken)
                                with
                                // a cancelled read may fail with an IOException, as the connection is aborted
                                | _ when cancellationToken.IsCancellationRequested ->
                                    return raise (OperationCanceledException cancellationToken)
                                | ex ->
                                    return raise (RemoteCallException $"Reading the response of {url} failed: {ex.Message}")
                            }

                        return Array.sub buffer 0 n
                    }

                    member _.Dispose() =
                        stream.Dispose()
                        response.Dispose()
                }
        }

    static member private ThrowIfFailed (url: string) (response: HttpResponseMessage) (bytes: byte array) =
        if not response.IsSuccessStatusCode then
            raise (
                RemoteCallException(
                    $"{url} answered {int response.StatusCode}: {Encoding.UTF8.GetString bytes}",
                    Some(int response.StatusCode)
                )
            )
#endif

/// One reading of a stream: the call starts with the first item asked for, and stops when the reading is disposed,
/// or either token is cancelled.
type internal StreamReading<'T>
    (url: string, start: CancellationToken -> Task<IRemoteBody>, decode: byte array -> obj, callToken: CancellationToken, readToken: CancellationToken) =
    let stop = new CancellationTokenSource()
    let byCall = callToken.Register(fun () -> stop.Cancel()) :> IDisposable
    let byRead = readToken.Register(fun () -> stop.Cancel()) :> IDisposable
    let mutable body: IRemoteBody option = None
    let mutable frames: Frames.Reader option = None
    let mutable current = Unchecked.defaultof<'T>

    member _.Current = current

    member _.MoveNext() : Task<bool> = task {
        let! reader =
            match frames with
            | Some reader -> task { return reader }
            | None -> task {
                let! b = start stop.Token
                body <- Some b
                let reader = Frames.Reader b
                frames <- Some reader
                return reader
              }

        match! reader.Next() with
        | Some(Item content) ->
            current <- unbox<'T> (decode content)
            return true
        | Some End -> return false
        | Some(Failed message) -> return raise (RemoteCallException $"The stream of {url} failed on the server: {message}")
        | None -> return raise (RemoteCallException $"The stream of {url} ended before its end.")
    }

    member _.Dispose() =
        // ends the call if it hasn't ended
        stop.Cancel()
        body |> Option.iter (fun b -> b.Dispose())
        byCall.Dispose()
        byRead.Dispose()

/// <summary>
/// A member of a remote interface, with the codecs of its arguments and result, made once: what a proxy calls.
/// </summary>
type RemoteMember(url: string, format: IWireFormat, transport: IRemoteTransport, arguments: IWireArgumentsCodec, result: IWireCodec) =

    /// <summary>
    /// Calls the member, and returns its result as an object. Cancelling the token aborts the call, which raises
    /// OperationCanceledException, and the server's RequestAborted.
    /// </summary>
    /// <param name="args">The arguments sent, in order, without the member's CancellationToken</param>
    /// <param name="cancellationToken">Cancels the call</param>
    member _.Call(args: obj array, cancellationToken: CancellationToken) : Task<obj> = task {
        let! body = transport.Post(url, format.ContentType, arguments.Encode args, cancellationToken)
        return result.Decode body
    }

    /// Calls the member, and returns its result as an object.
    member this.Call(args: obj array) : Task<obj> = this.Call(args, CancellationToken.None)

    /// Calls a member that returns a Task of a value, cancelled by the token.
    member this.Task<'T>(args: obj array, cancellationToken: CancellationToken) : Task<'T> = task {
        let! value = this.Call(args, cancellationToken)
        return unbox<'T> value
    }

    /// Calls a member that returns a Task of a value.
    member this.Task<'T>(args: obj array) : Task<'T> = this.Task<'T>(args, CancellationToken.None)

    /// Calls a member that returns a Task without a value, cancelled by the token.
    member this.UnitTask(args: obj array, cancellationToken: CancellationToken) : Task =
        this.Call(args, cancellationToken) :> Task

    /// Calls a member that returns a Task without a value.
    member this.UnitTask(args: obj array) : Task = this.UnitTask(args, CancellationToken.None)

    /// <summary>
    /// Calls a member that returns a stream, of items of the member's result type, which are read as they come. Each
    /// reading of the stream is a call of its own, cancelled by either token, or by stopping it.
    /// </summary>
    member _.Stream<'T>(args: obj array, cancellationToken: CancellationToken) : RemoteStream<'T> =
        let start token =
            transport.PostStream(url, format.ContentType, arguments.Encode args, token)

        let reading readToken =
            StreamReading<'T>(url, start, result.Decode, cancellationToken, readToken)
#if FABLE_COMPILER
        { new RemoteStream<'T> with
            member _.GetAsyncEnumerator(readToken) =
                let r = reading readToken

                { new IRemoteStreamEnumerator<'T> with
                    member _.Current = r.Current
                    member _.MoveNextAsync() = r.MoveNext()
                    member _.DisposeAsync() = task { r.Dispose() } :> Task
                }
        }
#else
        { new System.Collections.Generic.IAsyncEnumerable<'T> with
            member _.GetAsyncEnumerator(readToken) =
                let r = reading readToken

                { new System.Collections.Generic.IAsyncEnumerator<'T> with
                    member _.Current = r.Current
                    member _.MoveNextAsync() = ValueTask<bool>(r.MoveNext())
                    member _.DisposeAsync() =
                        r.Dispose()
                        ValueTask.CompletedTask
                }
        }
#endif

    /// <summary>Calls a member that returns a stream, of items of the member's result type.</summary>
    member this.Stream<'T>(args: obj array) : RemoteStream<'T> = this.Stream<'T>(args, CancellationToken.None)

    /// Calls a member that returns an Async, cancelled by the Async's own token.
    member this.Async<'T>(args: obj array) : Async<'T> = async {
        let! cancellationToken = Async.CancellationToken
        let! value = this.Call(args, cancellationToken) |> Async.AwaitTask
        return unbox<'T> value
    }

#if !FABLE_COMPILER
    /// Calls a member that returns a ValueTask of a value, cancelled by the token.
    member this.ValueTask<'T>(args: obj array, cancellationToken: CancellationToken) : ValueTask<'T> =
        ValueTask<'T>(this.Task<'T>(args, cancellationToken))

    /// Calls a member that returns a ValueTask of a value.
    member this.ValueTask<'T>(args: obj array) : ValueTask<'T> = this.ValueTask<'T>(args, CancellationToken.None)
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
