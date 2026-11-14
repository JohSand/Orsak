namespace Orsak.Remoting.AspNetCore

open System
open System.IO
open System.Reflection
open System.Runtime.CompilerServices
open System.Threading
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Routing
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Orsak.Remoting

/// <summary>
/// Serves the members of an interface to remote clients, such as the proxies <c>RemoteClient</c> calls.
/// </summary>
[<Extension>]
type RemoteEndpoints =

    /// Whether a member returns a stream, an IAsyncEnumerable, whose items are written as they come.
    static member private IsStream(returnType: Type) =
        returnType.IsGenericType
        && returnType.GetGenericTypeDefinition() = typedefof<Collections.Generic.IAsyncEnumerable<obj>>

    /// The type of the value a member returns: that of its Task, ValueTask or Async, unit for a Task of none, the
    /// items' of a stream, or itself.
    static member private ResultType(returnType: Type) =
        let generic = if returnType.IsGenericType then returnType.GetGenericTypeDefinition() else null

        if
            generic = typedefof<Task<obj>>
            || generic = typedefof<ValueTask<obj>>
            || generic = typedefof<Async<obj>>
            || RemoteEndpoints.IsStream returnType
        then
            returnType.GetGenericArguments()[0]
        elif typeof<Task>.IsAssignableFrom returnType || returnType = typeof<Void> then
            typeof<unit>
        else
            returnType

    static member internal AwaitTask<'T>(returned: obj, _: CancellationToken) : Task<obj> =
        let t = returned :?> Task<'T>

        if t.IsCompletedSuccessfully then
            Task.FromResult(box t.Result)
        else
            task {
                let! value = t
                return box value
            }

    static member internal AwaitValueTask<'T>(returned: obj, _: CancellationToken) : Task<obj> =
        let t = returned :?> ValueTask<'T>

        if t.IsCompletedSuccessfully then
            Task.FromResult(box t.Result)
        else
            task {
                let! value = t
                return box value
            }

    static member internal AwaitAsync<'T>(returned: obj, cancellationToken: CancellationToken) : Task<obj> = task {
        let! value = Async.StartAsTask(returned :?> Async<'T>, cancellationToken = cancellationToken)
        return box value
    }

    static member private Unit: Task<obj> = Task.FromResult(box ())

    /// <summary>
    /// How the value a member returns is awaited, chosen once, by its return type: a Task, a ValueTask or an Async of a
    /// value, a Task of none, or a value. An Async is started with the token.
    /// </summary>
    static member private Awaiter(returnType: Type) : Func<obj, CancellationToken, Task<obj>> =
        let generic = if returnType.IsGenericType then returnType.GetGenericTypeDefinition() else null

        let ofType name =
            typeof<RemoteEndpoints>
                .GetMethod(name, BindingFlags.NonPublic ||| BindingFlags.Static)
                .MakeGenericMethod(returnType.GetGenericArguments())
                .CreateDelegate<Func<obj, CancellationToken, Task<obj>>>()

        if generic = typedefof<Task<obj>> then
            ofType "AwaitTask"
        elif generic = typedefof<ValueTask<obj>> then
            ofType "AwaitValueTask"
        elif generic = typedefof<Async<obj>> then
            ofType "AwaitAsync"
        elif typeof<Task>.IsAssignableFrom returnType then
            Func<_, _, _>(fun returned _ ->
                let t = returned :?> Task

                if t.IsCompletedSuccessfully then
                    RemoteEndpoints.Unit
                else
                    task {
                        do! t
                        return box ()
                    })
        elif returnType = typeof<Void> then
            Func<_, _, _>(fun _ _ -> RemoteEndpoints.Unit)
        else
            Func<_, _, _>(fun returned _ -> Task.FromResult returned)

    /// <summary>
    /// Writes the items of a stream as frames, as they come, flushing each, and then its end; or, if it fails once
    /// the response has started, so its status can't change, a frame of its failure, without its details.
    /// </summary>
    static member internal WriteStream<'T>(items: obj, codec: IWireCodec, ctx: HttpContext, contentType: string, logger: unit -> ILogger) : Task = task {
        let aborted = ctx.RequestAborted
        let write frame = ctx.Response.Body.WriteAsync(Frames.write frame, aborted)
        ctx.Response.ContentType <- contentType
        // the headers now, so the client starts reading before the first item
        do! ctx.Response.StartAsync(aborted)

        let e = (items :?> Collections.Generic.IAsyncEnumerable<'T>).GetAsyncEnumerator(aborted)

        // the enumerator is disposed after a failure too, awaited, which a finally in a task can't
        let! failure =
            task {
                try
                    let mutable more = true

                    while more do
                        let! next = e.MoveNextAsync()

                        if next then
                            do! write (Item(codec.Encode(box e.Current)))
                            do! ctx.Response.Body.FlushAsync(aborted)
                        else
                            more <- false

                    do! write End
                    return None
                with e ->
                    return Some e
            }

        do! e.DisposeAsync()

        match failure with
        | None -> ()
        | Some(:? OperationCanceledException) when aborted.IsCancellationRequested ->
            logger().LogDebug("The remote stream {Path} was stopped by its client", ctx.Request.Path)
        | Some e ->
            logger().LogError(e, "The remote stream {Path} failed", ctx.Request.Path)

            try
                do! write (Failed "The stream failed on the server.")
            with _ ->
                ()
    }

    static member private Handle<'Api>
        (m: MethodInfo, formats: IWireFormat list, create: HttpContext -> 'Api)
        : RequestDelegate =
        // a CancellationToken isn't sent: the request's RequestAborted is passed in its place
        let parameters = m.GetParameters() |> Array.map (fun p -> p.ParameterType)
        let isToken (t: Type) = t = typeof<CancellationToken>
        let argTypes = parameters |> Array.filter (isToken >> not)

        let withTokens (args: obj array) (token: CancellationToken) =
            let mutable sent = 0

            parameters
            |> Array.map (fun t ->
                if isToken t then
                    box token
                else
                    sent <- sent + 1
                    args[sent - 1])

        // the codecs of each format, made once, when the endpoint is mapped: a type a format can't write fails here
        let resultType = RemoteEndpoints.ResultType m.ReturnType

        let codecs = [
            for f in formats -> f, f.ArgumentsCodecFor argTypes, f.CodecFor resultType
        ]

        // a stream's items are written as they come, by WriteStream of their type; anything else is awaited, by an
        // awaiter of its return type: either made once
        let writeStream =
            if RemoteEndpoints.IsStream m.ReturnType then
                Some(
                    typeof<RemoteEndpoints>
                        .GetMethod("WriteStream", BindingFlags.NonPublic ||| BindingFlags.Static)
                        .MakeGenericMethod(resultType)
                        .CreateDelegate<Func<obj, IWireCodec, HttpContext, string, unit -> ILogger, Task>>()
                )
            else
                None

        let await = RemoteEndpoints.Awaiter m.ReturnType

        RequestDelegate(fun ctx -> task {
            let contentType = string ctx.Request.ContentType

            match
                codecs
                |> List.tryFind (fun (f, _, _) -> contentType.StartsWith(f.ContentType, StringComparison.OrdinalIgnoreCase))
            with
            | None -> ctx.Response.StatusCode <- StatusCodes.Status415UnsupportedMediaType
            | Some(format, arguments, result) ->
                let logger () =
                    ctx.RequestServices.GetRequiredService<ILoggerFactory>().CreateLogger("Orsak.Remoting")

                let! args =
                    task {
                        try
                            use body = new MemoryStream()
                            do! ctx.Request.Body.CopyToAsync(body, ctx.RequestAborted)
                            return Ok(arguments.Decode(body.ToArray()))
                        with e ->
                            return Error e
                    }

                match args with
                | Error e ->
                    // the caller's fault, not the server's: a warning, and 400 with what couldn't be read
                    logger().LogWarning(e, "The arguments of the remote call {Path} couldn't be read", ctx.Request.Path)
                    ctx.Response.StatusCode <- StatusCodes.Status400BadRequest
                    ctx.Response.ContentType <- "text/plain"
                    do! ctx.Response.WriteAsync($"The arguments of {ctx.Request.Path} couldn't be read.")
                | Ok args ->
                    let aborted = ctx.RequestAborted

                    let! response =
                        task {
                            try
                                let returned = m.Invoke(create ctx, withTokens args aborted)

                                match writeStream with
                                | Some _ -> return Ok(Choice2Of2 returned)
                                | None ->
                                    let! value = await.Invoke(returned, aborted)
                                    return Ok(Choice1Of2(result.Encode value))
                            with e ->
                                let e = match e with :? TargetInvocationException as t when not (isNull t.InnerException) -> t.InnerException | e -> e
                                return Error e
                        }

                    match response with
                    | Ok(Choice1Of2 body) ->
                        ctx.Response.ContentType <- format.ContentType
                        do! ctx.Response.Body.WriteAsync(body, aborted)
                    | Ok(Choice2Of2 items) ->
                        do! writeStream.Value.Invoke(items, result, ctx, format.ContentType, logger)
                    | Error(:? OperationCanceledException) when aborted.IsCancellationRequested ->
                        // the client cancelled the call, or went away: nobody reads an answer, and nothing failed
                        logger().LogDebug("The remote call {Path} was cancelled by its client", ctx.Request.Path)
                    | Error e ->
                        // logged, and answered without its details, which a client shouldn't see
                        logger().LogError(e, "The remote call {Path} failed", ctx.Request.Path)
                        ctx.Response.StatusCode <- StatusCodes.Status500InternalServerError
                        ctx.Response.ContentType <- "text/plain"
                        do! ctx.Response.WriteAsync($"The call to {ctx.Request.Path} failed on the server.")
        })

    /// <summary>
    /// Maps each member of <typeparamref name="'Api"/> to a POST endpoint at
    /// <c>{prefix}/{interface}/{member}</c>, which reads the member's arguments in the format of the request's content
    /// type, calls the member of the implementation <paramref name="create"/> makes for the request, and writes its
    /// result in the same format. A content type none of <paramref name="formats"/> has is answered 415, arguments that
    /// can't be read 400, and an exception from the implementation is logged and answered 500, without its details.
    /// </summary>
    /// <remarks>
    /// A member's CancellationToken isn't read from the request: the request's RequestAborted is passed in its place,
    /// and an Async is started with it, so a call stops when its client cancels it or goes away, which is logged at
    /// the debug level and not answered.
    /// The members may return a Task, a ValueTask or an Async, of a value or of none. Reflection finds them, so nothing is
    /// generated for the server. The formats' codecs for each member are made here, once, so a type a format can't
    /// write fails when the endpoints are mapped, as the application starts, rather than on a call. A member's
    /// attributes are its endpoint's metadata, so <c>[&lt;Authorize(Policy = "editors")&gt;]</c> on a member of the
    /// interface applies to it alone, while conventions on the returned group apply to all of them.
    /// </remarks>
    /// <param name="endpoints">The endpoints to add to</param>
    /// <param name="prefix">The path the members' routes start with, such as <c>/api</c></param>
    /// <param name="formats">The formats the endpoints read and write, chosen by content type</param>
    /// <param name="create">Makes the implementation that answers a request, such as one running effects in an
    /// environment of the request</param>
    /// <returns>The group of the endpoints, so authorization and other conventions can be added to all of them</returns>
    /// <example>
    /// <code lang="fsharp">
    /// app.MapRemote&lt;IOrderApi&gt;("/api", [ JsonFormat.Instance ], fun ctx -> ServerOrderApi(env ctx))
    ///    .RequireAuthorization()
    /// </code>
    /// </example>
    [<Extension>]
    static member MapRemote<'Api>
        (endpoints: IEndpointRouteBuilder, prefix: string, formats: IWireFormat list, create: HttpContext -> 'Api)
        : RouteGroupBuilder =
        let api = typeof<'Api>
        let group = endpoints.MapGroup(prefix)

        for m in api.GetMethods() do
            // a member's attributes are its endpoint's metadata, so [<Authorize(Policy = "editors")>] on one applies
            // to it alone, as on a controller's action
            group
                .MapPost($"/{api.Name}/{m.Name}", RemoteEndpoints.Handle(m, formats, create))
                .WithMetadata(m.GetCustomAttributes(true))
            |> ignore

        group
