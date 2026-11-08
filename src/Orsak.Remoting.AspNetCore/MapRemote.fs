namespace Orsak.Remoting.AspNetCore

open System
open System.IO
open System.Reflection
open System.Runtime.CompilerServices
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

    /// The value a member returned, awaited: a Task, a ValueTask or an Async of a value, a Task of none, or a value.
    static member private Await(returnType: Type, returned: obj) : Task<obj * Type> = task {
        let generic = if returnType.IsGenericType then returnType.GetGenericTypeDefinition() else null

        if generic = typedefof<Async<obj>> then
            let resultType = returnType.GetGenericArguments()[0]

            let started =
                typeof<Async>
                    .GetMethod("StartAsTask")
                    .MakeGenericMethod(resultType)
                    // its two optional parameters, None
                    .Invoke((null: obj), [| returned; (null: obj); (null: obj) |])

            return! RemoteEndpoints.Await(typedefof<Task<obj>>.MakeGenericType resultType, started)
        elif generic = typedefof<ValueTask<obj>> then
            let task = returnType.GetMethod("AsTask").Invoke(returned, [||])
            return! RemoteEndpoints.Await(typedefof<Task<obj>>.MakeGenericType(returnType.GetGenericArguments()), task)
        else
            match returned with
            | :? Task as t ->
                do! t

                if generic = typedefof<Task<obj>> then
                    return returnType.GetProperty("Result").GetValue(returned), returnType.GetGenericArguments()[0]
                else
                    return box (), typeof<unit>
            | _ when returnType = typeof<Void> -> return box (), typeof<unit>
            | _ -> return returned, returnType
    }

    static member private Handle<'Api>
        (m: MethodInfo, formats: IWireFormat list, create: HttpContext -> 'Api)
        : RequestDelegate =
        let argTypes = m.GetParameters() |> Array.map (fun p -> p.ParameterType)

        RequestDelegate(fun ctx -> task {
            let contentType = string ctx.Request.ContentType

            match formats |> List.tryFind (fun f -> contentType.StartsWith(f.ContentType, StringComparison.OrdinalIgnoreCase)) with
            | None -> ctx.Response.StatusCode <- StatusCodes.Status415UnsupportedMediaType
            | Some format ->
                let! result =
                    task {
                        try
                            use body = new MemoryStream()
                            do! ctx.Request.Body.CopyToAsync(body, ctx.RequestAborted)
                            let args = format.DecodeArguments(body.ToArray(), argTypes)
                            let returned = m.Invoke(create ctx, args)
                            let! value, resultType = RemoteEndpoints.Await(m.ReturnType, returned)
                            return Ok(format.EncodeResult(value, resultType))
                        with e ->
                            let e = match e with :? TargetInvocationException as t when not (isNull t.InnerException) -> t.InnerException | e -> e
                            return Error e
                    }

                match result with
                | Ok body ->
                    ctx.Response.ContentType <- format.ContentType
                    do! ctx.Response.Body.WriteAsync(body, ctx.RequestAborted)
                | Error e ->
                    // logged, and answered without its details, which a client shouldn't see
                    let logger = ctx.RequestServices.GetRequiredService<ILoggerFactory>().CreateLogger("Orsak.Remoting")
                    logger.LogError(e, "The remote call {Path} failed", ctx.Request.Path)
                    ctx.Response.StatusCode <- StatusCodes.Status500InternalServerError
                    ctx.Response.ContentType <- "text/plain"
                    do! ctx.Response.WriteAsync($"The call to {ctx.Request.Path} failed on the server.")
        })

    /// <summary>
    /// Maps each member of <typeparamref name="'Api"/> to a POST endpoint at
    /// <c>{prefix}/{interface}/{member}</c>, which reads the member's arguments in the format of the request's content
    /// type, calls the member of the implementation <paramref name="create"/> makes for the request, and writes its
    /// result in the same format. A content type none of <paramref name="formats"/> has is answered 415, and an
    /// exception is logged and answered 500, without its details.
    /// </summary>
    /// <remarks>
    /// The members may return a Task, a ValueTask or an Async, of a value or of none. Reflection finds them, so nothing is
    /// generated for the server.
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
            group.MapPost($"/{api.Name}/{m.Name}", RemoteEndpoints.Handle(m, formats, create)) |> ignore

        group
