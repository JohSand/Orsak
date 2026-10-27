namespace Orsak.AspNetCore


open FSharp.Core.OptimizedClosures
open FSharp.Quotations
open FSharp.Quotations.Patterns
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Routing
open System
//open System.Linq.Expressions
open FastExpressionCompiler.LightExpression
open System.Runtime.CompilerServices
open System.ComponentModel
open System.Reflection
open FSharp.Reflection
open System.Text
open FSharp.Core.Operators.NonStructuralComparison

/// <summary>
/// Endpoint metadata with the method that handles the endpoint's requests, read from the route handler.
/// </summary>
type HandlingMethod = HandlingMethod of MethodInfo

/// <summary>
/// Endpoint metadata with the type of the effect the route handler creates, such as
/// <c>Effect&lt;AppEnv, Order, OrderError&gt;</c>: known for every endpoint, also one handled by a lambda, which has
/// no <see cref="T:Orsak.AspNetCore.HandlingMethod"/>.
/// </summary>
/// <remarks>
/// It is whatever the runner's <c>*>></c> operator takes, so not necessarily an <c>Effect</c>.
/// </remarks>
type EffectType = EffectType of Type

/// <summary>
/// A response an endpoint can give: its status code, the type of its body, if it has one, and what it means. As
/// endpoint metadata, it is an <see cref="T:Microsoft.AspNetCore.Http.Metadata.IProducesResponseTypeMetadata"/>, which
/// ApiExplorer and OpenAPI generators read.
/// </summary>
type ResponseDescription =
    {
        /// <summary>The status code of the response.</summary>
        StatusCode: int
        /// <summary>The type of the response's body, if it has one.</summary>
        BodyType: Type option
        /// <summary>The content types of the body.</summary>
        ContentTypes: string list
        /// <summary>What the response means, such as "No order has the id".</summary>
        Description: string option
    }

    /// <summary>A response without a body.</summary>
    /// <param name="statusCode">The status code of the response</param>
    /// <param name="description">What the response means</param>
    static member Empty(statusCode: int, ?description: string) = {
        StatusCode = statusCode
        BodyType = None
        ContentTypes = []
        Description = description
    }

    /// <summary>A response with a JSON body of type <paramref name="bodyType"/>.</summary>
    /// <param name="statusCode">The status code of the response</param>
    /// <param name="bodyType">The type of the body</param>
    /// <param name="description">What the response means</param>
    static member Json(statusCode: int, bodyType: Type, ?description: string) = {
        StatusCode = statusCode
        BodyType = Some bodyType
        ContentTypes = [ "application/json" ]
        Description = description
    }

    interface Metadata.IProducesResponseTypeMetadata with
        member this.StatusCode = this.StatusCode
        member this.Type = Option.toObj this.BodyType
        member this.ContentTypes = this.ContentTypes

/// <summary>
/// A runner that describes the responses it gives, for OpenAPI: only the runner knows how an effect's result and errors
/// become responses. When the runner of an endpoint implements it, the endpoint gets the responses as metadata.
/// </summary>
/// <example>
/// <code lang="fsharp">
/// type Runner =
///     | RunWith of (HttpContext -> AppEnv)
///
///     static member ( *>> )(effect: Effect&lt;AppEnv, 'a, AppError&gt;, RunWith createEnv) = ...
///
///     interface IDescribeResponses with
///         member _.Responses effectType =
///             match effectType.GetGenericArguments() with
///             | [| _; result; _ |] -> [
///                 ResponseDescription.Json(200, result)
///                 ResponseDescription.Json(404, typeof&lt;ProblemDetails&gt;, "Not found")
///               ]
///             | _ -> []
/// </code>
/// </example>
type IDescribeResponses =
    /// <summary>
    /// The responses of an endpoint whose handler creates an effect of type <paramref name="effectType"/>, such as
    /// <c>Effect&lt;AppEnv, Order, OrderError&gt;</c>.
    /// </summary>
    /// <param name="effectType">The type of the effect, as in <see cref="T:Orsak.AspNetCore.EffectType"/></param>
    abstract Responses: effectType: Type -> ResponseDescription list

/// <summary>
/// Endpoint metadata with the responses the endpoint's runner describes, which the endpoint also has as
/// <see cref="T:Orsak.AspNetCore.ResponseDescription"/>s: so an OpenAPI generator can rank them below the
/// endpoint's own, such as its XML docs.
/// </summary>
type RunnerResponses = RunnerResponses of ResponseDescription list

/// <summary>
/// An endpoint created by one of the <c>Route*</c> methods, such as <c>RouteGet</c>, to be mapped with
/// <c>MapEffectEndpoints</c>. Its methods add conventions, as on ASP.NET Core's endpoint convention builders.
/// </summary>
/// <example>
/// <code lang="fsharp">
/// runner.RouteGet("/users/%i", getUser)
///     .RequiresAuthorization()
///     .WithName("GetUser")
/// </code>
/// </example>
type Endpoint =
    | Endpoint of
        {|
            verb: string
            path: string
            requestDelegate: RequestDelegate
            conventions: IEndpointConventionBuilder -> IEndpointConventionBuilder
        |}

    /// <summary>
    /// Adds a convention to apply to the endpoint when it is mapped.
    /// </summary>
    /// <param name="f">Applies the convention to the endpoint's convention builder</param>
    member inline this.AddConvention([<InlineIfLambda>] f: IEndpointConventionBuilder -> IEndpointConventionBuilder) =
        let (Endpoint this) = this in Endpoint {| this with conventions = fun b -> f (this.conventions b) |}

    /// <summary>Requires authorization with the default policy, as <c>RequireAuthorization</c>.</summary>
    member this.RequiresAuthorization() =
        this.AddConvention(fun a -> a.RequireAuthorization())

    /// <summary>Allows anonymous access, as <c>AllowAnonymous</c>.</summary>
    member this.AllowAnonymous() =
        this.AddConvention(fun a -> a.AllowAnonymous())

    /// <summary>Applies a named CORS policy, as <c>RequireCors</c>.</summary>
    /// <param name="name">The name of the policy</param>
    member this.RequireCors(name: string) =
        this.AddConvention(fun a -> a.RequireCors(name))

    /// <summary>Applies a CORS policy built by <paramref name="builder"/>, as <c>RequireCors</c>.</summary>
    /// <param name="builder">Builds the policy</param>
    member this.RequireCors(builder: Action<_>) =
        this.AddConvention(fun a -> a.RequireCors(builder))

    /// <summary>Names the endpoint, e.g. for link generation, as <c>WithName</c>.</summary>
    /// <param name="name">The name</param>
    member this.WithName(name) =
        this.AddConvention(fun a -> a.WithName(name))

    /// <summary>Adds metadata to the endpoint, as <c>WithMetadata</c>.</summary>
    /// <param name="items">The metadata</param>
    member this.WithMetadata([<ParamArray>] items) =
        this.AddConvention(fun a -> a.WithMetadata(items))

    /// <summary>Sets the endpoint's display name, as <c>WithDisplayName</c>.</summary>
    /// <param name="name">The display name</param>
    member this.WithDisplayName(name: string) =
        this.AddConvention(fun a -> a.WithDisplayName(name))

    /// <summary>Sets the endpoint's display name with a function of its builder, as <c>WithDisplayName</c>.</summary>
    /// <param name="f">Creates the display name</param>
    member this.WithDisplayName(f: Func<_, _>) =
        this.AddConvention(fun a -> a.WithDisplayName(f))

    /// <summary>Sets the endpoint's group name, e.g. for OpenAPI, as <c>WithGroupName</c>.</summary>
    /// <param name="name">The group name</param>
    member this.WithGroupName(name) =
        this.AddConvention(fun a -> a.WithGroupName(name))

    /// <summary>Adds a convention that changes the endpoint builder, as <c>Add</c>.</summary>
    /// <param name="f">Changes the endpoint builder</param>
    member this.Add(f) =
        this.AddConvention(fun b ->
            b.Add(f)
            b)

/// <exclude/>
module Helpers =
    /// The metadata every endpoint gets: the type of its effect, and the responses its runner describes, if it does.
    /// Added before the endpoint's conventions, so explicit metadata comes after it.
    let withEffectMetadata (runner: obj) (effectType: Type) (endpoint: Endpoint) =
        let endpoint = endpoint.WithMetadata(EffectType effectType)

        match runner with
        | :? IDescribeResponses as describer ->
            match describer.Responses effectType with
            | [] -> endpoint
            | responses -> endpoint.WithMetadata([| yield! Seq.map box responses; box (RunnerResponses responses) |])
        | _ -> endpoint

    let private unEscape (s: string) =
        s.Replace("%2F", "/").Replace("%2f", "/")

    /// The parser for a route value, from its format character (%s, %i, ...). It matches the route
    /// constraint AppendParameter gives the same character, and is chosen once per endpoint rather than
    /// looked up from the route pattern for every value of every request.
    let routeValueParser (c: char) : Func<string, obj> =
        match c with
        | 's' -> Func<_, _>(fun s -> box (unEscape s))
        | 'b' -> Func<_, _>(fun s -> box (bool.Parse s))
        | 'c' -> Func<_, _>(fun s -> box (char s))
        | 'i' -> Func<_, _>(fun s -> box (int s))
        | 'd' -> Func<_, _>(fun s -> box (int64 s))
        | 'f' -> Func<_, _>(fun s -> box (float s))
        | 'O' -> Func<_, _>(fun s -> box (Guid s))
        | _ -> failwith $"%c{c} is not a supported route format character."

    /// A parser for each placeholder of a route format such as "/items/%s/%i", in order. As in
    /// AppendPath, %% is a literal %.
    let routeValueParsers (format: string) =
        let parsers = ResizeArray()
        let mutable i = 0

        while i < format.Length - 1 do
            if format[i] = '%' then
                if format[i + 1] <> '%' then
                    parsers.Add(routeValueParser format[i + 1])

                i <- i + 2
            else
                i <- i + 1

        parsers.ToArray()

    let readRouteValue (parser: Func<string, obj>) (name: string) (ctx: HttpContext) =
        parser.Invoke(ctx.GetRouteValue(name) :?> string)


    type StringBuilder with

        member sb.AppendParameter(c, name) =
            match c with
            | 'b' -> sb.Append($"{{%s{name}:bool}}")
            | 'c' -> sb.Append($"{{%s{name}:length(1)}}")
            | 's' -> sb.Append($"{{%s{name}}}")
            | 'i' -> sb.Append($"{{%s{name}:int}}")
            | 'd' -> sb.Append($"{{%s{name}:long}}")
            | 'f' -> sb.Append($"{{%s{name}:double}}")
            | 'O' -> sb.Append($"{{%s{name}:guid}}")
            | _ -> failwith $"%c{c} is not a supported route format character."

        [<TailCall>]
        member sb.AppendPath(chars: char ReadOnlySpan, names: string ReadOnlySpan) =
            let paramIndex = chars.IndexOf('%')

            if paramIndex = -1 then
                sb.Append(chars).ToString()
            elif chars[paramIndex + 1] = '%' then
                sb.Append(chars.Slice(0, paramIndex)).Append('%').AppendPath(chars.Slice(paramIndex + 2), names)
            else
                sb
                    .Append(chars.Slice(0, paramIndex))
                    .AppendParameter(chars[paramIndex + 1], names[0])
                    .AppendPath(chars.Slice(paramIndex + 2), names.Slice(1))

    let createCtorFunc<'T> () =
        let ctorInfo = typeof<'T>.GetConstructors()[0]
        let args = Expression.Parameter(typeof<obj array>, "args")

        let (ctorArgs: Expression array) =
            ctorInfo.GetParameters()
            |> Array.mapi (fun i pinfo ->
                Expression.Convert(Expression.ArrayIndex(args, Expression.Constant(i)), pinfo.ParameterType))

        Expression.Lambda(typeof<Func<obj array, 'T>>, Expression.New(ctorInfo, ctorArgs), args).CompileFast()
        :?> Func<obj array, 'T>

    let inline createEndpointDelegate (eff: 'T -> 'A) (names: string[]) (parsers: Func<string, obj>[]) this =
        //type tests for all primitives we support
        if
            typeof<'T> = typeof<int>
            || typeof<'T> = typeof<bool>
            || typeof<'T> = typeof<char>
            || typeof<'T> = typeof<string>
            || typeof<'T> = typeof<int64>
            || typeof<'T> = typeof<float>
            || typeof<'T> = typeof<Guid>
        then
            RequestDelegate(fun ctx ->
                let arg = readRouteValue parsers[0] names[0] ctx :?> 'T
                let (a: RequestDelegate) = eff arg *>> this in
                a.Invoke(ctx))
        //if not a single value, it is a tuple
        else
            //tupled types
            let activator = createCtorFunc<'T>()
            //we avoid paying the cost by creating this outside the request delegate
            RequestDelegate(fun ctx ->
                let argArray = Array.zeroCreate names.Length

                for i = 0 to names.Length - 1 do
                    argArray[i] <- readRouteValue parsers[i] names[i] ctx

                eff (activator.Invoke argArray) *>> this |> fun x -> x.Invoke(ctx))


open Helpers

/// <summary>
/// Creates endpoints from effects, with a route format string whose values are parsed and passed to the handler, and
/// maps them with <c>MapEffectEndpoints</c>.
/// </summary>
/// <remarks>
/// The methods extend a "runner" of your own: any value whose type has a <c>*>></c> operator that turns a handler's
/// effect into a <see cref="T:Microsoft.AspNetCore.Http.RequestDelegate"/>. The runner decides how the environment is
/// created for each request, and how the effect's result and errors become a response.
/// </remarks>
/// <example>
/// <code lang="fsharp">
/// type Runner =
///     | RunWith of (HttpContext -> AppEnv)
///
///     static member ( *>> )(effect: Effect&lt;AppEnv, IResult, AppError&gt;, RunWith createEnv) =
///         RequestDelegate(fun ctx -> task {
///             match! Effect.run (createEnv ctx) effect with
///             | Ok result -> do! result.ExecuteAsync ctx
///             | Error err -> ctx.Response.StatusCode &lt;- 500
///         })
///
/// let getUser (id: int) = eff {
///     let! user = Users.load id
///     return Results.Ok user
/// }
///
/// app.UseRouting().UseEndpoints(fun endpoints ->
///     let runner = RunWith createEnv
///     endpoints.MapEffectEndpoints [
///         runner.RouteGet("/users/%i", getUser)
///     ])
/// </code>
/// </example>
[<Extension>]
type EffectRunnerExtensions =
    /// <exclude/>
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline CreateEndpoint<'H, 'Eff, 'Printer, 'T
        when ('Eff or 'H): (static member ( *>> ): 'Eff * 'H -> RequestDelegate)>
        (this: 'H, path: PrintfFormat<'Printer, unit, unit, 'Eff, 'T>, verb: string, handler: (Expr<'T -> 'Eff>))
        =
        let eff, shape = ReflectedFunction.capture handler

        let endpoint =
            if typeof<'T> = typeof<unit> then
                Endpoint {|
                    verb = verb
                    path = path.ToString().Replace("%%", "%")
                    requestDelegate = eff (Unchecked.defaultof<'T>) *>> this
                    conventions = id
                |}
            else
                let names = shape.ParameterNames

                Endpoint {|
                    verb = verb
                    path = StringBuilder().AppendPath(path.Value, names)
                    requestDelegate = createEndpointDelegate eff names (routeValueParsers path.Value) this
                    conventions = id
                |}

        let endpoint = withEffectMetadata (box this) typeof<'Eff> endpoint

        match shape.Method with
        | Some mi -> endpoint.WithMetadata(HandlingMethod mi)
        | None -> endpoint


    /// <summary>
    /// Creates a GET endpoint for <paramref name="path"/>, handled by <paramref name="routeHandler"/>.
    /// </summary>
    /// <remarks>
    /// The route values are written as format specifiers, which also constrain the route: <c>%s</c> for a
    /// <c>string</c>, <c>%i</c> for an <c>int</c>, <c>%d</c> for an <c>int64</c>, <c>%b</c> for a <c>bool</c>,
    /// <c>%c</c> for a <c>char</c>, <c>%f</c> for a <c>float</c> and <c>%O</c> for a <c>Guid</c>; <c>%%</c> is a
    /// literal <c>%</c>. The handler takes the values as a tuple, or a single value, or <c>unit</c> for a route without
    /// values, and the names of its parameters name the route values, so pass a function or a lambda directly.
    /// </remarks>
    /// <param name="this">The runner, which turns the handler's effect into a request delegate</param>
    /// <param name="path">The route, with format specifiers for the route values</param>
    /// <param name="routeHandler">Creates the effect that handles a request, from the route values</param>
    /// <example>
    /// <code lang="fsharp">
    /// let getOrderLine (orderId: int, line: int) = eff { ... }
    ///
    /// // maps GET /orders/{orderId:int}/lines/{line:int}
    /// runner.RouteGet("/orders/%i/lines/%i", getOrderLine)
    /// </code>
    /// </example>
    [<Extension>]
    static member inline RouteGet
        (
            this: 'H,
            path: PrintfFormat<'Printer, unit, unit, ^Eff, 'T>,
            [<ReflectedDefinition(includeValue = true)>] routeHandler
        ) =
        EffectRunnerExtensions.CreateEndpoint<'H, 'Eff, 'Printer, 'T>(this, path, HttpMethods.Get, routeHandler)

    /// <summary>
    /// Creates a POST endpoint for <paramref name="path"/>, handled by <paramref name="routeHandler"/>. The route
    /// values work as for <c>RouteGet</c>.
    /// </summary>
    /// <param name="this">The runner, which turns the handler's effect into a request delegate</param>
    /// <param name="path">The route, with format specifiers for the route values</param>
    /// <param name="routeHandler">Creates the effect that handles a request, from the route values</param>
    [<Extension>]
    static member inline RoutePost(this, path, [<ReflectedDefinition(includeValue = true)>] routeHandler) =
        EffectRunnerExtensions.CreateEndpoint(this, path, HttpMethods.Post, routeHandler)

    /// <summary>
    /// Creates a PUT endpoint for <paramref name="path"/>, handled by <paramref name="routeHandler"/>. The route
    /// values work as for <c>RouteGet</c>.
    /// </summary>
    /// <param name="this">The runner, which turns the handler's effect into a request delegate</param>
    /// <param name="path">The route, with format specifiers for the route values</param>
    /// <param name="routeHandler">Creates the effect that handles a request, from the route values</param>
    [<Extension>]
    static member inline RoutePut(this, path, [<ReflectedDefinition(includeValue = true)>] routeHandler) =
        EffectRunnerExtensions.CreateEndpoint(this, path, HttpMethods.Put, routeHandler)

    /// <summary>
    /// Creates a PATCH endpoint for <paramref name="path"/>, handled by <paramref name="routeHandler"/>. The route
    /// values work as for <c>RouteGet</c>.
    /// </summary>
    /// <param name="this">The runner, which turns the handler's effect into a request delegate</param>
    /// <param name="path">The route, with format specifiers for the route values</param>
    /// <param name="routeHandler">Creates the effect that handles a request, from the route values</param>
    [<Extension>]
    static member inline RoutePatch(this, path, [<ReflectedDefinition(includeValue = true)>] routeHandler) =
        EffectRunnerExtensions.CreateEndpoint(this, path, HttpMethods.Patch, routeHandler)

    /// <summary>
    /// Creates a DELETE endpoint for <paramref name="path"/>, handled by <paramref name="routeHandler"/>. The route
    /// values work as for <c>RouteGet</c>.
    /// </summary>
    /// <param name="this">The runner, which turns the handler's effect into a request delegate</param>
    /// <param name="path">The route, with format specifiers for the route values</param>
    /// <param name="routeHandler">Creates the effect that handles a request, from the route values</param>
    [<Extension>]
    static member inline RouteDelete(this, path, [<ReflectedDefinition(includeValue = true)>] routeHandler) =
        EffectRunnerExtensions.CreateEndpoint(this, path, HttpMethods.Delete, routeHandler)

    /// <summary>
    /// Creates a HEAD endpoint for <paramref name="path"/>, handled by <paramref name="routeHandler"/>. The route
    /// values work as for <c>RouteGet</c>.
    /// </summary>
    /// <param name="this">The runner, which turns the handler's effect into a request delegate</param>
    /// <param name="path">The route, with format specifiers for the route values</param>
    /// <param name="routeHandler">Creates the effect that handles a request, from the route values</param>
    [<Extension>]
    static member inline RouteHead(this, path, [<ReflectedDefinition(includeValue = true)>] routeHandler) =
        EffectRunnerExtensions.CreateEndpoint(this, path, HttpMethods.Head, routeHandler)

    /// <summary>
    /// Creates a TRACE endpoint for <paramref name="path"/>, handled by <paramref name="routeHandler"/>. The route
    /// values work as for <c>RouteGet</c>.
    /// </summary>
    /// <param name="this">The runner, which turns the handler's effect into a request delegate</param>
    /// <param name="path">The route, with format specifiers for the route values</param>
    /// <param name="routeHandler">Creates the effect that handles a request, from the route values</param>
    [<Extension>]
    static member inline RouteTrace(this, path, [<ReflectedDefinition(includeValue = true)>] routeHandler) =
        EffectRunnerExtensions.CreateEndpoint(this, path, HttpMethods.Trace, routeHandler)

    /// <summary>
    /// Creates a CONNECT endpoint for <paramref name="path"/>, handled by <paramref name="routeHandler"/>. The route
    /// values work as for <c>RouteGet</c>.
    /// </summary>
    /// <param name="this">The runner, which turns the handler's effect into a request delegate</param>
    /// <param name="path">The route, with format specifiers for the route values</param>
    /// <param name="routeHandler">Creates the effect that handles a request, from the route values</param>
    [<Extension>]
    static member inline RouteConnect(this, path, [<ReflectedDefinition(includeValue = true)>] routeHandler) =
        EffectRunnerExtensions.CreateEndpoint(this, path, HttpMethods.Connect, routeHandler)

    /// <summary>
    /// Creates a OPTIONS endpoint for <paramref name="path"/>, handled by <paramref name="routeHandler"/>. The route
    /// values work as for <c>RouteGet</c>.
    /// </summary>
    /// <param name="this">The runner, which turns the handler's effect into a request delegate</param>
    /// <param name="path">The route, with format specifiers for the route values</param>
    /// <param name="routeHandler">Creates the effect that handles a request, from the route values</param>
    [<Extension>]
    static member inline RouteOptions(this, path, [<ReflectedDefinition(includeValue = true)>] routeHandler) =
        EffectRunnerExtensions.CreateEndpoint(this, path, HttpMethods.Options, routeHandler)

    /// <summary>
    /// Maps the endpoints, with their conventions, and instruments each with <c>EffectDiagnostics</c>.
    /// </summary>
    /// <param name="builder">The endpoint route builder, e.g. in <c>UseEndpoints</c></param>
    /// <param name="endpoints">The endpoints to map</param>
    /// <example>
    /// <code lang="fsharp">
    /// app.UseRouting().UseEndpoints(fun endpoints ->
    ///     endpoints.MapEffectEndpoints [
    ///         runner.RouteGet("/users/%i", getUser)
    ///         runner.RoutePost("/users", createUser)
    ///     ])
    /// </code>
    /// </example>
    [<Extension>]
    static member inline MapEffectEndpoints(builder: IEndpointRouteBuilder, endpoints: Endpoint list) =
        endpoints
        |> List.iter (fun (Endpoint e) ->
            let convBuilder = builder.MapMethods(e.path, [| e.verb |], EffectDiagnostics.instrument e.path e.verb e.requestDelegate)
            e.conventions convBuilder |> ignore)
