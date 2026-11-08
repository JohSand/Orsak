namespace Orsak.Remoting.AspNetCore

open System
open System.IO
open System.Reflection
open System.Runtime.CompilerServices
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Routing
open Orsak.Remoting

/// Serves the members of an interface to remote clients: a POST endpoint for each, at {prefix}/{interface}/{member},
/// which reads the arguments in the format of the request's content type, calls the member, and writes its result in
/// the same format. On .NET, reflection finds the members, so nothing is generated for the server.
[<Extension>]
type RemoteEndpoints =
    static member private ResultOf(returnType: Type, returned: obj) : Task<obj * Type> = task {
        match returned with
        | :? Task as t ->
            do! t

            if returnType.IsGenericType then
                let resultType = returnType.GetGenericArguments()[0]
                return returnType.GetProperty("Result").GetValue(returned), resultType
            else
                return box (), typeof<unit>
        | _ -> return returned, returnType
    }

    /// Maps each member of <typeparamref name="'Api"/> to an endpoint, which calls the implementation
    /// <paramref name="create"/> makes for each request, such as one running effects in the request's environment.
    [<Extension>]
    static member MapRemote<'Api>
        (endpoints: IEndpointRouteBuilder, prefix: string, formats: IWireFormat list, create: HttpContext -> 'Api)
        =
        let api = typeof<'Api>

        for m in api.GetMethods() do
            let argTypes = m.GetParameters() |> Array.map (fun p -> p.ParameterType)

            let handle (ctx: HttpContext) : Task = task {
                let contentType = string ctx.Request.ContentType

                match formats |> List.tryFind (fun f -> contentType.StartsWith f.ContentType) with
                | None -> ctx.Response.StatusCode <- StatusCodes.Status415UnsupportedMediaType
                | Some format ->
                    use body = new MemoryStream()
                    do! ctx.Request.Body.CopyToAsync body
                    let args = format.DecodeArguments(body.ToArray(), argTypes)
                    let returned = m.Invoke(create ctx, args)
                    let! value, resultType = RemoteEndpoints.ResultOf(m.ReturnType, returned)
                    ctx.Response.ContentType <- format.ContentType
                    do! ctx.Response.Body.WriteAsync(format.EncodeResult(value, resultType))
            }

            endpoints.MapPost(Routes.ofMember prefix api.Name m.Name, RequestDelegate handle) |> ignore

        endpoints
