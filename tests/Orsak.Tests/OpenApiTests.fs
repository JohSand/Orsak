/// The OpenAPI documents of effect endpoints, with Orsak.AspNetCore.OpenApi.
module Orsak.Tests.OpenApiTests

open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Mvc
open Microsoft.AspNetCore.OpenApi
open Microsoft.Extensions.DependencyInjection
open Microsoft.OpenApi
open Orsak
open Orsak.AspNetCore
open Orsak.AspNetCore.OpenApi
open Orsak.AspNetCore.V2
open Orsak.Tests.OpenApiInferenceTests
open VerifyXunit
open Xunit

/// A runner that describes its responses: the effect's result as a 200, and its errors as problem details.
type DescribingRunner =
    | Describing

    static member inline ( *>> )(_: Effect<unit, 'a, Orders.OrderError>, Describing) : RequestDelegate =
        RequestDelegate(fun _ -> Task.CompletedTask)

    interface IDescribeResponses with
        member _.Responses effectType =
            match effectType.GetGenericArguments() with
            | [| _; result; _ |] -> [
                ResponseDescription.Json(200, result)
                ResponseDescription.Json(404, typeof<ProblemDetails>, "Not found")
              ]
            | _ -> []

/// The document named "v1" of an app mapping `endpoints`, as JSON.
let documentOf (configure: OpenApiOptions -> unit) (endpoints: Orsak.AspNetCore.Endpoint list) = task {
    let builder = WebApplication.CreateSlimBuilder()
    builder.Services.AddOpenApi(fun options -> configure options) |> ignore
    let app = builder.Build()
    // UseEndpoints registers the endpoints with the routing services, which a started app does too
    app.UseRouting().UseEndpoints(fun e -> e.MapEffectEndpoints endpoints) |> ignore

    let provider = app.Services.GetRequiredKeyedService<IOpenApiDocumentProvider>("v1")
    let! document = provider.GetOpenApiDocumentAsync()
    return! document.SerializeAsJsonAsync(OpenApiSpecVersion.OpenApi3_1)
}

[<Fact>]
let ``the generator doesn't describe effect endpoints by itself`` () = task {
    let! json = documentOf ignore (endpoints ())
    Assert.DoesNotContain("/orders", json)
}

[<Fact>]
let ``AddEffectEndpoints describes effect endpoints, from their routes, handlers, docs and metadata`` () = task {
    let explicitly =
        Describing
            .RouteGet2("/v2/orders/%i", Orders.getOrder)
            .WithMetadata(ResponseDescription.Empty(409, "The order is locked"))
            .AddConvention(fun b -> b.WithSummary("Gets an order, described explicitly").WithTags("Explicit"))

    let excluded =
        Describing.RouteGet2("/hidden", Orders.ping).AddConvention(fun b -> b.ExcludeFromDescription())

    let! json =
        documentOf (fun o -> o.AddEffectEndpoints() |> ignore) [
            yield! endpoints ()
            Describing.RouteGet2("/v2/customers/%O/orders", Orders.listOrders)
            explicitly
            excluded
        ]

    let! _ = Verifier.Verify(json, extension = "json")
    ()
}

/// A record of the F# types the schema transformer describes.
type Shapes = {
    Name: string
    Nickname: string option
    Age: int option
    Height: float voption
    Line: Orders.OrderLine option
    Line2: Orders.OrderLine
    Tags: string list
    Scores: int array
    Labels: Set<string>
    Counts: Map<string, int>
}

let getShapes () : Effect<unit, Shapes, Orders.OrderError> =
    eff { return Unchecked.defaultof<Shapes> }

[<Fact>]
let ``AddFSharpSchemas describes options, lists, sets and maps as System.Text.Json writes them`` () = task {
    let! json =
        documentOf (fun o -> o.AddEffectEndpoints().AddFSharpSchemas() |> ignore) [
            Describing.RouteGet2("/shapes", getShapes)
        ]

    let! _ = Verifier.Verify(json, extension = "json")
    ()
}
