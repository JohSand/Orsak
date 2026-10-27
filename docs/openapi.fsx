(**
---
title: OpenAPI
category: Guides
categoryindex: 3
index: 5
---
*)
(*** condition: prepare ***)
#load ".generated/aspnetcore.fsx"
#r "nuget: Microsoft.AspNetCore.OpenApi, 10.0.14"
#r "../src/Orsak/bin/Release/net8.0/Orsak.dll"
#r "../src/Orsak.AspNetCore/bin/Release/net8.0/Orsak.AspNetCore.dll"
#r "../src/Orsak.AspNetCore.OpenApi/bin/Release/net10.0/Orsak.AspNetCore.OpenApi.dll"
(**
# OpenAPI

ASP.NET Core's OpenAPI generator, Microsoft.AspNetCore.OpenApi, describes minimal API endpoints from the parameters and
return type of their handler. Endpoints mapped with Orsak.AspNetCore's `MapEffectEndpoints` have a request delegate
instead, so the generator leaves them out. Orsak.AspNetCore.OpenApi adds them, describing each from what can be read
from it: its route, the function handling it, that function's XML docs, the type of its effect, and the responses its
runner describes. Explicit metadata, such as `WithName` or `Produces`, comes first.

It needs .NET 10, and Microsoft.AspNetCore.OpenApi 10.0.14 or later.

This page is a script: it runs with `dotnet fsi docs/openapi.fsx` once Orsak is built, after
`dotnet fsi docs/.tools/aspnetcore-references.fsx`, as for [Web APIs](web-apis.html), and the output shown is from such
a run.

## Setting up

Reference `Orsak.AspNetCore.OpenApi`, and add the effect endpoints to the document with `AddEffectEndpoints`.
`AddFSharpSchemas` is optional, and describes F# types [as System.Text.Json writes them](#F-types):

```fsharp
builder.Services.AddOpenApi(fun options -> options.AddEffectEndpoints().AddFSharpSchemas() |> ignore)

app.MapOpenApi()
```

The endpoints are found through ASP.NET Core's routing, which knows of them once `UseEndpoints` has registered them,
as a started application does.

## An API to describe

The API from [Web APIs](web-apis.html) looks orders up. Its handlers return the order itself, so the type of their
effect tells what a successful response holds:
*)
open System
open System.Text.Json.Nodes
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Mvc
open Microsoft.AspNetCore.OpenApi
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.OpenApi
open Orsak
open Orsak.AspNetCore
open Orsak.AspNetCore.OpenApi
open Orsak.AspNetCore.V2

type OrderLine = { Product: string; Quantity: int }

type Order = {
    Id: int
    Lines: OrderLine list
    Note: string option
}

type OrderError =
    | NotFound of id: int
    | Invalid of reason: string

let getOrder (id: int) : Effect<unit, Order, OrderError> = eff {
    if id = 1 then
        return { Id = 1; Lines = [ { Product = "mug"; Quantity = 2 } ]; Note = None }
    else
        return! Error(NotFound id)
}

let getOrderLine (id: int) (line: int) : Effect<unit, OrderLine, OrderError> = eff {
    let! order = getOrder id

    match List.tryItem line order.Lines with
    | Some l -> return l
    | None -> return! Error(NotFound id)
}

(**
## The runner's responses

Only the runner knows how an effect's result and errors become responses. A runner that implements
`IDescribeResponses` says so, given the type of each endpoint's effect. This one writes the result as JSON, and the
errors as problem details:
*)
type Runner =
    | Runner

    static member inline ( *>> )(effect: Effect<unit, 'a, OrderError>, Runner) =
        RequestDelegate(fun ctx -> task {
            let! response =
                effect
                |> Effect.map Results.Ok
                |> Effect.run ()

            let response =
                match response with
                | Ok response -> response
                | Error(NotFound _) -> Results.Problem(statusCode = 404)
                | Error(Invalid reason) -> Results.Problem(reason, statusCode = 400)

            return! response.ExecuteAsync ctx
        })

    interface IDescribeResponses with
        member _.Responses effectType =
            match effectType.GetGenericArguments() with
            | [| _; result; _ |] -> [
                ResponseDescription.Json(200, result)
                ResponseDescription.Json(400, typeof<ProblemDetails>, "The request is invalid")
                ResponseDescription.Json(404, typeof<ProblemDetails>, "Not found")
              ]
            | _ -> []

(**
Each endpoint gets the responses as metadata, a `ResponseDescription` each, which is also an
`IProducesResponseTypeMetadata` for other tools that read ApiExplorer's metadata.

`openApiOf` maps endpoints, and returns the document the generator creates, as JSON:
*)
let openApiOf (configure: OpenApiOptions -> OpenApiOptions) (endpoints: Orsak.AspNetCore.Endpoint list) =
    let builder = WebApplication.CreateSlimBuilder()
    builder.Logging.ClearProviders() |> ignore
    builder.Services.AddOpenApi(fun options -> configure options |> ignore) |> ignore
    let app = builder.Build()
    app.UseRouting().UseEndpoints(fun e -> e.MapEffectEndpoints endpoints) |> ignore

    let provider = app.Services.GetRequiredKeyedService<IOpenApiDocumentProvider>("v1")
    let document = provider.GetOpenApiDocumentAsync().Result
    document.SerializeAsJsonAsync(OpenApiSpecVersion.OpenApi3_1).Result |> JsonNode.Parse

let show (node: JsonNode) = printfn "%s" (node.ToJsonString(Text.Json.JsonSerializerOptions(WriteIndented = true)))

/// The node at the end of `keys`, such as the path item of a route.
let at (keys: string list) (node: JsonNode) = keys |> List.fold (fun (n: JsonNode) key -> n[key]) node

let endpoints = [
    Runner.RouteGet2("/orders/%i", getOrder)
    Runner.RouteGet2("/orders/%i/lines/%i", getOrderLine)
]

(**
## What is inferred

With nothing written for OpenAPI, each endpoint is an operation named after its handler, tagged with the handler's
module, with its route values as path parameters, typed by their format specifiers, and the responses of its runner:
*)
let document = openApiOf (fun o -> o.AddEffectEndpoints()) endpoints
document |> at [ "paths"; "/orders/{id}/lines/{line}" ] |> show
(**
```text
{
  "get": {
    "tags": [
      "FSI_0003"
    ],
    "operationId": "getOrderLine",
    "parameters": [
      {
        "name": "id",
        "in": "path",
        "required": true,
        "schema": {
          "type": "integer",
          "format": "int32"
        }
      },
      {
        "name": "line",
        "in": "path",
        "required": true,
        "schema": {
          "type": "integer",
          "format": "int32"
        }
      }
    ],
    "responses": {
      "200": {
        "description": "OK",
        "content": {
          "application/json": {
            "schema": {
              "$ref": "#/components/schemas/OrderLine"
            }
          }
        }
      },
      "400": {
        "description": "The request is invalid",
        "content": {
          "application/json": {
            "schema": {
              "$ref": "#/components/schemas/ProblemDetails"
            }
          }
        }
      },
      "404": {
        "description": "Not found",
        "content": {
          "application/json": {
            "schema": {
              "$ref": "#/components/schemas/ProblemDetails"
            }
          }
        }
      }
    }
  }
}
```

The tag is the module of `getOrderLine`, which in a script is one F# Interactive names. The schemas are components of
the document, named as the generator names its own, by `OpenApiOptions.CreateSchemaReferenceId`.

Without a runner that describes its responses, an operation has a 200 with the effect's result as its body, unless
the result is `unit` or an `IResult`, which tell nothing of the body.

## From the XML docs

The handler's XML docs describe the operation, when the project writes them, with `GenerateDocumentationFile`:

```xml
<PropertyGroup>
    <GenerateDocumentationFile>true</GenerateDocumentationFile>
</PropertyGroup>
```

They are read from the `.xml` file next to the handler's assembly. A script has none, so this part is from a project,
the one of Orsak's tests:

```fsharp
/// <summary>Gets an order, with its lines.</summary>
/// <remarks>
/// The lines are in the order they were added. See <see cref="T:Orsak.Tests.OpenApiInferenceTests.Orders.OrderLine"/>.
/// </remarks>
/// <param name="orderId">The id of the order.</param>
/// <returns>The order.</returns>
/// <response code="404">No order has the id.</response>
let getOrder (orderId: int) : Effect<unit, Order, OrderError> = ...
```

The `<summary>` is the operation's summary, the `<remarks>` its description, each `<param>` describes the route value
of the same name, `<returns>` the success response, and each `<response code="...">` the response with that status
code, which it adds if the runner doesn't describe one:

```text
"summary": "Gets an order, with its lines.",
"description": "The lines are in the order they were added. See OrderLine.",
"operationId": "getOrder",
"parameters": [ { "name": "orderId", "in": "path", "description": "The id of the order.", ... } ],
"responses": {
  "200": { "description": "The order.", ... },
  "404": { "description": "No order has the id.", ... }
}
```

F# only reads a doc comment as XML when it starts with a tag. A comment of plain text is its summary, tags and all, so
write `<summary>` when the comment has other tags.

## Explicit metadata

Metadata on the endpoint comes before what is inferred, and before the docs. The standard conventions apply, through
`AddConvention`, and a `ResponseDescription` adds a response, or replaces the runner's for its status code:
*)
let described =
    Runner
        .RouteGet2("/orders/%i", getOrder)
        .WithName("GetOrder")
        .WithMetadata(ResponseDescription.Empty(404, "No order has the id"))
        .AddConvention(fun b -> b.WithSummary("Gets an order").WithTags("Orders"))

let explicitDocument = openApiOf (fun o -> o.AddEffectEndpoints()) [ described ]
let operation = explicitDocument |> at [ "paths"; "/orders/{id}"; "get" ]

printfn "%s, %s, %O" (operation["operationId"].ToString()) (operation["summary"].ToString()) (operation["tags"].ToJsonString())

for response in operation["responses"].AsObject() do
    printfn "%s: %s" response.Key (response.Value["description"].ToString())
(**
```text
GetOrder, Gets an order, ["Orders"]
200: OK
400: The request is invalid
404: No order has the id
```

The precedence, for each part of an operation:

| Part | First | Then | Then |
|---|---|---|---|
| Operation id | `WithName` | the handler's name | |
| Tags | `WithTags` | the handler's module | |
| Summary | `WithSummary` | `<summary>` | |
| Description | `WithDescription` | `<remarks>` | |
| Path parameters | | `<param>` describes them | typed by their route constraints |
| Responses | `ResponseDescription`, `Produces` | `<response code="...">` | the runner's, or a 200 of the effect's result |
| Response descriptions | the endpoint's metadata | the docs | the runner's |

A handler's name is only an operation id once, as for a function handling two routes, since operation ids are unique.
An endpoint is in the documents of its group name, set with `WithGroupName`, or in every document without one, and
`ExcludeFromDescription` leaves it out.

## F# types

The generator describes types as System.Text.Json writes them, and knows .NET's types, not F#'s. An option is written
as its value, or `null` for none, but the generator describes it as a component of its own, and requires it in a
record, though a missing option field is read as `None`. An F# list is never `null`, but is described as one that can
be:
*)
document |> at [ "components"; "schemas"; "Order" ] |> show
(**
```text
{
  "required": [
    "id",
    "lines",
    "note"
  ],
  "type": "object",
  "properties": {
    "id": {
      "pattern": "^-?(?:0|[1-9]\\d*)$",
      "type": [
        "integer",
        "string"
      ],
      "format": "int32"
    },
    "lines": {
      "type": [
        "null",
        "array"
      ],
      "items": {
        "$ref": "#/components/schemas/OrderLine"
      }
    },
    "note": {
      "$ref": "#/components/schemas/FSharpOptionOfstring"
    }
  }
}
```

`AddFSharpSchemas` describes options and value options as their value, or `null`, F# lists, sets and maps as never
`null`, and a set's items as unique:
*)
let fsharpDocument = openApiOf (fun o -> o.AddEffectEndpoints().AddFSharpSchemas()) endpoints
fsharpDocument |> at [ "components"; "schemas"; "Order" ] |> show
(**
```text
{
  "required": [
    "id",
    "lines"
  ],
  "type": "object",
  "properties": {
    "id": {
      "pattern": "^-?(?:0|[1-9]\\d*)$",
      "type": [
        "integer",
        "string"
      ],
      "format": "int32"
    },
    "lines": {
      "type": "array",
      "items": {
        "$ref": "#/components/schemas/OrderLine"
      }
    },
    "note": {
      "type": [
        "null",
        "string"
      ]
    }
  }
}
```

An option of a type with a component, such as `OrderLine option`, is one of `null` or the component. Unions aren't
described: System.Text.Json can't write them by itself, and how a converter such as FSharp.SystemTextJson writes them
depends on its options.
*)
