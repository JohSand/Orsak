(**
---
title: Web APIs
category: Guides
categoryindex: 3
index: 4
---
*)
(*** condition: prepare ***)
#load ".generated/aspnetcore.fsx"
#r "nuget: Microsoft.AspNetCore.TestHost, 10.0.0"
#r "nuget: Giraffe, 8.3.0"
#r "nuget: Oxpecker, 2.1.1"
#r "../src/Orsak/bin/Release/net8.0/Orsak.dll"
#r "../src/Orsak.AspNetCore/bin/Release/net8.0/Orsak.AspNetCore.dll"
(**
# Web APIs

Orsak isn't tied to a web framework. An effect needs an environment to run, and a request handler is a natural place
to create one: from the request, and from the services of the application. Whatever framework handles the request
only has to run the effect, and turn its result into a response.

This page builds the same small API with four frameworks: ASP.NET Core's minimal APIs, [Giraffe](https://giraffe.wiki),
[Oxpecker](https://lanayx.github.io/Oxpecker/), and Orsak.AspNetCore. The shared part comes first, and then a section
for each framework, which can be read on its own.

This page is a script: it runs with `dotnet fsi docs/web-apis.fsx` once Orsak is built, and the output shown is from
such a run. Scripts can't reference ASP.NET Core as a package, so it first needs
`dotnet fsi docs/.tools/aspnetcore-references.fsx`, which writes a script referencing the ASP.NET Core installed with
.NET. The page uses ASP.NET Core 10, Giraffe 8.3 and Oxpecker 2.1.

## The shared part

The API creates orders and looks them up. The orders are kept in a store, described in the usual way, and the
effects of the API are written once, for all the frameworks:
*)
open System
open System.Collections.Concurrent
open System.Net.Http
open System.Net.Http.Json
open System.Threading
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.TestHost
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Orsak

type Order = { Id: int; Product: string; Quantity: int }

type NewOrder = { Product: string; Quantity: int }

type OrderError =
    | NotFound of id: int
    | Invalid of reason: string

type IOrderStore =
    abstract member Get: id: int -> Task<Order option>
    abstract member Add: NewOrder -> Task<Order>

type IOrderStoreProvider =
    abstract member Orders: IOrderStore

module OrderStore =
    let get id =
        Effect.Create(fun (p: #IOrderStoreProvider) -> p.Orders.Get id)

    let add order =
        Effect.Create(fun (p: #IOrderStoreProvider) -> p.Orders.Add order)

let getOrder id = eff {
    match! OrderStore.get id with
    | Some order -> return order
    | None -> return! Error(NotFound id)
}

let createOrder (order: NewOrder) = eff {
    if order.Quantity <= 0 then
        return! Error(Invalid "the quantity must be positive")
    else
        return! OrderStore.add order
}

(**
The environment is created for each request. It has the store, which the application registers as a singleton
service, and the request's `HttpContext`, for effects that need the request:
*)
type IRequestProvider =
    abstract member Context: HttpContext

type AppEnv(context: HttpContext, orders: IOrderStore) =
    interface IRequestProvider with
        member _.Context = context

    interface IOrderStoreProvider with
        member _.Orders = orders

let createEnv (ctx: HttpContext) =
    AppEnv(ctx, ctx.RequestServices.GetRequiredService<IOrderStore>())

(**
The store keeps the orders in memory:
*)
type InMemoryOrderStore() =
    let orders = ConcurrentDictionary<int, Order>()
    let mutable lastId = 0

    interface IOrderStore with
        member _.Get id =
            Task.FromResult(
                match orders.TryGetValue id with
                | true, order -> Some order
                | _ -> None
            )

        member _.Add order =
            let id = Interlocked.Increment &lastId
            let created = { Id = id; Product = order.Product; Quantity = order.Quantity }
            orders[id] <- created
            Task.FromResult created

(**
To try each version of the API, `tryApi` starts it on a test server from Microsoft.AspNetCore.TestHost, which handles
requests in memory, without a port. It registers the store, lets the framework add its services and map its
routes, and then sends the same requests to each version:
*)
let tryApi (addServices: IServiceCollection -> unit) (mapRoutes: WebApplication -> unit) =
    let builder = WebApplication.CreateBuilder()
    builder.WebHost.UseTestServer() |> ignore
    builder.Logging.ClearProviders() |> ignore
    builder.Services.AddSingleton<IOrderStore, InMemoryOrderStore>() |> ignore
    addServices builder.Services
    use app = builder.Build()
    mapRoutes app
    app.StartAsync().Wait()
    use client = app.GetTestClient()

    let show (request: string) (response: HttpResponseMessage) =
        let body = response.Content.ReadAsStringAsync().Result
        printfn $"%-50s{request} -> %d{int response.StatusCode} {body}"

    client.GetAsync("/orders/1").Result |> show "GET /orders/1"

    client.PostAsJsonAsync("/orders", { Product = "mug"; Quantity = 2 }).Result
    |> show "POST /orders {\"product\":\"mug\",\"quantity\":2}"

    client.GetAsync("/orders/1").Result |> show "GET /orders/1"

    client.PostAsJsonAsync("/orders", { Product = "mug"; Quantity = 0 }).Result
    |> show "POST /orders {\"product\":\"mug\",\"quantity\":0}"

    app.StopAsync().Wait()

(**
Each version responds the same way: `NotFound` becomes a 404, and `Invalid` a 400 with the reason.

## Minimal APIs

A minimal API handler takes the route values, the body and the `HttpContext` as parameters, and returns an `IResult`.
The handler creates the environment, runs the effect, and turns its result into an `IResult`, with a function mapping
the errors:
*)
module MinimalApi =
    let toResult ok (result: Result<'a, OrderError>) =
        match result with
        | Ok value -> ok value
        | Error(NotFound _) -> Results.NotFound()
        | Error(Invalid reason) -> Results.BadRequest reason

    let mapOrders (app: WebApplication) =
        app.MapGet(
            "/orders/{id:int}",
            Func<int, HttpContext, Task<IResult>>(fun id ctx -> task {
                let! result = getOrder id |> Effect.run (createEnv ctx)
                return result |> toResult Results.Ok
            })
        )
        |> ignore

        app.MapPost(
            "/orders",
            Func<NewOrder, HttpContext, Task<IResult>>(fun order ctx -> task {
                let! result = createOrder order |> Effect.run (createEnv ctx)
                return result |> toResult (fun created -> Results.Created($"/orders/{created.Id}", created))
            })
        )
        |> ignore

(**
F# lambdas aren't converted to `Delegate` on their own, so each handler is given as a `Func`, whose parameter types
tell ASP.NET Core where to bind each value from. Minimal APIs need no services of their own:
*)
tryApi ignore MinimalApi.mapOrders
(**
```text
GET /orders/1                                      -> 404
POST /orders {"product":"mug","quantity":2}        -> 201 {"id":1,"product":"mug","quantity":2}
GET /orders/1                                      -> 200 {"id":1,"product":"mug","quantity":2}
POST /orders {"product":"mug","quantity":0}        -> 400 "the quantity must be positive"
```

## Giraffe

A Giraffe `HttpHandler` can run an effect like any other asynchronous work. A function turning an effect into a
handler, given a handler for its value, keeps the routes short:
*)
module GiraffeApi =
    open Giraffe

    let handleEffect (ok: 'a -> HttpHandler) (effect: Effect<AppEnv, 'a, OrderError>) : HttpHandler =
        fun next ctx -> task {
            match! effect |> Effect.run (createEnv ctx) with
            | Ok value -> return! ok value next ctx
            | Error(NotFound _) -> return! RequestErrors.NOT_FOUND "" next ctx
            | Error(Invalid reason) -> return! RequestErrors.BAD_REQUEST reason next ctx
        }

    let webApp: HttpHandler =
        choose [
            GET >=> routef "/orders/%i" (getOrder >> handleEffect json)
            POST >=> route "/orders" >=> bindJson<NewOrder> (createOrder >> handleEffect Successful.CREATED)
        ]

    let addServices (services: IServiceCollection) = services.AddGiraffe() |> ignore
    let mapRoutes (app: WebApplication) = app.UseGiraffe webApp

(**
`routef` and `bindJson` pass the route value and the body to a function, so composing it with `handleEffect` is
enough:
*)
tryApi GiraffeApi.addServices GiraffeApi.mapRoutes
(**
```text
GET /orders/1                                      -> 404 ""
POST /orders {"product":"mug","quantity":2}        -> 201 {"id":1,"product":"mug","quantity":2}
GET /orders/1                                      -> 200 {"id":1,"product":"mug","quantity":2}
POST /orders {"product":"mug","quantity":0}        -> 400 "the quantity must be positive"
```

## Oxpecker

Oxpecker's `EndpointHandler` takes the `HttpContext` and returns a `Task`, and the same approach works:
*)
module OxpeckerApi =
    open Oxpecker

    let handleEffect (ok: 'a -> EndpointHandler) (effect: Effect<AppEnv, 'a, OrderError>) : EndpointHandler =
        fun ctx -> task {
            match! effect |> Effect.run (createEnv ctx) with
            | Ok value -> return! ok value ctx
            | Error(NotFound _) -> ctx.SetStatusCode 404
            | Error(Invalid reason) ->
                ctx.SetStatusCode 400
                return! ctx.WriteText reason
        }

    let created (order: Order) : EndpointHandler =
        fun ctx ->
            ctx.SetStatusCode 201
            ctx.WriteJson order

    let endpoints = [
        GET [ routef "/orders/{%i}" (getOrder >> handleEffect json) ]
        POST [ route "/orders" (bindJson<NewOrder> (createOrder >> handleEffect created)) ]
    ]

    let addServices (services: IServiceCollection) =
        services.AddRouting().AddOxpecker() |> ignore

    let mapRoutes (app: WebApplication) =
        app.UseRouting().UseOxpecker(endpoints) |> ignore

(**
Oxpecker writes the reason for the 400 as plain text, where the others write it as JSON:
*)
tryApi OxpeckerApi.addServices OxpeckerApi.mapRoutes
(**
```text
GET /orders/1                                      -> 404
POST /orders {"product":"mug","quantity":2}        -> 201 {"id":1,"product":"mug","quantity":2}
GET /orders/1                                      -> 200 {"id":1,"product":"mug","quantity":2}
POST /orders {"product":"mug","quantity":0}        -> 400 the quantity must be positive
```

## Orsak.AspNetCore

Orsak.AspNetCore maps endpoints whose handlers return effects. How an effect becomes a response is up to the
application, which writes it once, as a `*>>` operator on a runner type. The runner here creates the environment,
runs the effect, and executes the `IResult` it returns, or the response for its error:
*)
module OrsakApi =
    open Orsak.AspNetCore

    type Runner =
        | RunWith of (HttpContext -> AppEnv)

        static member ( *>> )(effect: Effect<AppEnv, IResult, OrderError>, RunWith createEnv) =
            RequestDelegate(fun ctx -> task {
                let! result = effect |> Effect.run (createEnv ctx)

                let response =
                    match result with
                    | Ok response -> response
                    | Error(NotFound _) -> Results.NotFound()
                    | Error(Invalid reason) -> Results.BadRequest reason

                return! response.ExecuteAsync ctx
            })

(**
The handlers are effects too. Route values are written as format specifiers, such as `%i` for an `int`, and named
after the handler's parameters, so `/orders/%i` with `getOrderEndpoint` maps `/orders/{id:int}`. The body is read by
an effect, from the request in the environment:
*)
    module Request =
        let readJson () : Effect<#IRequestProvider, 'T, _> =
            Effect.Create(fun (p: #IRequestProvider) -> p.Context.Request.ReadFromJsonAsync<'T>())

    let getOrderEndpoint (id: int) = eff {
        let! order = getOrder id
        return Results.Ok order
    }

    let postOrderEndpoint () = eff {
        let! (order: NewOrder) = Request.readJson ()
        let! created = createOrder order
        return Results.Created($"/orders/{created.Id}", created)
    }

    let mapRoutes (app: WebApplication) =
        app.UseRouting().UseEndpoints(fun endpoints ->
            let runner = RunWith createEnv

            endpoints.MapEffectEndpoints [
                runner.RouteGet("/orders/%i", getOrderEndpoint)
                runner.RoutePost("/orders", postOrderEndpoint)
            ])
        |> ignore

(**
The handlers are passed as functions, not called, as their parameter names are read from them. Each endpoint is also
instrumented with Orsak's diagnostics, and takes conventions such as `.RequiresAuthorization()` and `.WithName(...)`.
Orsak.AspNetCore needs no services of its own:
*)
tryApi ignore OrsakApi.mapRoutes
(**
```text
GET /orders/1                                      -> 404
POST /orders {"product":"mug","quantity":2}        -> 201 {"id":1,"product":"mug","quantity":2}
GET /orders/1                                      -> 200 {"id":1,"product":"mug","quantity":2}
POST /orders {"product":"mug","quantity":0}        -> 400 "the quantity must be positive"
```
*)
