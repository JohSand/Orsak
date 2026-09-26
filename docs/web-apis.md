---
title: Web APIs
category: Guides
categoryindex: 3
index: 4
---

# Web APIs

Orsak isn't tied to a web framework. An effect needs an environment to run, and a request handler is a natural place
to create one: from the request, and from the services of the application. Whatever framework handles the request
only has to run the effect, and turn its result into a response.

This page builds the same small API with four frameworks: ASP.NET Core's minimal APIs, [Giraffe](https://giraffe.wiki),
[Oxpecker](https://lanayx.github.io/Oxpecker/), and Orsak.AspNetCore. The shared part comes first, and each framework
has a section of its own, which can be read on its own. The code was compiled and run against ASP.NET Core 10, with
Giraffe 8.3 and Oxpecker 2.1.

## The shared part

The API creates orders and looks them up. The orders are kept in a store, described in the usual way, and the
effects of the API are written once, for all the frameworks:

```fsharp
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
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
```

The environment is created for each request. It has the store, which the application registers as a singleton
service, and the request's `HttpContext`, for effects that need the request:

```fsharp
type IRequestProvider =
    abstract member Context: HttpContext

type AppEnv(context: HttpContext, orders: IOrderStore) =
    interface IRequestProvider with
        member _.Context = context

    interface IOrderStoreProvider with
        member _.Orders = orders

let createEnv (ctx: HttpContext) =
    AppEnv(ctx, ctx.RequestServices.GetRequiredService<IOrderStore>())
```

With an `InMemoryOrderStore` implementing `IOrderStore`, the application registers it as usual:

```fsharp
builder.Services.AddSingleton<IOrderStore, InMemoryOrderStore>()
```

Each version of the API responds the same way. `NotFound` becomes a 404, and `Invalid` a 400 with the reason, as a
JSON string, or as plain text with Oxpecker:

```text
GET  /orders/1                                -> 404
POST /orders {"product":"mug","quantity":2}   -> 201 {"id":1,"product":"mug","quantity":2}
GET  /orders/1                                -> 200 {"id":1,"product":"mug","quantity":2}
POST /orders {"product":"mug","quantity":0}   -> 400 the quantity must be positive
```

## Minimal APIs

A minimal API handler takes the route values, the body and the `HttpContext` as parameters, and returns an `IResult`.
The handler creates the environment, runs the effect, and turns its result into an `IResult`, with a function mapping
the errors:

```fsharp
open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Orsak

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
```

F# lambdas aren't converted to `Delegate` on their own, so each handler is given as a `Func`, whose parameter types
tell ASP.NET Core where to bind each value from.

## Giraffe

A Giraffe `HttpHandler` can run an effect like any other asynchronous work. A function turning an effect into a
handler, given a handler for its value, keeps the routes short:

```fsharp
open Giraffe
open Orsak

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
```

`routef` and `bindJson` pass the route value and the body to a function, so composing it with `handleEffect` is
enough. The application adds Giraffe with `builder.Services.AddGiraffe()` and `app.UseGiraffe webApp`.

## Oxpecker

Oxpecker's `EndpointHandler` takes the `HttpContext` and returns a `Task`, and the same approach works:

```fsharp
open Oxpecker
open Orsak

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
```

The application adds Oxpecker with `builder.Services.AddRouting().AddOxpecker()` and
`app.UseRouting().UseOxpecker(endpoints)`.

## Orsak.AspNetCore

Orsak.AspNetCore maps endpoints whose handlers return effects. How an effect becomes a response is up to the
application, which writes it once, as a `*>>` operator on a runner type. The runner here creates the environment,
runs the effect, and executes the `IResult` it returns, or the response for its error:

```fsharp
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Orsak
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
```

The handlers are effects too. Route values are written as format specifiers, such as `%i` for an `int`, and named
after the handler's parameters, so `/orders/%i` with `getOrderEndpoint` maps `/orders/{id:int}`. The body is read by
an effect, from the request in the environment:

```fsharp
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

let mapOrders (app: WebApplication) =
    app.UseRouting().UseEndpoints(fun endpoints ->
        let runner = RunWith createEnv

        endpoints.MapEffectEndpoints [
            runner.RouteGet("/orders/%i", getOrderEndpoint)
            runner.RoutePost("/orders", postOrderEndpoint)
        ])
    |> ignore
```

The handlers are passed as functions, not called, as their parameter names are read from them. Each endpoint is also
instrumented with Orsak's diagnostics, and takes conventions such as `.RequiresAuthorization()` and `.WithName(...)`.
