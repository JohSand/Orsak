/// The server: IOrderApi implemented by running Orsak effects in the server's environment, an in-memory store, and
/// served by MapRemote, which maps each of its members to an endpoint.
module Remoting.Server.Program

open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Orsak
open Orsak.Remoting
open Orsak.Remoting.AspNetCore
open Remoting.Shared.Contract

// The server's own side effect, which the client never sees
type IOrderStore =
    abstract Add: Order -> int
    abstract ForCustomer: string -> Order list

type IOrderStoreProvider =
    abstract Store: IOrderStore

module Store =
    let add order =
        Effect.Create(fun (p: #IOrderStoreProvider) -> p.Store.Add order)

    let forCustomer customer =
        Effect.Create(fun (p: #IOrderStoreProvider) -> p.Store.ForCustomer customer)

let knownProducts = set [ "apple"; "pear" ]

let placeOrder (order: Order) = eff {
    match order.Lines with
    | [] -> return! Error EmptyOrder
    | lines ->
        match lines |> List.tryFind (fun l -> not (knownProducts.Contains l.Product)) with
        | Some unknown -> return! Error(UnknownProduct unknown.Product)
        | None ->
            let! id = Store.add order
            return OrderId id
}

type InMemoryStore() =
    let orders = ConcurrentQueue<int * Order>()

    interface IOrderStore with
        member _.Add order =
            let id = orders.Count + 1
            orders.Enqueue((id, order))
            id

        member _.ForCustomer customer = [
            for _, order in orders do
                if order.Customer = customer then
                    order
        ]

type ServerEnv = {
    Store: IOrderStore
} with

    interface IOrderStoreProvider with
        member this.Store = this.Store

/// The calls held, and released by their cancellation, on the server.
type Holds() =
    member val Held = 0 with get, set
    member val Released = 0 with get, set

/// IOrderApi on the server: each call runs an effect in the server's environment.
type ServerOrderApi(env: ServerEnv, holds: Holds) =
    interface IOrderApi with
        member _.Place order = (placeOrder order |> Effect.run env).AsTask()

        member _.History customer =
            (Store.forCustomer customer: Effect<_, _, string>) |> Effect.runOrFail env

        member _.Hold cancellationToken = task {
            lock holds (fun () -> holds.Held <- holds.Held + 1)

            try
                do! Task.Delay(Timeout.Infinite, cancellationToken)
            finally
                lock holds (fun () -> holds.Released <- holds.Released + 1)
        }

        member _.Holds() = Task.FromResult(lock holds (fun () -> holds.Held, holds.Released))

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)
    builder.WebHost.UseUrls("http://localhost:5179") |> ignore
    let app = builder.Build()
    let env = { Store = InMemoryStore() }
    let holds = Holds()

    // the server requires the client's name, as authentication would
    app.Use(fun (ctx: Microsoft.AspNetCore.Http.HttpContext) (next: Microsoft.AspNetCore.Http.RequestDelegate) ->
        if ctx.Request.Headers.ContainsKey "X-Client" then
            next.Invoke ctx
        else
            ctx.Response.StatusCode <- 401
            System.Threading.Tasks.Task.CompletedTask)
    |> ignore

    app.MapRemote<IOrderApi>("/api", [ JsonFormat.Instance ], fun _ -> ServerOrderApi(env, holds)) |> ignore
    app.Run()
    0
