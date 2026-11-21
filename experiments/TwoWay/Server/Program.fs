/// The server: ITodoApi implemented by running Orsak effects in an environment made for each call, whose clients know
/// the caller's connection, from the header the client sends; and Connect, a connection's stream of callbacks.
module Todos.Server.Program

open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Orsak
open Orsak.Remoting
open Orsak.Remoting.AspNetCore
open Orsak.Remoting.Callbacks.Connections
open Todos.Shared
open Todos.Shared.Contract
open Todos.Server.Todos

type ServerTodoApi(env: ServerEnv, connections: Connections) =
    interface ITodoApi with
        member _.List() =
            (Store.all () : Effect<_, _, string>) |> Effect.runOrFail env

        member _.Add title = (addTodo title |> Effect.run env).AsTask()
        member _.Complete id = (completeTodo id |> Effect.run env).AsTask()
        member _.Connect() = connections.Stream()

/// The connection a call was made from, if the client sent one.
let caller (ctx: HttpContext) =
    match ctx.Request.Headers["X-Connection"].ToString() with
    | "" -> None
    | id -> Some id

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)
    let app = builder.Build()
    let store = InMemoryStore()
    let connections = Connections()
    let sender = TodoEvents.sender JsonFormat.Instance

    app.MapRemote<ITodoApi>(
        "/api",
        [ JsonFormat.Instance ],
        fun ctx ->
            let env = {
                Store = store
                Clients = Clients.ofConnections connections (caller ctx) sender
            }

            ServerTodoApi(env, connections)
    )
    |> ignore

    app.Run()
    0
