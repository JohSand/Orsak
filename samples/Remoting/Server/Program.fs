/// The server: ITodoApi implemented by running Orsak effects in the server's own environment, served by MapRemote, and
/// the browser client, served as static files.
module Todos.Server.Program

open System
open System.Collections.Concurrent
open Microsoft.AspNetCore.Builder
open Orsak
open Orsak.Remoting
open Orsak.Remoting.AspNetCore
open Todos.Shared.Contract

/// The server's own side effect, which the browser never sees.
type ITodoStore =
    abstract All: unit -> Todo list
    abstract Find: id: int -> Todo option
    abstract Add: title: string -> Todo
    abstract Save: Todo -> unit

type ITodoStoreProvider =
    abstract Store: ITodoStore

module Store =
    let all () =
        Effect.Create(fun (p: #ITodoStoreProvider) -> p.Store.All())

    let find id =
        Effect.Create(fun (p: #ITodoStoreProvider) -> p.Store.Find id)

    let add title =
        Effect.Create(fun (p: #ITodoStoreProvider) -> p.Store.Add title)

    let save todo =
        Effect.Create(fun (p: #ITodoStoreProvider) -> p.Store.Save todo)

/// The rules, as effects of the store: what the contract's errors are for.
let addTodo (title: string) = eff {
    if String.IsNullOrWhiteSpace title then
        return! Error EmptyTitle
    else
        return! Store.add (title.Trim())
}

let completeTodo id = eff {
    match! Store.find id with
    | None -> return! Error(NotFound id)
    | Some todo when todo.Done -> return! Error(AlreadyDone id)
    | Some todo ->
        let completed = { todo with Done = true }
        do! Store.save completed
        return completed
}

type InMemoryStore() =
    let todos = ConcurrentDictionary<int, Todo>()

    interface ITodoStore with
        member _.All() =
            todos.Values |> Seq.sortBy _.Id |> List.ofSeq

        member _.Find id =
            match todos.TryGetValue id with
            | true, todo -> Some todo
            | _ -> None

        member _.Add title =
            lock todos (fun () ->
                let todo = { Id = todos.Count + 1; Title = title; Done = false }
                todos[todo.Id] <- todo
                todo)

        member _.Save todo = todos[todo.Id] <- todo

type ServerEnv = {
    Store: ITodoStore
} with

    interface ITodoStoreProvider with
        member this.Store = this.Store

/// ITodoApi on the server: each call runs an effect in the server's environment.
type ServerTodoApi(env: ServerEnv) =
    interface ITodoApi with
        member _.List() =
            (Store.all () : Effect<_, _, string>) |> Effect.runOrFail env

        member _.Add title = (addTodo title |> Effect.run env).AsTask()
        member _.Complete id = (completeTodo id |> Effect.run env).AsTask()

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)
    let app = builder.Build()
    let env = { Store = InMemoryStore() }

    app.UseDefaultFiles().UseStaticFiles() |> ignore
    app.MapRemote<ITodoApi>("/api", [ JsonFormat.Instance ], fun _ -> ServerTodoApi env) |> ignore
    app.Run()
    0
