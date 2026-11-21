/// The server's effects: the remoting sample's store and rules, and, once a change is made, the other clients told of
/// it, as an effect of the environment's clients, which a test fakes.
module Todos.Server.Todos

open System
open System.Collections.Concurrent
open Orsak
open Orsak.Remoting.Callbacks.Connections
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

/// Tells the other clients the todos, as they are now.
let private changed () = eff {
    let! todos = Store.all ()
    do! Clients.others (fun (c: ITodoEvents) -> c.Changed todos)
}

/// The rules, as effects of the store: what the contract's errors are for.
let addTodo (title: string) = eff {
    if String.IsNullOrWhiteSpace title then
        return! Error EmptyTitle
    else
        let! todo = Store.add (title.Trim())
        do! changed ()
        return todo
}

let completeTodo id = eff {
    match! Store.find id with
    | None -> return! Error(NotFound id)
    | Some todo when todo.Done -> return! Error(AlreadyDone id)
    | Some todo ->
        let completed = { todo with Done = true }
        do! Store.save completed
        do! changed ()
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

/// The environment of a call: the store, and the clients, as the caller sees them.
type ServerEnv = {
    Store: ITodoStore
    Clients: IClients<ITodoEvents>
} with

    interface ITodoStoreProvider with
        member this.Store = this.Store

    interface IClientsProvider<ITodoEvents> with
        member this.Clients = this.Clients
