/// The browser client: its environment provides ITodoApi with the generated proxy, so the effects of the contract run
/// here as they would against a fake in a test, while the work happens on the server.
module Todos.Client.App

open Browser.Dom
open Browser.Types
open Orsak
open Orsak.Remoting
open Todos.Shared.Contract

type ClientEnv(api: ITodoApi) =
    interface ITodoApiProvider with
        member _.TodoApi = api

let env =
    ClientEnv(TodoApiProxy.create (RemoteClient(window.location.origin, "/api", JsonFormat.Instance, FetchTransport())))

/// Effects of the contract, which make two remote calls each, and stop at the first error.
let addTodo title = eff {
    let! _ = TodoApi.add title
    return! TodoApi.list ()
}

let completeTodo id = eff {
    let! _ = TodoApi.complete id
    return! TodoApi.list ()
}

let todos = document.getElementById "todos"
let title = document.getElementById "title" :?> HTMLInputElement
let message = document.getElementById "message"

let describe =
    function
    | EmptyTitle -> "A todo needs a title."
    | NotFound id -> $"There is no todo {id}."
    | AlreadyDone id -> $"Todo {id} is already done."

let rec render (list: Todo list) =
    todos.innerHTML <- ""

    for todo in list do
        let item = document.createElement "li"
        item.textContent <- todo.Title

        if todo.Done then
            item.classList.add "done"
        else
            let button = document.createElement "button"
            button.textContent <- "Done"
            button.onclick <- fun _ -> show (completeTodo todo.Id)
            item.appendChild button |> ignore

        todos.appendChild item |> ignore

/// Runs an effect that ends with the todos, and shows them, or the contract's error, or that the call failed.
and show (effect: Effect<ClientEnv, Todo list, TodoError>) =
    task {
        try
            match! Effect.run env effect with
            | Ok list ->
                message.textContent <- ""
                render list
            | Error e -> message.textContent <- describe e
        with :? RemoteCallException ->
            message.textContent <- "The server can't be reached."
    }
    |> ignore

(document.getElementById "add").onclick <-
    fun _ ->
        show (addTodo title.value)
        title.value <- ""

show (TodoApi.list ())
