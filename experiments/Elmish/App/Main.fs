/// The browser: the environment, with the generated proxy, the view, drawn on the page's DOM, and the program.
module Todos.App.Main

open Browser.Dom
open Browser.Types
open Elmish
open Orsak.Remoting
open Todos.Shared.Contract
open Todos.App.Todos
open Todos.App.OrsakElmish

type ClientEnv(api: ITodoApi) =
    interface ITodoApiProvider with
        member _.TodoApi = api

let env =
    ClientEnv(TodoApiProxy.create (RemoteClient(window.location.origin, "/api", JsonFormat.Instance, FetchTransport())))

let onException (e: exn) =
    match e with
    | :? RemoteCallException -> Failed "The server can't be reached."
    | e -> Failed e.Message

let title = document.getElementById "title" :?> HTMLInputElement
let add = document.getElementById "add" :?> HTMLButtonElement
let todos = document.getElementById "todos"
let message = document.getElementById "message"

/// Draws the model; the input stays out of it, so it keeps its focus and text.
let view (model: Model) (dispatch: Dispatch<Msg>) =
    add.disabled <- model.Busy

    add.onclick <-
        fun _ ->
            dispatch (Add title.value)
            title.value <- ""

    message.textContent <- defaultArg model.Error ""
    todos.innerHTML <- ""

    for todo in model.Todos do
        let item = document.createElement "li"
        item.textContent <- todo.Title

        if todo.Done then
            item.classList.add "done"
        else
            let button = document.createElement "button"
            button.textContent <- "Done"
            button.onclick <- fun _ -> dispatch (Complete todo.Id)
            item.appendChild button |> ignore

        todos.appendChild item |> ignore

Program.mkWithEffects env onException init update view |> Program.run
