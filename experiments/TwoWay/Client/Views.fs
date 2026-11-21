/// The views: each part's is given its part of the model and a run for its part's changes, lifted from the app's, so
/// it knows nothing of the rest of the app; a change of more than one part is a callback from the app.
module Todos.Changes.Views

open Feliz
open Orsak.Remoting
open Orsak.Remoting.Callbacks
open Todos.Shared
open Todos.Shared.Contract
open Todos.Changes
open Todos.Changes.OrsakReact

/// The environment, and the connection's id, which the api's calls send.
type ClientEnv(api: ITodoApi, connectionId: Connection.ConnectionId) =
    member _.Api = api
    member _.ConnectionId = connectionId

    interface ITodoApiProvider with
        member _.TodoApi = api

let private error (latest: Latest<string option>) =
    Html.p [ prop.className "message"; prop.text (latest.Value |> Option.defaultValue "") ]

[<ReactComponent>]
let TodoListView (model: TodoList.Model, run: Change<ClientEnv, TodoList.Model> -> unit, openTodo: int -> unit) =
    let title, setTitle = React.useState ""

    Html.div [
        Html.form [
            prop.onSubmit (fun e ->
                e.preventDefault ()
                run (TodoList.add title)
                setTitle "")
            prop.children [
                Html.input [ prop.placeholder "A todo"; prop.value title; prop.onChange (setTitle: string -> unit) ]
                Html.button [ prop.type'.submit; prop.text "Add" ]
            ]
        ]
        error model.Error
        Html.ul [
            for todo in model.Todos.Value ->
                Html.li [
                    prop.key todo.Id
                    prop.className (if todo.Done then "done" else "")
                    prop.children [
                        Html.a [ prop.href "#"; prop.text todo.Title; prop.onClick (fun e -> e.preventDefault (); openTodo todo.Id) ]
                        if not todo.Done then
                            Html.button [ prop.text "Done"; prop.onClick (fun _ -> run (TodoList.complete todo.Id)) ]
                    ]
                ]
        ]
    ]

[<ReactComponent>]
let DetailView (model: Detail.Model, complete: int -> unit, close: unit -> unit) =
    Html.div [
        Html.button [ prop.className "back"; prop.text "Back"; prop.onClick (fun _ -> close ()) ]
        match model.Todo.Value with
        | None -> Html.p "Loading..."
        | Some todo ->
            Html.h2 todo.Title
            Html.p (if todo.Done then "Done." else "Not done yet.")

            if not todo.Done then
                Html.button [ prop.className "complete"; prop.text "Done"; prop.onClick (fun _ -> complete todo.Id) ]
        error model.Error
    ]

[<ReactComponent>]
let App (env: ClientEnv) =
    let model, run, _ = React.useChanges (env, AppModel.initial, AppModel.crashed)

    // keeps a connection open while it's mounted, down which the server calls ITodoEvents, as others change the todos;
    // loads the todos each time it opens, for what changed while it wasn't
    React.useEffect (
        (fun () ->
            let receive = TodoEvents.receiver JsonFormat.Instance (Events.events run)
            let onConnected () = run (AppModel.list (TodoList.load ()))
            let connection = Connection.keep env.ConnectionId env.Api.Connect receive onConnected
            fun () -> connection.Dispose()),
        [||]
    )

    match model.Detail with
    | Some detail ->
        DetailView(
            detail,
            (fun id -> run (AppModel.completeOnPage id)),
            (fun () -> run (AppModel.closed ()))
        )
    | None -> TodoListView(model.List, (AppModel.list >> run), (fun id ->
            run (AppModel.openPage id)
            run (AppModel.loadPage id)))
