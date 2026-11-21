/// The todo list as React components, whose effects run with the hooks of OrsakReact, in the environment a context
/// provides: the proxy in the browser, a fake in a test.
module Todos.React.Todos

open Feliz
open Todos.Shared.Contract
open Todos.React.OrsakReact

type ClientEnv(api: ITodoApi) =
    interface ITodoApiProvider with
        member _.TodoApi = api

/// The environment of the app's effects, given once, at the root.
let EnvContext = React.createContext<ClientEnv> ()

let describe =
    function
    | EmptyTitle -> "A todo needs a title."
    | NotFound id -> $"There is no todo {id}."
    | AlreadyDone id -> $"Todo {id} is already done."

let todoItem (todo: Todo) (complete: int -> unit) =
    Html.li [
        prop.key todo.Id
        prop.className (if todo.Done then "done" else "")
        prop.children [
            Html.span todo.Title
            if not todo.Done then
                Html.button [ prop.text "Done"; prop.onClick (fun _ -> complete todo.Id) ]
        ]
    ]

[<ReactComponent>]
let TodoList () =
    let env = React.useContext EnvContext
    let version, setVersion = React.useStateWithUpdater 0
    let error, setError = React.useState (None: string option)
    let title, setTitle = React.useState ""

    // a change loads the todos again; its error, the contract's, is shown until the next change
    let changed result =
        match result with
        | Ok _ -> setError None
        | Error e -> setError (Some(describe e))

        setVersion ((+) 1)

    let todos: Run<Todo list, TodoError> = React.useEffectResult (env, TodoApi.list, [| box version |])
    let add, adding = React.useEffectCallback (env, TodoApi.add, changed)
    let complete, completing = React.useEffectCallback (env, TodoApi.complete, changed)

    // loads the todos every few seconds, to show what others change; a load still running is overtaken, not awaited
    React.useEffect (
        (fun () ->
            let timer = Fable.Core.JS.setInterval (fun () -> setVersion ((+) 1)) 3000
            fun () -> Fable.Core.JS.clearInterval timer),
        [||]
    )

    let crashed =
        [ todos.Crashed; adding.Crashed; completing.Crashed ]
        |> List.tryPick id
        |> Option.map (fun _ -> "The server can't be reached.")

    Html.div [
        Html.form [
            prop.onSubmit (fun e ->
                e.preventDefault ()
                add title
                setTitle "")
            prop.children [
                Html.input [
                    prop.placeholder "A todo"
                    prop.value title
                    prop.onChange (setTitle: string -> unit)
                    prop.autoFocus true
                ]
                Html.button [ prop.type'.submit; prop.disabled adding.Running; prop.text "Add" ]
            ]
        ]
        Html.p [ prop.id "message"; prop.text (crashed |> Option.orElse error |> Option.defaultValue "") ]
        Html.ul [
            match todos.Result with
            | Some(Ok list) ->
                for todo in list do
                    todoItem todo complete
            | Some(Error e) -> Html.li (describe e)
            | None -> Html.li "Loading..."
        ]
    ]

[<ReactComponent>]
let App (env: ClientEnv) =
    EnvContext.Provider(env, TodoList())
