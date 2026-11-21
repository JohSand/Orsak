/// The components, rendered by React in jsdom against a fake ITodoApi whose calls wait until the test answers them, so
/// the test chooses the order answers come back in. Fails unless each check holds.
module Todos.React.Tests

open System.Threading.Tasks
open Fable.Core
open Fable.Core.JsInterop
open Browser.Types
open Browser.Dom
open Feliz
open Todos.Shared.Contract
open Todos.React.Todos

/// Each call is kept, unanswered, until the test answers it.
type HeldTodoApi() =
    member val Lists = ResizeArray<TaskCompletionSource<Todo list>>()
    member val Completes = ResizeArray<TaskCompletionSource<Result<Todo, TodoError>>>()

    interface ITodoApi with
        member this.List() =
            let call = TaskCompletionSource<Todo list>()
            this.Lists.Add call
            call.Task

        member _.Add _ = Task.FromResult(Error EmptyTitle)

        member this.Complete _ =
            let call = TaskCompletionSource<Result<Todo, TodoError>>()
            this.Completes.Add call
            call.Task

[<Import("act", "react")>]
let private reactAct: (unit -> JS.Promise<unit>) -> JS.Promise<unit> = jsNative

/// Lets the effects' continuations run, then React render what they set.
let private settle () : JS.Promise<unit> = emitJsExpr () "new Promise(r => setTimeout(r, 0))"

/// Does something to the page, such as answering a call or clicking, and waits until React has rendered the result.
/// act returns a thenable whose then returns nothing, so it is made a promise, which task can bind.
let act (f: unit -> unit) : Task<unit> =
    let thenable =
        reactAct (fun () ->
            f ()
            settle ())

    emitJsExpr thenable "Promise.resolve($0)"

let todo id title = { Id = id; Title = title; Done = false }

let items (root: HTMLElement) =
    let list = root.querySelectorAll "li"
    [ for i in 0 .. list.length - 1 -> list.item i :?> HTMLElement ]

/// The titles of the todos, or what the list shows instead of them.
let titles root =
    items root
    |> List.map (fun item ->
        match item.querySelector "span" with
        | null -> item.textContent
        | title -> title.textContent)

let message (root: HTMLElement) = (root.querySelector "#message").textContent

let doneButton (root: HTMLElement) title =
    items root
    |> List.find (fun item -> (item.querySelector "span").textContent = title)
    |> fun item -> item.querySelector "button" :?> HTMLElement

let check name condition =
    if condition then printfn $"passed  {name}" else failwith $"failed  {name}"

/// Renders the app with a new fake, runs the test, and unmounts it, stopping its refresh timer.
let withApp (test: HeldTodoApi -> HTMLElement -> Task<unit>) = task {
    let api = HeldTodoApi()
    let root = document.createElement "div"
    document.body.appendChild root |> ignore
    let react = ReactDOM.createRoot root
    do! act (fun () -> react.render (App(ClientEnv api)))
    do! test api root
    do! act (fun () -> react.unmount ())
    document.body.removeChild root |> ignore
}

task {
    do!
        withApp (fun api root -> task {
            check "the todos load when it mounts" (api.Lists.Count = 1 && titles root = [ "Loading..." ])
            do! act (fun () -> api.Lists[0].SetResult [ todo 1 "milk"; todo 2 "eggs" ])
            check "and are shown once they're answered" (titles root = [ "milk"; "eggs" ])
        })

    do!
        withApp (fun api root -> task {
            do! act (fun () -> api.Lists[0].SetResult [ todo 1 "milk"; todo 2 "eggs" ])

            // two changes, each loading the todos again, whose loads are answered in the wrong order
            do! act (fun () -> (doneButton root "milk").click ())
            do! act (fun () -> api.Completes[0].SetResult(Ok { todo 1 "milk" with Done = true }))
            do! act (fun () -> (doneButton root "eggs").click ())
            do! act (fun () -> api.Completes[1].SetResult(Ok { todo 2 "eggs" with Done = true }))
            check "each change loads the todos again" (api.Lists.Count = 3)

            do! act (fun () -> api.Lists[2].SetResult [ todo 1 "newer" ])
            do! act (fun () -> api.Lists[1].SetResult [ todo 1 "older" ])
            check "an older load, answered last, is dropped" (titles root = [ "newer" ])
        })

    do!
        withApp (fun api root -> task {
            do! act (fun () -> api.Lists[0].SetResult [ todo 1 "milk"; todo 2 "eggs" ])

            // two changes at once, the first answered last, with an error
            do! act (fun () -> (doneButton root "milk").click ())
            do! act (fun () -> (doneButton root "eggs").click ())
            do! act (fun () -> api.Completes[1].SetResult(Ok { todo 2 "eggs" with Done = true }))
            do! act (fun () -> api.Completes[0].SetResult(Error(AlreadyDone 1)))
            check "a call overtaken by another drops its result, even an error" (message root = "")
            check "and doesn't load the todos again" (api.Lists.Count = 2)
        })

    do!
        withApp (fun api root -> task {
            do! act (fun () -> api.Lists[0].SetResult [ todo 1 "milk" ])
            do! act (fun () -> (doneButton root "milk").click ())
            do! act (fun () -> api.Completes[0].SetResult(Error(AlreadyDone 1)))
            check "the contract's error is shown" (message root = "Todo 1 is already done.")
        })

    do!
        withApp (fun api root -> task {
            do! act (fun () -> api.Lists[0].SetException(exn "unreachable"))
            check "an exception is shown as the server being unreachable" (message root = "The server can't be reached.")
        })

    // a call answered once the component is gone: its result is dropped, and nothing fails
    let api = HeldTodoApi()
    let root = document.createElement "div"
    let react = ReactDOM.createRoot root
    do! act (fun () -> react.render (App(ClientEnv api)))
    do! act (fun () -> react.unmount ())
    do! act (fun () -> api.Lists[0].SetResult [ todo 1 "milk" ])
    check "an answer after unmounting is dropped" (root.innerHTML = "")
}
|> ignore
