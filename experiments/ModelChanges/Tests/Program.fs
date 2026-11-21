/// The changes, tested on .NET without React, a browser or a server: each runs against a fake ITodoApi, and what it
/// ends with is applied to a model, as the adapter does. Fails unless each check holds.
module Todos.Changes.Tests

open System.Threading.Tasks
open Orsak
open Todos.Shared.Contract
open Todos.Changes

/// The contract's rules, kept in a list. A held List waits until the test answers it, with the todos as they were
/// when it was called.
type FakeTodoApi() =
    let mutable todos: Todo list = []
    member val Hold = false with get, set
    member val Held = ResizeArray<unit -> unit>()

    interface ITodoApi with
        member this.List() =
            if this.Hold then
                let call = TaskCompletionSource<Todo list>()
                let answer = todos
                this.Held.Add(fun () -> call.SetResult answer)
                call.Task
            else
                Task.FromResult todos

        member _.Add title =
            if title.Trim() = "" then
                Task.FromResult(Error EmptyTitle)
            else
                let todo = { Id = todos.Length + 1; Title = title; Done = false }
                todos <- todos @ [ todo ]
                Task.FromResult(Ok todo)

        member _.Complete id =
            match todos |> List.tryFind (fun t -> t.Id = id) with
            | None -> Task.FromResult(Error(NotFound id))
            | Some todo when todo.Done -> Task.FromResult(Error(AlreadyDone id))
            | Some todo ->
                let completed = { todo with Done = true }
                todos <- todos |> List.map (fun t -> if t.Id = id then completed else t)
                Task.FromResult(Ok completed)

type TestEnv(api: ITodoApi) =
    interface ITodoApiProvider with
        member _.TodoApi = api

/// Runs the change, and gives what it ends with, as the adapter applies it.
let run env (change: Change<TestEnv, 'model>) = task {
    match! Effect.run env change with
    | Ok apply
    | Error apply -> return apply
}

let titles (model: TodoList.Model) = model.Todos.Value |> List.map _.Title

let check name condition =
    if condition then printfn $"passed  {name}" else failwith $"failed  {name}"

let listTests (api: FakeTodoApi) env = task {
    let! loaded = run env (TodoList.load ())
    check "load loads the todos" (loaded TodoList.initial |> titles = [])

    let! added = run env (TodoList.add "milk")
    let model = added TodoList.initial
    check "add adds the todo, and loads the todos again" (titles model = [ "milk" ] && model.Error.Value = None)

    let! empty = run env (TodoList.add "  ")
    let model = empty model
    check "an empty title is the contract's error" (model.Error.Value = Some "A todo needs a title." && titles model = [ "milk" ])

    let! completed = run env (TodoList.complete 1)
    let model = completed model
    check "complete completes it, and clears the error" (model.Todos.Value.Head.Done && model.Error.Value = None)

    let! again = run env (TodoList.complete 1)
    check "completing it again is an error" ((again model).Error.Value = Some "Todo 1 is already done.")

    // two loads, the first answered last: it is older, so the model keeps the newer
    api.Hold <- true
    let older = run env (TodoList.load ())
    api.Hold <- false
    let! _ = run env (TodoList.add "eggs")
    api.Hold <- true
    let newer = run env (TodoList.load ())
    api.Held[1] ()
    api.Held[0] ()
    let! older = older
    let! newer = newer
    api.Hold <- false
    api.Held.Clear()
    check "an older load, applied last, is ignored" ((model |> newer |> older) |> titles = [ "milk"; "eggs" ])
    check "in either order" ((model |> older |> newer) |> titles = [ "milk"; "eggs" ])

    // two changes ending in the other order than they started: the one started last decides the error
    let! succeeded = run env (TodoList.add "bread")
    let! failedLater = run env (TodoList.add " ")
    check "a success ending after a later failure doesn't clear its error" ((model |> failedLater |> succeeded).Error.Value.IsSome)

    let! failedFirst = run env (TodoList.add " ")
    let! succeededLater = run env (TodoList.add "jam")
    check "a failure ending after a later success isn't shown" ((model |> succeededLater |> failedFirst).Error.Value = None)
}

/// The app's model, with the list and a todo's page: the parts' changes lifted into it.
let appTests (api: FakeTodoApi) env = task {
    let! loaded = run env (AppModel.list (TodoList.load ()))
    let model = loaded AppModel.initial
    check "a list's change, lifted, changes the list in the app's model" (titles model.List = [ "milk"; "eggs"; "bread"; "jam" ])

    let! opened = run env (AppModel.openPage 2)
    let! page = run env (AppModel.loadPage 2)
    let onPage = model |> opened |> page
    check "opening a todo's page loads it" ((onPage.Detail |> Option.bind _.Todo.Value |> Option.map _.Title) = Some "eggs")

    // the page of todo 2, then todo 3, whose loads are answered in the wrong order
    let! openTwo = run env (AppModel.openPage 2)
    let! openThree = run env (AppModel.openPage 3)
    api.Hold <- true
    let loadTwo = run env (AppModel.loadPage 2)
    let loadThree = run env (AppModel.loadPage 3)
    api.Held[1] ()
    api.Held[0] ()
    let! loadTwo = loadTwo
    let! loadThree = loadThree
    api.Hold <- false
    api.Held.Clear()
    let onThree = model |> openTwo |> openThree |> loadThree |> loadTwo
    check "a page's load ending after another page opened is dropped" (
        (onThree.Detail |> Option.map (fun d -> d.Id, d.Todo.Value |> Option.map _.Title)) = Some(3, Some "bread")
    )

    let beforeThree = model |> openTwo |> openThree |> loadTwo
    check "or before the other page's own load ends" (
        (beforeThree.Detail |> Option.map (fun d -> d.Id, d.Todo.Value)) = Some(3, None)
    )

    let! back = run env (AppModel.closed ())
    let left = model |> openTwo |> back |> loadTwo
    check "and so is one ending after the page was left" (left.Detail = None && left.List = model.List)

    // completing on the page changes the page, and loads the list again
    let! completed = run env (AppModel.completeOnPage 2)
    let done' = onPage |> completed
    check "completing on a page completes the page's todo" ((done'.Detail |> Option.bind _.Todo.Value |> Option.map _.Done) = Some true)
    check "and loads the list again" (done'.List.Todos.Value |> List.find (fun t -> t.Id = 2) |> _.Done)

    let! again = run env (AppModel.completeOnPage 2)
    let failed = done' |> again
    check "an error on the page is the page's, and the list isn't loaded again" (
        (failed.Detail |> Option.bind _.Error.Value) = Some "Todo 2 is already done." && failed.List = done'.List
    )

    check "an exception is shown on the list" ((AppModel.crashed (exn "unreachable") model).List.Error.Value = Some "The server can't be reached.")
}

[<EntryPoint>]
let main _ =
    task {
        let api = FakeTodoApi()
        let env = TestEnv api
        do! listTests api env
        do! appTests api env
    }
    |> _.GetAwaiter().GetResult()

    0
