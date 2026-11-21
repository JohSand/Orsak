/// The app's update, tested on .NET without Elmish, a browser or a server: its commands run against a fake ITodoApi,
/// and each message they end with goes back to update, as Elmish's loop would do. Fails unless each check holds.
module Todos.App.Tests

open System.Threading.Tasks
open Orsak
open Todos.Shared.Contract
open Todos.App.Todos

/// The contract's rules, kept in a list.
type FakeTodoApi() =
    let mutable todos: Todo list = []

    interface ITodoApi with
        member _.List() = Task.FromResult todos

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

/// Runs the commands, and update with each message they end with, until there are none left: Elmish's loop.
let rec settle (env: TestEnv) (model: Model, commands: Command<TestEnv> list) : Task<Model> = task {
    let mutable model = model

    for command in commands do
        let! result = Effect.run env command

        let msg =
            match result with
            | Ok msg
            | Error msg -> msg

        let! next = settle env (update msg model)
        model <- next

    return model
}

let check name condition =
    if condition then printfn $"passed  {name}" else failwith $"failed  {name}"

[<EntryPoint>]
let main _ =
    task {
        let env = TestEnv(FakeTodoApi())

        let! started = settle env (init ())
        check "init loads the todos, and isn't busy after" (started.Todos = [] && not started.Busy)

        // update alone, without running its commands: the model changes at once, and one command is to run
        let busy, commands = update<TestEnv> (Add "milk") started
        check "Add marks the model busy, with one command to run" (busy.Busy && commands.Length = 1)

        let! added = settle env (busy, commands)
        check "Add adds the todo, and loads the todos again" (added.Todos |> List.map _.Title = [ "milk" ])

        let! empty = settle env (update (Add "  ") added)
        check "an empty title is the contract's error, as a message" (empty.Error = Some "A todo needs a title.")

        let! completed = settle env (update (Complete 1) empty)
        check "Complete completes it, and clears the error" (completed.Todos.Head.Done && completed.Error = None)

        let! again = settle env (update (Complete 1) completed)
        check "completing it again is an error" (again.Error = Some "Todo 1 is already done.")

        // two requests, whose answers come back in the wrong order: the older is stale, and dropped
        let loading, older = update<TestEnv> Load again
        let adding, newer = update<TestEnv> (Add "eggs") loading
        let answer (command: Command<TestEnv>) = task {
            match! Effect.run env command with
            | Ok msg
            | Error msg -> return msg
        }

        let! newerAnswer = answer newer.Head
        let! olderAnswer = answer older.Head
        let afterNewer, _ = update<TestEnv> newerAnswer adding
        let afterBoth, _ = update<TestEnv> olderAnswer afterNewer
        check "an answer to an older request, arriving last, is dropped" (afterBoth = afterNewer)
        check "the latest answer is kept" (afterBoth.Todos |> List.map _.Title = [ "milk"; "eggs" ])

        let busy, whileBusy = update<TestEnv> Refresh adding
        let _, whileIdle = update<TestEnv> Refresh afterBoth
        check "a refresh while a request runs makes no command" (busy = adding && whileBusy.IsEmpty)
        check "a refresh otherwise loads the todos" (whileIdle.Length = 1)

        let crashed, _ = update<TestEnv> (Crashed "The server can't be reached.") adding
        check "an exception is shown, whatever request was running" (crashed.Error.IsSome && not crashed.Busy)
    }
    |> _.GetAwaiter().GetResult()

    0
