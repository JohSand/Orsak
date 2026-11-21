/// The client's side of ITodoEvents: each call the server makes is a change of the model, run as the view's are, so a
/// test calls it on .NET and applies what it runs.
module Todos.Changes.Events

open System.Threading.Tasks
open Todos.Shared.Contract
open Todos.Changes

let events (run: Change<'r, AppModel.Model> -> unit) : ITodoEvents =
    { new ITodoEvents with
        member _.Changed todos =
            run (AppModel.list (TodoList.pushed todos))
            Task.FromResult()
    }
