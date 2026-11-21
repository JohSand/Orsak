/// The list of todos, a part of the app: its model and its changes, which know nothing of the rest of the app.
module Todos.Changes.TodoList

open Orsak
open Todos.Shared.Contract

type Model = {
    Todos: Latest<Todo list>
    /// The outcome of the change started last, whatever order changes end in: its error, or None.
    Error: Latest<string option>
}

let initial = { Todos = Latest.empty []; Error = Latest.empty None }

let describe =
    function
    | EmptyTitle -> "A todo needs a title."
    | NotFound id -> $"There is no todo {id}."
    | AlreadyDone id -> $"Todo {id} is already done."

/// The todos, as the server pushed them, newer than any load asked for before they came.
let pushed todos : Change<'r, Model> = eff {
    let order = Latest.next ()
    return fun model -> { model with Todos = Latest.newer model.Todos { Value = todos; Order = order } }
}

let load () : Change<'r, Model> = eff {
    let order = Latest.next ()
    let! todos = TodoApi.list ()
    return fun model -> { model with Todos = Latest.newer model.Todos { Value = todos; Order = order } }
}

/// A change on the server, then the todos again. Its outcome, the contract's error or None, is shown unless a change
/// started after it has ended already.
let private changed (effect: Effect<'r, Todo, TodoError>) : Change<'r, Model> = eff {
    let order = Latest.next ()
    let outcome error model = { model with Error = Latest.newer model.Error { Value = error; Order = order } }
    let! _ = effect |> Effect.changeError (describe >> Some >> outcome)
    let! loaded = load ()
    return loaded >> outcome None
}

let add title = changed (TodoApi.add title)

let complete id = changed (TodoApi.complete id)
