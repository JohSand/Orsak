/// A todo's page, a part of the app with a model of its own, for the todo it shows. Its changes know nothing of the
/// rest of the app, nor that the page may be left, or another todo's opened, before they end.
module Todos.Changes.Detail

open Orsak
open Todos.Shared.Contract

type Model = {
    Id: int
    Todo: Latest<Todo option>
    Error: Latest<string option>
}

let initial id = { Id = id; Todo = Latest.empty None; Error = Latest.empty None }

/// The todo, from the list: the contract has no member for one.
let load id : Change<'r, Model> = eff {
    let order = Latest.next ()
    let! todos = TodoApi.list ()
    let todo = todos |> List.tryFind (fun t -> t.Id = id)
    return fun model -> { model with Todo = Latest.newer model.Todo { Value = todo; Order = order } }
}

let complete id : Change<'r, Model> = eff {
    let order = Latest.next ()
    let outcome error model = { model with Error = Latest.newer model.Error { Value = error; Order = order } }
    let! todo = TodoApi.complete id |> Effect.changeError (TodoList.describe >> Some >> outcome)

    return
        outcome None
        >> fun model -> { model with Todo = Latest.newer model.Todo { Value = Some todo; Order = order } }
}
