/// The whole app: the list, and the page of the todo that's open, if one is. Its changes are the parts', lifted, and
/// those that change more than one part.
module Todos.Changes.AppModel

open Todos.Changes

type Model = {
    List: TodoList.Model
    Detail: Detail.Model option
}

let initial = { List = TodoList.initial; Detail = None }

let list change =
    Change.lift (fun m -> Some m.List) (fun part m -> { m with List = part }) change

/// A change of the page of the todo id: dropped if that page is no longer open when it ends.
let detail id change =
    Change.lift (fun m -> m.Detail |> Option.filter (fun d -> d.Id = id)) (fun part m -> { m with Detail = Some part }) change

/// Opening a todo's page is two changes: the page, at once, and then its todo, once it's loaded. A change is applied
/// when it ends, so one change can't show the page before its effect has run.
let openPage id =
    Change.now (fun m -> { m with Detail = Some(Detail.initial id) })

let loadPage id = detail id (Detail.load id)

let closed () = Change.now (fun m -> { m with Detail = None })

/// Completing a todo on its page changes the page, and loads the list again, as it changed too.
let completeOnPage id =
    detail id (Detail.complete id) |> Change.andThen (list (TodoList.load ()))

/// An exception, such as a server that can't be reached, which isn't a change's error: shown on the list.
let crashed (_: exn) (model: Model) = {
    model with
        List.Error = { Value = Some "The server can't be reached."; Order = Latest.next () }
}
