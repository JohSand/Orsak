/// The app's state and how it changes, as in Elmish, but with Orsak effects for its commands: update returns the
/// effects to run, of any environment that provides ITodoApi, rather than Elmish's Cmd. So this file doesn't depend on
/// Elmish, and a test runs the commands against a fake ITodoApi, with no browser or server.
module Todos.App.Todos

open Orsak
open Todos.Shared.Contract

type Model = {
    Todos: Todo list
    Error: string option
    Busy: bool
}

type Msg =
    | Load
    | Loaded of Todo list
    | Add of title: string
    | Complete of id: int
    | Failed of string

/// A command: an effect whose value and error are both messages, so whatever it ends with is dispatched.
type Command<'r> = Effect<'r, Msg, Msg>

let describe =
    function
    | EmptyTitle -> "A todo needs a title."
    | NotFound id -> $"There is no todo {id}."
    | AlreadyDone id -> $"Todo {id} is already done."

let private load () : Command<'r> = eff {
    let! todos = TodoApi.list ()
    return Loaded todos
}

/// A change, then the todos again: the contract's error becomes the message Failed.
let private change (effect: Effect<'r, Todo, TodoError>) : Command<'r> = eff {
    let! _ = effect |> Effect.changeError (describe >> Failed)
    let! todos = TodoApi.list ()
    return Loaded todos
}

let init<'r when 'r :> ITodoApiProvider> () : Model * Command<'r> list =
    { Todos = []; Error = None; Busy = true }, [ load () ]

let update<'r when 'r :> ITodoApiProvider> (msg: Msg) (model: Model) : Model * Command<'r> list =
    match msg with
    | Load -> { model with Busy = true }, [ load () ]
    | Loaded todos -> { model with Todos = todos; Busy = false; Error = None }, []
    | Add title -> { model with Busy = true }, [ change (TodoApi.add title) ]
    | Complete id -> { model with Busy = true }, [ change (TodoApi.complete id) ]
    | Failed error -> { model with Busy = false; Error = Some error }, []
