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
    /// The number of the latest request: an answer to an earlier one is stale, and dropped.
    Request: int
}

type Msg =
    | Load
    /// Sent by a timer, to see changes made elsewhere: a Load, unless a request is already running.
    | Refresh
    | Loaded of request: int * Todo list
    | Add of title: string
    | Complete of id: int
    | Failed of request: int * string
    /// An exception, such as a server that can't be reached, which isn't a request's answer, so it is never stale.
    | Crashed of string

/// A command: an effect whose value and error are both messages, so whatever it ends with is dispatched.
type Command<'r> = Effect<'r, Msg, Msg>

let describe =
    function
    | EmptyTitle -> "A todo needs a title."
    | NotFound id -> $"There is no todo {id}."
    | AlreadyDone id -> $"Todo {id} is already done."

let private load request : Command<'r> = eff {
    let! todos = TodoApi.list ()
    return Loaded(request, todos)
}

/// A change, then the todos again: the contract's error becomes the message Failed.
let private change request (effect: Effect<'r, Todo, TodoError>) : Command<'r> = eff {
    let! _ = effect |> Effect.changeError (fun e -> Failed(request, describe e))
    let! todos = TodoApi.list ()
    return Loaded(request, todos)
}

/// A new request: the model is busy with it, and remembers its number, which the command's answer carries.
let private request model (command: int -> Command<'r>) =
    let number = model.Request + 1
    { model with Busy = true; Request = number }, [ command number ]

let init<'r when 'r :> ITodoApiProvider> () : Model * Command<'r> list =
    request { Todos = []; Error = None; Busy = false; Request = 0 } load

let update<'r when 'r :> ITodoApiProvider> (msg: Msg) (model: Model) : Model * Command<'r> list =
    match msg with
    | Load -> request model load
    | Refresh when model.Busy -> model, []
    | Refresh -> request model load
    | Add title -> request model (fun n -> change n (TodoApi.add title))
    | Complete id -> request model (fun n -> change n (TodoApi.complete id))
    | Loaded(number, todos) when number = model.Request ->
        { model with Todos = todos; Busy = false; Error = None }, []
    | Failed(number, error) when number = model.Request -> { model with Busy = false; Error = Some error }, []
    | Loaded _
    | Failed _ -> model, []
    | Crashed error -> { model with Busy = false; Error = Some error }, []
