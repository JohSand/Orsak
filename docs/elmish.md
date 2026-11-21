---
title: Elmish
category: Guides
categoryindex: 3
index: 11
---

Elmish
======

In an app with the [Elmish](https://elmish.github.io/elmish/) architecture, `update` returns the new model and the
commands to run, whose results come back as messages. Those commands can be Orsak effects: `update` then returns
effects of any environment that provides what they use, a test runs them against fakes, and the app gives the real
environment once, to the program. With [Orsak.Remoting](remoting.html), that environment is a Fable client's proxies.

This is a recipe, not a package: the adapter is about 25 lines, for an app to copy, and Orsak needs nothing new for it.
It works with Elmish 4 and 5. [The Elmish sample](https://github.com/JohSand/Orsak/tree/main/samples/Elmish) is the
remoting sample's todo list written this way, with all of what this page shows, and its tests.

## A command is an effect of messages

A command is an effect whose value and error are both messages, so whatever it ends with is dispatched:

```fsharp
type Command<'r> = Effect<'r, Msg, Msg>
```

`update` is generic in the environment, constrained to what its commands use, and returns a list of commands rather
than Elmish's `Cmd`. It uses Orsak and the contract, not Elmish, so it compiles and is tested on .NET:

```fsharp
let update<'r when 'r :> ITodoApiProvider> (msg: Msg) (model: Model) : Model * Command<'r> list =
    match msg with
    | Add title -> { model with Busy = true }, [ change (TodoApi.add title) ]
    | Loaded todos -> { model with Todos = todos; Busy = false; Error = None }, []
    | Failed error -> { model with Busy = false; Error = Some error }, []
    // ...

/// A change, then the todos again: the contract's error becomes the message Failed.
let private change (effect: Effect<'r, Todo, TodoError>) : Command<'r> = eff {
    let! _ = effect |> Effect.changeError (describe >> Failed)
    let! todos = TodoApi.list ()
    return Loaded todos
}
```

A command needing another provider widens the constraint, as in any effect.

Elmish 5 has a type `Effect<'msg>` too. With both namespaces open, F# tells it from Orsak's `Effect<'r, 'a, 'e>` by the
number of type arguments, and `Effect.run` is Orsak's.

## The adapter

`Cmd.ofEffects` runs each command in the environment, as an Elmish command, and dispatches the message it ends with.
An exception, such as a `RemoteCallException` when the server can't be reached, becomes a message with `onException`,
once for the app. `Program.mkWithEffects` is `Program.mkProgram` with `init` and `update` returning commands:

```fsharp
module Cmd =
    let ofEffects (env: 'r) (onException: exn -> 'msg) (effects: Effect<'r, 'msg, 'msg> list) : Cmd<'msg> = [
        for effect in effects ->
            fun dispatch ->
                task {
                    try
                        match! Effect.run env effect with
                        | Ok msg
                        | Error msg -> dispatch msg
                    with e ->
                        dispatch (onException e)
                }
                |> ignore
    ]

module Program =
    let mkWithEffects
        (env: 'r)
        (onException: exn -> 'msg)
        (init: unit -> 'model * Effect<'r, 'msg, 'msg> list)
        (update: 'msg -> 'model -> 'model * Effect<'r, 'msg, 'msg> list)
        (view: 'model -> Dispatch<'msg> -> unit)
        =
        let withCmd (model, effects) = model, Cmd.ofEffects env onException effects
        Program.mkProgram (init >> withCmd) (fun msg model -> update msg model |> withCmd) view
```

So the two kinds of failure stay apart: the contract's errors become messages in `update`'s own code, and a failed
call in the adapter.

The program is given the environment, here with the proxy that `[<GenRemote>]` generates:

```fsharp
type ClientEnv(api: ITodoApi) =
    interface ITodoApiProvider with
        member _.TodoApi = api

let env =
    ClientEnv(TodoApiProxy.create (RemoteClient(window.location.origin, "/api", JsonFormat.Instance, FetchTransport())))

let onException (e: exn) =
    match e with
    | :? RemoteCallException -> Crashed "The server can't be reached."
    | e -> Crashed e.Message

Program.mkWithEffects env onException init update view |> Program.run
```

`onException` can tell a 401 by `RemoteCallException.StatusCode`, and send the user to sign in, rather than show an
error.

## Testing update

Elmish's `Cmd` is a list of functions taking `dispatch`, which a test can only call. An Orsak command runs in any
environment, so a test runs `update`'s commands against a fake and feeds each message back to `update`, as Elmish's
loop does:

```fsharp
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
```

```fsharp
let env = TestEnv(FakeTodoApi())
let! started = settle env (init ())
let! added = settle env (update (Add "milk") started)
// added.Todos is [ milk ]
```

`update` alone is pure, so a test can also check the model and the commands it returns without running them:

```fsharp
let busy, commands = update<TestEnv> (Add "milk") started
// busy.Busy, and one command to run
```

## Answers out of order

Two requests can be answered in either order, and an older answer arriving last shouldn't replace a newer one. Each
message that makes a command gives the model a new request number, which the command's answer carries back, and
`update` keeps only the latest's:

```fsharp
let private request model (command: int -> Command<'r>) =
    let number = model.Request + 1
    { model with Busy = true; Request = number }, [ command number ]

let update<'r when 'r :> ITodoApiProvider> (msg: Msg) (model: Model) : Model * Command<'r> list =
    match msg with
    | Add title -> request model (fun n -> change n (TodoApi.add title))
    | Loaded(number, todos) when number = model.Request ->
        { model with Todos = todos; Busy = false; Error = None }, []
    | Loaded _ -> model, [] // an older request's answer
    // ...
```

The older call still runs to its end on the server. To stop it, its member takes a `CancellationToken` (see
[Cancellation](remoting.html#Cancellation)), which the app would cancel when a newer request starts; the sample
doesn't do that.

## Subscriptions

A subscription sends a message, and `update` decides whether it makes a command. A timer refreshing the todos sends
`Refresh`, which `update` turns into a load unless a request is running, so the request numbers and the busy flag stay
in `update`, where the tests check them:

```fsharp
module Sub =
    let every (milliseconds: int) (msg: 'msg) : Subscribe<'msg> =
        fun dispatch ->
            let timer = Fable.Core.JS.setInterval (fun () -> dispatch msg) milliseconds

            { new System.IDisposable with
                member _.Dispose() = Fable.Core.JS.clearInterval timer }

let subscriptions (_: Model) : Sub<Msg> = [ [ "refresh" ], Sub.every 3000 Refresh ]

// in update
| Refresh when model.Busy -> model, []
| Refresh -> request model load
```

A subscription to a server's pushes, such as a [stream](remoting.html#Streams), would read the stream and dispatch a
message for each item, in the same way; the sample doesn't have one.
