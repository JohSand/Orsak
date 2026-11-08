# Elmish: Orsak effects as an Elmish app's commands

The question: what Orsak.Remoting looks like in an app with the Elmish architecture, where `update` returns the new
model and the commands to run, and the commands' results come back as messages.

The idea: a command is an Orsak effect whose value and error are both messages, `Effect<'r, Msg, Msg>`. `update`
returns a list of them, for any environment that provides the contract, rather than Elmish's `Cmd`, and a small
adapter runs them in the app's environment as Elmish commands.

```fsharp
let update<'r when 'r :> ITodoApiProvider> (msg: Msg) (model: Model) : Model * Command<'r> list =
    match msg with
    | Add title -> { model with Busy = true }, [ change (TodoApi.add title) ]
    | Loaded todos -> { model with Todos = todos; Busy = false; Error = None }, []
    ...

/// A change, then the todos again: the contract's error becomes the message Failed.
let private change (effect: Effect<'r, Todo, TodoError>) : Command<'r> = eff {
    let! _ = effect |> Effect.changeError (describe >> Failed)
    let! todos = TodoApi.list ()
    return Loaded todos
}

Program.mkWithEffects env onException init update view |> Program.run
```

The app is the remoting sample's todo list (`samples/Remoting`), with its contract and server:

* **`App/Todos.fs`:** the model, messages, `init` and `update`. It uses Orsak and the contract, not Elmish.
* **`App/OrsakElmish.fs`:** the adapter, about 25 lines. `Cmd.ofEffects` runs each command in the environment and
  dispatches the message it ends with, or the one `onException` makes of an exception, such as a
  `RemoteCallException`. `Program.mkWithEffects` is `Program.mkProgram` with init and update returning commands.
* **`App/Main.fs`:** the environment, with the generated proxy, a view drawn on the DOM, and the program.
* **`Tests/Program.fs`:** `update`, tested on .NET against a fake `ITodoApi`, with no Elmish, browser or server.

## What it shows

* **Commands are values a test can run.** Elmish's `Cmd` is a list of functions taking `dispatch`, which a test can
  only call. An Orsak command runs in any environment, so the test runs `update`'s commands against a fake and feeds
  each message back, as Elmish's loop does, in ten lines (`settle`). `update` alone is pure: a test checks the model
  and the number of commands without running them.
* **`update` doesn't take the environment.** It is generic in it, constrained to what its commands use; the app gives
  the environment once, to the program. A command needing another provider widens the constraint, as in any effect.
* **The two kinds of failure stay apart.** The contract's errors become messages in `update`'s own code, with
  `Effect.changeError`. A transport failure is an exception, which the adapter turns into a message with
  `onException`, once for the app.
* **Elmish's `Effect` doesn't clash with Orsak's.** Elmish 5 has `type Effect<'msg> = Dispatch<'msg> -> unit`, and
  with both namespaces open, F# tells it from Orsak's `Effect<'r, 'a, 'e>` by the number of type arguments, and
  `Effect.run` is Orsak's module.
* **No view library is needed for the spike.** The view draws on the DOM itself, so the browser loads Fable's output
  as modules, without npm or a bundler. An app would use Feliz or Fable.Elmish.React; the commands don't change.

The usual alternative passes the environment to `update` and makes commands with `Cmd.OfTask`, which works, but its
commands can't be run by a test:

```fsharp
let update env msg model =
    match msg with
    | Add title -> { model with Busy = true }, Cmd.OfTask.either (Effect.run env) (TodoApi.add title) Added Crashed
```

## Running it

From this folder, after building the remoting sample's server, which generates the contract's proxy:

```sh
dotnet run --project Tests
dotnet fable App -o wwwroot/app
dotnet run --project ../../samples/Remoting/Server -- --webroot ../../../experiments/Elmish/wwwroot
```

The tests print each check. The server then serves this app rather than the sample's (the web root is resolved from
the server's project folder). The program also ran on Node.js, compiled with Fable, against the server: the real Elmish
loop and remote calls, with a view logging each model. The page itself wasn't clicked through in a browser.

## Left for later

* **A package or a recipe:** the adapter is small enough for the docs to show, as for encodings, rather than an
  Orsak.Elmish package to keep in step with Elmish.
* **Commands in flight:** they run at once, so an older `Loaded` can arrive after a newer one; the busy flag only hides
  it. Cancelling a command needs the cancellation Orsak.Remoting doesn't have yet.
* **Subscriptions:** Elmish's `Sub`, such as a timer or a server push, as effects.
