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
  `Sub.every` is a timer subscription, from the second round, below.
* **`App/Main.fs`:** the environment, with the generated proxy, a view drawn on the DOM, the refresh subscription, and
  the program.
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
loop and remote calls, with a view logging each model, and with the refresh subscription, which showed a todo another
client added. The page itself wasn't clicked through in a browser.

## Second round: answers out of order, subscriptions, Elmish 4

* **Stale answers are dropped by number.** Each command-making message gives the model a new request number, which the
  command's `Loaded` or `Failed` carries back; `update` keeps only the latest's, and stays busy until it comes. A test
  runs two commands and feeds their answers in the wrong order: the older is dropped. It needs no cancellation, so it
  works today, though the older call still runs on the server. An exception, `Crashed`, answers no request, so it is
  never stale.
* **A subscription sends a message; `update` makes the command.** The refresh timer, `Sub.every 3000 Refresh`, runs no
  effect: `update` loads the todos on `Refresh`, unless a request is running, so the request numbers and the busy flag
  stay in one place, and the decision is tested on .NET with the rest. A subscription running an effect itself would
  have to repeat that, so for timers it isn't worth it. A push from the server, such as a stream of changes, is the
  case where a subscription would need effects, and is still open.
* **Elmish 4 works too.** The app, adapter included, compiles against Fable.Elmish 4.2.0 unchanged: both versions have
  the same `Cmd`, `Sub` and `Program.withSubscription`.

## As a feature

What the spike suggests building, for when it's picked up.

**The feature:** Orsak effects as the commands of an Elmish app, so that `update` runs against any environment and a
test runs its commands against fakes. It goes with Orsak.Remoting, whose proxies are the environment of a Fable client,
but works for any effect.

**Settled by the spike:**

* **A command is `Effect<'r, 'msg, 'msg>`:** the effect's value and its error are both messages. The contract's errors
  become messages in `update`, with `Effect.changeError`, and exceptions in the adapter, with `onException`, once for
  the app.
* **`update` is generic in the environment,** and the program is given it once. `update` needs neither Elmish nor the
  environment, so it compiles and is tested on .NET.
* **A recipe in the docs, not a package:** the adapter, `Cmd.ofEffects` and `Program.mkWithEffects`, is about 25 lines,
  for an app to copy, as encodings are for formats, rather than an Orsak.Elmish package to keep in step with Elmish's
  releases. Worth revisiting if cancellation or subscriptions make it much larger.
* **Nothing in Orsak changes:** the spike needed no new API, and Elmish's `Effect<'msg>` doesn't clash with Orsak's.

**To build it:**

1. **A tested home for the recipe:** move the app into `samples/` (a second client of the remoting sample, or its own
   sample), and run its tests, `Tests/Program.fs`, in CI's fable job, with Fable compiling the app, so the code the
   docs show can't go stale.
2. **A docs page,** Elmish, linked from the Fable and Remoting guides: the command type, the adapter, an `update`, and
   the test loop, `settle`, which a project copies into its own tests.
3. **Check it with a view library:** the spike draws on the DOM itself. An app with Feliz or Fable.Elmish.React, and
   npm with a bundler, should need no change to the commands; that's untested.
4. **The patterns of the second round in the docs:** request numbers for stale answers, and subscriptions that send
   messages rather than run effects. Both work with Elmish 4 and 5.

**Open, and needing a design first:**

* **Cancelling a superseded call:** request numbers drop a stale answer, but its call still runs on the server. To
  stop it, Orsak's Fable build needs cancellation, and Orsak.Remoting a token through to `fetch`'s `AbortController`.
* **Pushes from the server:** a stream of changes, rather than a timer. A stream of messages would be an `EffectSeq`,
  which Orsak's Fable build doesn't have, and Orsak.Remoting has no streaming; a subscription that dispatches from a
  callback would work today.
* **With the authorization PR,** `onException` can tell a 401 by `RemoteCallException.StatusCode` and send the user to
  sign in, rather than show an error.
