# Elmish sample: Orsak effects as an Elmish app's commands

The remoting sample's todo list, written with the Elmish architecture: `update` returns the new model and Orsak
effects to run, whose results come back as messages. It uses the remoting sample's contract and server. See the
[Elmish guide](../../docs/elmish.md) for how it works.

* **`App/Todos.fs`:** the model, messages, `init` and `update`. It uses Orsak and the contract, not Elmish, and its
  commands run in any environment that provides `ITodoApi`.
* **`App/OrsakElmish.fs`:** the adapter, which an app copies: `Cmd.ofEffects` runs commands as Elmish commands, and
  `Program.mkWithEffects` is `Program.mkProgram` with `init` and `update` returning commands. `Sub.every` is a timer
  subscription.
* **`App/Main.fs`:** the environment, with the generated proxy, a view drawn on the DOM, the refresh subscription, and
  the program.
* **`Tests/Program.fs`:** `update`, tested on .NET against a fake `ITodoApi`, with no Elmish, browser or server.

## Running it

From this folder, after building the remoting sample's server, which generates the contract's proxy:

```sh
dotnet tool restore
dotnet run --project Tests
dotnet build ../Remoting/Server
dotnet fable App -o wwwroot/app
dotnet run --project ../Remoting/Server -- --webroot ../../Elmish/wwwroot
```

The tests print each check. The server then serves this app rather than the remoting sample's: the web root is
resolved from the server's project folder. Then open the address the server prints.

The app is compiled only with Fable, as it uses the browser, so it isn't in `samples.sln`; the tests are. CI's Fable
job runs the tests and compiles the app.

## Why a sample, not a package

The adapter is about 25 lines, the same for any app, and works with Elmish 4 and 5 unchanged, so an app copies it,
rather than depending on an Orsak.Elmish package kept in step with Elmish's releases. Orsak needs nothing new for it.
That's worth revisiting if cancelling superseded calls, or subscriptions running effects, make it much larger.

## In an application

The sample references Orsak's projects. An application references the packages instead: Orsak and Orsak.Remoting, and
Fable.Elmish, with a view library such as Feliz. The commands don't depend on how the view is drawn.
