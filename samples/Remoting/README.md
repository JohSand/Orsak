# Remoting sample: todos in the browser, kept on the server

A todo list whose effects run in the browser, compiled with Fable, while the work happens on an ASP.NET Core server.
See the [Remoting guide](../../docs/remoting.md) for how it works.

* **`Shared/Contract.fs`:** the contract, `ITodoApi`, an Orsak interface with its provider and effects, and the
  contract's errors as a union. `[<GenRemote>]` generates the proxy, `TodoApiProxy`, at the end of the file, when .NET
  builds the project.
* **`Server/Program.fs`:** implements `ITodoApi` by running effects in the server's environment, an in-memory store,
  where the rules are: a todo needs a title, and is completed once. `MapRemote` serves it at `/api`, and the server
  serves the browser client from `wwwroot`.
* **`Client/App.fs`:** the browser client. Its environment provides `ITodoApi` with the proxy, so an effect such as
  adding a todo and listing them makes two calls to the server, and the contract's errors come back as values.

## Running it

From this folder. Node.js isn't needed: the browser loads Fable's output as modules, without a bundler.

```sh
dotnet tool restore
dotnet build Server
dotnet fable Client -o Server/wwwroot/app
dotnet run --project Server
```

Then open the address the server prints. Build the server before compiling the client: the build generates the proxy
that the client uses. Add `--watch` to `dotnet fable` to compile the client again on each change.

The client is compiled only with Fable, as it uses the browser, so it isn't in `samples.sln`.

## In an application

The sample references Orsak's projects, so it runs with the code in this repository. An application references the
packages instead: Orsak.Remoting in the shared project and the client, Orsak.Remoting.AspNetCore in the server, and
Myriad.Sdk and Orsak.Myriad in the shared project, for .NET only, as `Shared/Shared.fsproj` shows.
