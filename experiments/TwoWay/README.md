# Two-way: the server calls the client, through an interface the client implements

The question: what ergonomic two-way communication looks like with Orsak.Remoting, built on what it has, before
choosing a transport such as WebSockets or SignalR. The app is the model-changes spike's todo list, whose 3-second
refresh is gone: when one client changes the todos, the server tells the others, and they show it at once.

The idea: contracts both ways. The client implements an interface, `ITodoEvents`; the server calls it, as an effect of
its environment's clients; and the client's implementation turns each call into a change of its model.

```fsharp
type ITodoEvents =
    abstract Changed: todos: Todo list -> Task<unit>

// the server: telling the others is an effect, so a test fakes the clients
let addTodo title = eff {
    let! todo = Store.add title
    let! todos = Store.all ()
    do! Clients.others (fun (c: ITodoEvents) -> c.Changed todos)
    return todo
}

// the client: a call is a change of the model, run as the view's are
let events run =
    { new ITodoEvents with
        member _.Changed todos =
            run (AppModel.list (TodoList.pushed todos))
            Task.FromResult() }
```

The calls go down a `RemoteStream`, one for each connection, opened by the client with `ITodoApi.Connect`, so this
needs no new transport: a callback is the member's name and its arguments, written by the format's codecs, as a
call's are.

* **`Shared/Callbacks.fs`:** what Orsak.Remoting would provide, the same on .NET and Fable: `Callback`, an item of a
  connection (`Connected` with its id, `Call` of a member, `Alive`), and `CallbackMember`, a member's codec.
* **`Shared/TodoEvents.fs`:** what a generator would write for `ITodoEvents`, as `[<GenRemote>]` writes a proxy: the
  server's sender, a call to a callback, and the client's receiver, a callback to a call.
* **`Server/Connections.fs`:** what Orsak.Remoting.AspNetCore would provide: the open connections, each a channel, its
  stream, and `IClients<'events>`, with `All` and `Others`, as an Orsak provider, with `Clients.all` and `others`.
* **`Server/Todos.fs`, `Server/Program.fs`:** the remoting sample's server, whose changes tell the other clients, with
  an environment made for each call that knows the caller's connection, from a header.
* **`Client/Connection.fs`:** what Orsak.Remoting would provide on the client: a connection kept open, reconnecting,
  its id sent with each call by `FetchTransport`'s headers.
* **`Client/Events.fs`:** the client's `ITodoEvents`, a change for each call. The rest of `Client/` is the
  model-changes spike's.
* **`Tests/Program.fs`:** on .NET, with no browser or server running: the codecs, the server's effects against fake
  clients, the connections, and the client's events as changes.

## What it shows

* **Telling the others is one line of the server's effects,** `Clients.others (fun c -> c.Changed todos)`, typed by the
  client's interface, with no hub method named by a string. The environment provides the clients, so a test gives it
  fakes that record the calls: adding tells the others the todos, a change the rules refuse tells no one.
* **A call the client receives is a model change,** tested on .NET as the view's are. Pushed todos are a `Latest` read
  taken when they come, so a load asked for before them and answered after is dropped.
* **"Others" needs the caller's connection,** which a call, a separate request, doesn't have. The connection's first
  item is its id, which the client sends with each call in a header, from `FetchTransport`'s headers; `MapRemote`'s
  factory takes the `HttpContext`, so the environment of each call is made with the clients as the caller sees them.
* **Missed calls are loaded, not replayed.** Each time a connection opens, also after it failed, the client loads the
  todos; calls made while it was closed are lost, and the load covers them. Simple, and enough for state a load can
  read; events that can't be read again, such as chat messages, would need the server to keep them, and the client to
  say what it has seen.
* **A dead connection isn't always closed: found in the browser.** Stopping the server ended a direct stream at once,
  but not one through Vite's dev proxy, which kept the browser's side open, so the clients never reconnected. A
  proxy, a load balancer, or a network that drops packets do the same. So the server sends `Alive` when it has sent
  nothing for 5 seconds, and the client takes a connection it hasn't heard from for 12 for dead, stops it, and opens
  another. With that, both tabs reconnected after the server restarted, loaded its empty list, and the next add
  reached the other tab. SignalR does the same, with 15 and 30 seconds.
* **A test found a cancelled wait raised `AggregateException`,** as the stream's enumerator read `.Result` of a
  cancelled task; awaiting it raises `OperationCanceledException`, which `MapRemote` takes for a client that stopped
  reading.
* **Cost:** the plumbing a library would provide is 12 lines shared, 84 on the server and 63 on the client; per
  interface, the 22 lines a generator would write; and the app's own, 10 lines of the client's events and 4 of the
  server's effect telling the others.

In the browser, two tabs against the server: an add in one shows in the other, a todo completed in the second shows
done in the first, and after the server is restarted, both reconnect within the timeout and pushes reach them again.

## What it doesn't do

* **The server can't wait for the client's answer.** A callback returns `Task<unit>`, sent and not answered; a member
  returning a value, the server asking the client, would need the client to call back with the answer, matched by
  an id. Nothing in the app needed it.
* **The client can't stream to the server.** Calls are separate requests, which is fine for actions, but not for, say,
  a stream of a cursor's positions. Browsers can't stream a request's body over HTTP/1.1, and only Chrome over HTTP/2.
* **One server.** The connections are in its memory; with more than one, a call on one must reach the clients of the
  others, through a backplane such as Redis, as SignalR's does.
* **Groups,** such as the clients looking at one list, beyond all and others.
* **Authorization of a connection** is as for any call, but who is connected isn't known to the server's effects.

## The transport, step 2

None of the above changes the API: `Clients.others`, the client's interface, and its events as changes. So the transport
is a choice to make for what it adds:

* **The stream, as here,** is enough for the server telling clients, over HTTP like any call, with headers, cookies and
  authorization as they are, and no new dependency. The heartbeat and reconnecting are needed with any transport.
* **WebSockets** would add the client streaming to the server, and an answer from the client, both matched by ids
  over one socket, and fewer requests; but they're Orsak's own to keep, with a second path through proxies and
  authorization.
* **SignalR** would add a backplane for more than one server, groups, and reconnecting that's been run at scale, with
  `@microsoft/signalr` on the client; Orsak's callbacks would be its messages.

What this spike suggests: build on the stream first, as it covers the common case, the server telling clients, with
nothing new below it, and add a transport when an app needs the client's answers or streams, or more than one server.

## Running it

From this folder:

```sh
dotnet tool restore
npm install
dotnet build Server
dotnet run --project Tests
dotnet fable Client -o out
dotnet run --project Server -- --urls http://localhost:5288
npx vite
```

Then open http://localhost:5173 in two tabs.
