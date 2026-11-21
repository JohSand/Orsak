/// Both ways, tested on .NET without a browser or a server running. Fails unless each check holds.
module Todos.TwoWay.Tests

open System
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks
open Orsak
open Orsak.Remoting
open Orsak.Remoting.Callbacks
open Orsak.Remoting.Callbacks.Connections
open Todos.Shared
open Todos.Shared.Contract
open Todos.Server.Todos
open Todos.Changes

let check name condition =
    if condition then printfn $"passed  {name}" else failwith $"failed  {name}"

/// ITodoEvents recording what it's called with.
type RecordedEvents() =
    member val Changed = ResizeArray<Todo list>()

    interface ITodoEvents with
        member this.Changed todos =
            this.Changed.Add todos
            Task.FromResult()

/// The clients, as fakes: what the others and all were called with.
type FakeClients() =
    member val Others = RecordedEvents()
    member val All = RecordedEvents()

    interface IClients<ITodoEvents> with
        member this.All = this.All
        member this.Others = this.Others

let milk = { Id = 1; Title = "milk"; Done = false }

let codecs () = task {
    let format = JsonFormat.Instance
    let sent = ResizeArray<Callback>()
    let sender = TodoEvents.sender format sent.Add
    do! sender.Changed [ milk ]

    let received = RecordedEvents()
    let receive = TodoEvents.receiver format received

    match sent |> List.ofSeq with
    | [ Call(name, arguments) ] ->
        // as it crosses the wire: the callback itself is written by the format too
        let codec = (format :> IWireFormat).CodecFor typeof<Callback>
        let across = codec.Decode(codec.Encode(box (Call(name, arguments)))) :?> Callback

        match across with
        | Call(name, arguments) -> do! receive name arguments
        | _ -> ()
    | _ -> ()

    check "a call of ITodoEvents crosses as a callback, and calls the client's" (List.ofSeq received.Changed = [ [ milk ] ])
}

let serverEffects () = task {
    let clients = FakeClients()
    let env = { Store = InMemoryStore(); Clients = clients }

    let! added = addTodo "milk" |> Effect.run env
    check "adding tells the other clients the todos" (added.IsOk && List.ofSeq clients.Others.Changed = [ [ milk ] ])
    check "and not all, the caller among them" (clients.All.Changed.Count = 0)

    let! empty = addTodo " " |> Effect.run env
    check "a change the rules refuse tells no one" (empty = Error EmptyTitle && clients.Others.Changed.Count = 1)

    let! _ = completeTodo 1 |> Effect.run env
    check "completing tells them too" (clients.Others.Changed.Count = 2 && clients.Others.Changed[1].Head.Done)
}

let connections () = task {
    let connections = Connections()
    let sender = TodoEvents.sender JsonFormat.Instance
    Connections.Heartbeat <- TimeSpan.FromMilliseconds 100.0

    use cancel = new CancellationTokenSource()
    let a = connections.Stream().GetAsyncEnumerator cancel.Token
    let b = connections.Stream().GetAsyncEnumerator cancel.Token
    let! _ = a.MoveNextAsync()
    let! _ = b.MoveNextAsync()

    let idOf (e: Collections.Generic.IAsyncEnumerator<Callback>) =
        match e.Current with
        | Connected id -> id
        | other -> failwith $"{other}"

    check "a connection starts with its id" (idOf a <> idOf b && connections.Count = 2)

    // a call made from a: the others are b
    let clients = Clients.ofConnections connections (Some(idOf a)) sender
    do! clients.Others.Changed [ milk ]
    let! _ = b.MoveNextAsync()
    check "a call of the others goes down their connections" (b.Current.IsCall)
    let! _ = a.MoveNextAsync()
    check "and not down the caller's, which hears Alive instead" (a.Current = Alive)

    do! a.DisposeAsync()
    check "a connection the client stops reading is closed" (connections.Count = 1)

    cancel.Cancel()

    let! ended =
        task {
            try
                let! _ = b.MoveNextAsync()
                return false
            with :? OperationCanceledException ->
                return true
        }

    check "and one whose request is aborted ends" ended
}

type TestEnv() =
    interface ITodoApiProvider with
        member _.TodoApi = failwith "not called"

let clientEvents () = task {
    let ran = ResizeArray<Change<TestEnv, AppModel.Model>>()
    let events = Events.events ran.Add
    do! events.Changed [ milk ]

    let! result = Effect.run (TestEnv()) ran[0]

    let apply =
        match result with
        | Ok f
        | Error f -> f

    let model = apply AppModel.initial
    check "a call of the client's events is a change of its model" (model.List.Todos.Value = [ milk ])
}

[<EntryPoint>]
let main _ =
    task {
        do! codecs ()
        do! serverEffects ()
        do! connections ()
        do! clientEvents ()
    }
    |> _.GetAwaiter().GetResult()

    0
