/// What Orsak.Remoting.AspNetCore would provide for calls from the server to the client: the open connections, a
/// stream for each, and the clients as an Orsak provider, so an effect calls them, and a test fakes them.
module Orsak.Remoting.Callbacks.Connections

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Threading
open System.Threading.Channels
open System.Threading.Tasks
open Orsak

/// The connections open now, each with the callbacks waiting to go down it.
type Connections() =
    let open' = ConcurrentDictionary<string, Channel<Callback>>()

    /// How long a connection goes without a callback before Alive is sent: under the client's timeout.
    static member val Heartbeat = TimeSpan.FromSeconds 5.0 with get, set

    member _.Count = open'.Count

    /// Opens a connection: its id, and the callbacks for it.
    member _.Open() =
        let id = Guid.NewGuid().ToString "N"
        let channel = Channel.CreateUnbounded<Callback>()
        open'[id] <- channel
        id, channel.Reader

    member _.Close(id: string) =
        match open'.TryRemove id with
        | true, channel -> channel.Writer.TryComplete() |> ignore
        | _ -> ()

    /// Sends the callback down each connection but the one excepted, such as the caller's.
    member _.Send(except: string option, callback: Callback) =
        for KeyValue(id, channel) in open' do
            if Some id <> except then
                channel.Writer.TryWrite callback |> ignore

    /// The stream of a connection: Connected with its id, then its callbacks, until the client stops reading, which
    /// cancels the token, or the connection is closed.
    member this.Stream() =
        { new IAsyncEnumerable<Callback> with
            member _.GetAsyncEnumerator cancellationToken =
                let id, reader = this.Open()
                new ConnectionEnumerator(id, reader, (fun () -> this.Close id), cancellationToken)
        }

and private ConnectionEnumerator
    (id: string, reader: ChannelReader<Callback>, close: unit -> unit, cancellationToken: CancellationToken) =
    let mutable current = Connected id
    let mutable started = false

    interface IAsyncEnumerator<Callback> with
        member _.Current = current

        member _.MoveNextAsync() =
            if not started then
                started <- true
                ValueTask<bool> true
            else
                ValueTask<bool>(
                    task {
                        let mutable next = Unchecked.defaultof<Callback>

                        if reader.TryRead(&next) then
                            current <- next
                            return true
                        else
                            // waits for a callback, but sends Alive if none comes for a while
                            let waiting = reader.WaitToReadAsync(cancellationToken).AsTask()
                            let! first = Task.WhenAny(waiting, Task.Delay(Connections.Heartbeat, cancellationToken))

                            if first <> (waiting :> Task) then
                                cancellationToken.ThrowIfCancellationRequested()
                                current <- Alive
                                return true
                            else
                                // awaited, not .Result, so a cancelled wait raises OperationCanceledException as it is
                                let! more = waiting

                                if more && reader.TryRead(&next) then
                                    current <- next
                                    return true
                                else
                                    return false
                    }
                )

        member _.DisposeAsync() =
            close ()
            ValueTask.CompletedTask

/// The clients of the server, as their interface: calling a member calls it on each.
type IClients<'events> =
    /// Every connected client.
    abstract All: 'events
    /// Every connected client but the caller's.
    abstract Others: 'events

type IClientsProvider<'events> =
    abstract Clients: IClients<'events>

module Clients =
    /// Calls the other clients: those connected but the caller.
    let others (call: 'events -> Task<unit>) =
        Effect.Create(fun (p: #IClientsProvider<'events>) -> call p.Clients.Others)

    let all (call: 'events -> Task<unit>) =
        Effect.Create(fun (p: #IClientsProvider<'events>) -> call p.Clients.All)

    /// The clients of the connections, for a call made by the connection caller, if it has one; sender makes the
    /// client's interface of a function sending callbacks, as a generator writes it.
    let ofConnections (connections: Connections) (caller: string option) (sender: (Callback -> unit) -> 'events) =
        let all = sender (fun callback -> connections.Send(None, callback))
        let others = sender (fun callback -> connections.Send(caller, callback))

        { new IClients<'events> with
            member _.All = all
            member _.Others = others
        }
