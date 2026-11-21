/// What Orsak.Remoting would provide on the client: a connection kept open, its callbacks given to the client's
/// interface, reconnecting after it fails, and its id sent with the client's calls.
module Orsak.Remoting.Callbacks.Connection

open System
open System.Threading
open System.Threading.Tasks
open Fable.Core
open Fable.Core.JsInterop
open Orsak.Remoting
open Orsak.Remoting.Callbacks

/// The id of the connection open now, if one is, which the client's calls send, so the server knows who called.
type ConnectionId() =
    member val Current: string option = None with get, set

    /// The header of the calls, for FetchTransport.
    member this.Headers() =
        Task.FromResult [
            match this.Current with
            | Some id -> "X-Connection", id
            | None -> ()
        ]

let private wait (milliseconds: int) : Task<unit> =
    emitJsExpr milliseconds "new Promise(r => setTimeout(r, $0))"

/// How long without hearing from the server before a connection is taken for dead: over the server's heartbeat.
let mutable timeout = 12000

/// Keeps a connection open until disposed: each callback is given to receive, and onConnected is called each time it
/// opens, also after it fails and opens again, when callbacks may have been missed, so the client loads what it shows.
/// A connection that hears nothing, not even Alive, for longer than the timeout is stopped, and opened again.
let keep
    (id: ConnectionId)
    (connect: unit -> RemoteStream<Callback>)
    (receive: string -> byte array -> Task<unit>)
    (onConnected: unit -> unit)
    : IDisposable =
    let mutable stopped = false
    let mutable attempt = new CancellationTokenSource()
    let mutable heard = DateTime.Now

    let watchdog =
        JS.setInterval
            (fun () ->
                if (DateTime.Now - heard).TotalMilliseconds > float timeout then
                    attempt.Cancel())
            1000

    let rec loop () = task {
        attempt <- new CancellationTokenSource()
        heard <- DateTime.Now

        try
            do!
                connect ()
                |> RemoteStream.iterTask attempt.Token (fun callback ->
                    heard <- DateTime.Now

                    match callback with
                    | Connected connection ->
                        id.Current <- Some connection
                        onConnected ()
                        Task.FromResult()
                    | Call(memberName, arguments) -> receive memberName arguments
                    | Alive -> Task.FromResult())
        with _ ->
            ()

        id.Current <- None

        if not stopped then
            do! wait 2000
            return! loop ()
    }

    loop () |> ignore

    { new IDisposable with
        member _.Dispose() =
            stopped <- true
            JS.clearInterval watchdog
            attempt.Cancel()
    }