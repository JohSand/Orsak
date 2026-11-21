/// What a generator would write for ITodoEvents, as [<GenRemote>] writes ITodoApi's proxy: the server's sender, which
/// turns a call into a callback, and the client's receiver, which turns a callback back into a call.
module Todos.Shared.TodoEvents

open System.Threading.Tasks
open Orsak.Remoting
open Orsak.Remoting.Callbacks
open Todos.Shared.Contract

let private changed format =
    CallbackMember(format, "Changed", [| typeof<Todo list> |])

/// ITodoEvents for the server: each call is a callback, given to send.
let sender (format: IWireFormat) (send: Callback -> unit) : ITodoEvents =
    let changed = changed format

    { new ITodoEvents with
        member _.Changed a =
            send (changed.Call [| box a |])
            Task.FromResult()
    }

/// Calls the client's ITodoEvents for a callback's member and arguments.
let receiver (format: IWireFormat) (events: ITodoEvents) : string -> byte array -> Task<unit> =
    let changed = changed format

    fun memberName arguments ->
        match memberName with
        | "Changed" ->
            let a = changed.Arguments arguments
            events.Changed(unbox a[0])
        | other -> failwith $"ITodoEvents has no member {other}"
