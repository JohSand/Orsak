/// What connects Orsak's commands to Elmish: the part a package could provide, for any app.
module Todos.App.OrsakElmish

open Orsak
open Elmish

module Cmd =
    /// Elmish's Cmd of Orsak commands, effects whose value and error are both messages, run in the environment. An
    /// exception, such as a RemoteCallException when the server can't be reached, is a message too, by onException.
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

module Sub =
    /// Dispatches the message every so many milliseconds, until Elmish stops the subscription. It runs no effect:
    /// update makes the message a command, so it decides, with the model, whether one is needed.
    let every (milliseconds: int) (msg: 'msg) : Subscribe<'msg> =
        fun dispatch ->
            let timer = Fable.Core.JS.setInterval (fun () -> dispatch msg) milliseconds

            { new System.IDisposable with
                member _.Dispose() = Fable.Core.JS.clearInterval timer
            }

module Program =
    /// An Elmish program whose init and update return Orsak commands, run in the environment.
    let mkWithEffects
        (env: 'r)
        (onException: exn -> 'msg)
        (init: unit -> 'model * Effect<'r, 'msg, 'msg> list)
        (update: 'msg -> 'model -> 'model * Effect<'r, 'msg, 'msg> list)
        (view: 'model -> Dispatch<'msg> -> unit)
        =
        let withCmd (model, effects) = model, Cmd.ofEffects env onException effects
        Program.mkProgram (init >> withCmd) (fun msg model -> update msg model |> withCmd) view
