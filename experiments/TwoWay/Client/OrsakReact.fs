/// What connects the changes to React: the model as state, and a function that runs a change and applies what it ends
/// with to the model as it is then, so changes ending in any order don't lose each other's work.
module Todos.Changes.OrsakReact

open Feliz
open Orsak

type React with

    /// The model, a function running a change, and how many changes are running.
    static member useChanges
        (env: 'r, initial: 'model, onException: exn -> 'model -> 'model)
        : 'model * (Effect<'r, 'model -> 'model, 'model -> 'model> -> unit) * int =
        let model, setModel = React.useStateWithUpdater initial
        let running, setRunning = React.useStateWithUpdater 0

        let run change =
            setRunning ((+) 1)

            task {
                try
                    match! Effect.run env change with
                    | Ok apply
                    | Error apply -> setModel apply
                with e ->
                    setModel (onException e)

                setRunning (fun n -> n - 1)
            }
            |> ignore

        model, run, running
