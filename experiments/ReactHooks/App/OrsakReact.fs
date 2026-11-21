/// What connects Orsak's effects to React: hooks that run an effect in an environment and keep its result as state.
/// It is the same for any app, about the size of the Elmish adapter.
module Todos.React.OrsakReact

open Feliz
open Orsak

/// What a component knows of an effect it runs: the result of the latest run that ended, whether another is running,
/// and an exception, such as a server that can't be reached, which isn't one of the effect's errors.
type Run<'a, 'e> = {
    Result: Result<'a, 'e> option
    Running: bool
    Crashed: exn option
}

module Run =
    let initial = { Result = None; Running = false; Crashed = None }

    let started run = { run with Running = true }

    let ended result = { Result = Some result; Running = false; Crashed = None }

    let crashed run e = { run with Running = false; Crashed = Some e }

/// Runs the effect, and sets the state it ends with, unless isLatest says another run has begun since.
let private start env (effect: Effect<'r, 'a, 'e>) isLatest setRun onEnded =
    setRun Run.started

    task {
        try
            let! result = Effect.run env effect

            if isLatest () then
                setRun (fun _ -> Run.ended result)
                onEnded result
        with e ->
            if isLatest () then
                setRun (fun run -> Run.crashed run e)
    }
    |> ignore

type React with

    /// Runs the effect when the component mounts and again when a dependency changes. The result of a run that a
    /// later one, or the component's unmounting, overtook is dropped, so an older answer never replaces a newer one.
    /// The last result is kept while the next run runs.
    static member useEffectResult(env: 'r, effect: unit -> Effect<'r, 'a, 'e>, dependencies: obj array) : Run<'a, 'e> =
        let run, setRun = React.useStateWithUpdater Run.initial

        React.useEffect (
            (fun () ->
                let mutable latest = true
                start env (effect ()) (fun () -> latest) setRun ignore
                fun () -> latest <- false),
            dependencies
        )

        run

    /// A function that runs the effect of its argument, as on a click, and the state of the latest call. A call's
    /// result is dropped once another call has begun, or the component has unmounted; onEnded is given the result of
    /// one that wasn't, such as to load again what it changed.
    static member useEffectCallback
        (env: 'r, effect: 'arg -> Effect<'r, 'a, 'e>, onEnded: Result<'a, 'e> -> unit)
        : ('arg -> unit) * Run<'a, 'e> =
        let run, setRun = React.useStateWithUpdater Run.initial
        let calls = React.useRef 0
        let mounted = React.useRef true

        React.useEffect (
            (fun () ->
                mounted.current <- true
                fun () -> mounted.current <- false),
            [||]
        )

        let call arg =
            calls.current <- calls.current + 1
            let number = calls.current
            start env (effect arg) (fun () -> mounted.current && calls.current = number) setRun onEnded

        call, run
