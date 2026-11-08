namespace Orsak

// The Fable implementation of ../Effect.fs, compiled instead of it when FABLE_COMPILER is set. It has the functions
// of the Effect module that Fable supports so far; see experiments/Fable/README.md.

open System.Threading.Tasks

/// <summary>Functions for creating, running and combining effects.</summary>
[<RequireQualifiedAccess>]
module Effect =

    /// <summary>Starts the effect with the environment.</summary>
    let run<'r, 'a, 'e> (env: 'r) (e: Effect<'r, 'a, 'e>) = e.Run env

    /// <summary>Starts the effect with the environment, and raises an exception in case of error.</summary>
    let runOrFail<'r, 'a, 'e> (env: 'r) (e: Effect<'r, 'a, 'e>) = e.RunOrFail env

/// <summary>Creates effects.</summary>
type Effect =

    /// <summary>An effect that fails with <paramref name="error"/>.</summary>
    static member Error(error: 'e) : Effect<'a, 'b, 'e> = eff { return! Error error }

    /// <summary>Creates an effect from a function returning a task of a result.</summary>
    static member Create(f: 'a -> Task<Result<'b, 'e>>) = mkEffect f

    /// <summary>Creates an effect from a function returning an async of a result.</summary>
    static member Create(f: 'a -> Async<Result<'b, 'e>>) =
        mkEffect (fun a -> Async.StartAsTask(f a))

/// <exclude/>
[<AutoOpen>]
module WrapTwice =
    // the lowest priority: a function returning anything else is wrapped in Ok
    type Effect with

        /// <summary>Creates an effect from a function, whose result is the effect's.</summary>
        static member Create<'a, 'b, 'e>(f: 'a -> 'b) : Effect<'a, 'b, 'e> =
            mkEffect (fun a -> Task.FromResult(Ok(f a)))

/// <exclude/>
[<AutoOpen>]
module WrapOnce =
    // a higher priority than WrapTwice, but lower than the members of Effect itself
    type Effect with

        /// <summary>Creates an effect from a function returning an async.</summary>
        [<CompiledName("CreateAsync")>]
        static member Create(f: 'a -> Async<'b>) : Effect<'a, 'b, _> =
            mkEffect (fun a -> task {
                let! b = Async.StartAsTask(f a)
                return Ok b
            })

        /// <summary>Creates an effect from a function returning a result.</summary>
        [<CompiledName("CreateResult")>]
        static member Create(f: 'a -> Result<'b, 'e>) : Effect<'a, 'b, 'e> =
            mkEffect (fun a -> Task.FromResult(f a))

        /// <summary>Creates an effect from a function returning a task.</summary>
        [<CompiledName("CreateTask")>]
        static member Create(f: 'a -> Task<'b>) : Effect<'a, 'b, _> =
            mkEffect (fun a -> task {
                let! b = f a
                return Ok b
            })
