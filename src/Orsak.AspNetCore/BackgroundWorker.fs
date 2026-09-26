namespace Orsak.AspNetCore

open System
open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection


open Orsak
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Quotations
open FSharp.Control

/// <summary>
/// Hosts long-running effects as ASP.NET Core background services, with <c>AddEffectWorker</c>.
/// </summary>
[<AutoOpen>]
module BackgroundWorker =
    let private interpret2<'r, 'a, 'e> (work: Expr<'a -> Effect<'r, unit, 'e>>)  =
        match work with
        | Patterns.WithValue(:? ('a -> Effect<'r, unit, 'e>) as work, _, expr) ->
            let s =
                match expr with
                | Patterns.ValueWithName(_, _, name) -> name
                | Patterns.Application(Patterns.ValueWithName(_, _, name), _) -> name
                | _ -> "guess?"

            s, work
        | Patterns.WithValue(_, _, expr) ->
            let _ =
                match expr with
                | Patterns.ValueWithName(_, _, name) -> name
                | Patterns.Application(Patterns.ValueWithName(_, _, name), _) -> name
                | _ -> "guess?"

            failwith "what type-safety?"
        | _ -> failwith "what type-safety?"

    /// <summary>
    /// The hosted service <c>AddEffectWorker</c> registers: it runs the worker effect with <c>Effect.forever</c>,
    /// starting it again whenever it fails, until it completes successfully or the host stops.
    /// </summary>
    /// <param name="runnerFactory">Creates the environment, given the host's stopping token</param>
    /// <param name="e">Creates the worker effect, given the host's stopping token</param>
    /// <param name="_logger">The worker's logger</param>
    type EffectfulBackgroundService<'r, 'e>(runnerFactory: _ -> 'r, e: _ -> Effect<'r, unit, 'e>, _logger) =
        inherit BackgroundService()

        override this.ExecuteAsync(ct) = task {
            let runner = runnerFactory ct

            let! _ = e ct |> Effect.forever |> Effect.run runner

            return ()
        }

    type IServiceCollection with

        /// <summary>
        /// Registers a hosted service that runs an effect in the background for the lifetime of the application.
        /// The environment is created from the application's services and the host's stopping token. The effect is started again whenever it fails, and the worker stops when it completes
        /// successfully, so it is typically written as a loop.
        /// </summary>
        /// <param name="f">Creates the environment from the application's services and the host's stopping token</param>
        /// <param name="work">The worker: a function, passed directly so that its name can name the worker's logger, taking the host's stopping token and returning the effect to run</param>
        /// <example>
        /// <code lang="fsharp">
        /// let processMessages (ct: CancellationToken) = eff {
        ///     while not ct.IsCancellationRequested do
        ///         let! message = Queue.receive ()
        ///         do! handle message
        /// }
        ///
        /// builder.Services.AddEffectWorker((fun services ct -> createEnv services ct), processMessages)
        /// </code>
        /// </example>
        member this.AddEffectWorker<'r, 'e>(f, [<ReflectedDefinition(includeValue = true)>] work) =
            this.AddHostedService(fun ctx ->
                let effectName, work = interpret2<'r, CancellationToken, 'e> work
                let logger = ctx.GetService<ILoggerFactory>().CreateLogger(effectName)

                new EffectfulBackgroundService<_, _>(f ctx, work, logger))

        /// <summary>
        /// Registers a hosted service that runs an effect in the background for the lifetime of the application.
        /// The environment is created from the host's stopping token. The effect is started again whenever it fails, and the worker stops when it completes
        /// successfully, so it is typically written as a loop.
        /// </summary>
        /// <param name="f">Creates the environment from the host's stopping token</param>
        /// <param name="work">The worker: a function, passed directly so that its name can name the worker's logger, taking the host's stopping token and returning the effect to run</param>
        member this.AddEffectWorker<'r, 'e>(f, [<ReflectedDefinition(includeValue = true)>] work) =
            this.AddHostedService(fun ctx ->
                let effectName, work = interpret2<'r, CancellationToken, 'e> work
                let logger = ctx.GetService<ILoggerFactory>().CreateLogger(effectName)

                new EffectfulBackgroundService<_, _>(f, work, logger))

        /// <summary>
        /// Registers a hosted service that runs an effect in the background for the lifetime of the application.
        /// The environment is created from the application's services. The effect is started again whenever it fails, and the worker stops when it completes
        /// successfully, so it is typically written as a loop.
        /// </summary>
        /// <param name="f">Creates the environment from the application's services</param>
        /// <param name="work">The worker: a function, passed directly so that its name can name the worker's logger, taking the host's stopping token and returning the effect to run</param>
        member this.AddEffectWorker<'r, 'e>(f, [<ReflectedDefinition(includeValue = true)>] work) =
            this.AddHostedService(fun ctx ->
                let effectName, work = interpret2<'r, CancellationToken, 'e> work
                let logger = ctx.GetService<ILoggerFactory>().CreateLogger(effectName)

                new EffectfulBackgroundService<_, _>((fun _ -> f ctx), work, logger))

        /// <summary>
        /// Registers a hosted service that runs an effect in the background for the lifetime of the application.
        /// The environment is the one given. The effect is started again whenever it fails, and the worker stops when it completes
        /// successfully, so it is typically written as a loop.
        /// </summary>
        /// <param name="provider">The environment</param>
        /// <param name="work">The worker: a function, passed directly so that its name can name the worker's logger, taking the host's stopping token and returning the effect to run</param>
        member this.AddEffectWorker<'r, 'e>(provider, [<ReflectedDefinition(includeValue = true)>] work) =
            this.AddHostedService(fun ctx ->
                let effectName, work = interpret2<'r, CancellationToken, 'e> work
                let logger = ctx.GetService<ILoggerFactory>().CreateLogger(effectName)

                new EffectfulBackgroundService<_, _>((fun _ -> provider), work, logger))

        /// <summary>
        /// Registers a hosted service that runs an effect in the background for the lifetime of the application.
        /// The environment is created from the application's services and the host's stopping token. The effect is started again whenever it fails, and the worker stops when it completes
        /// successfully, so it is typically written as a loop.
        /// </summary>
        /// <param name="f">Creates the environment from the application's services and the host's stopping token</param>
        /// <param name="work">The worker: a function, passed directly so that its name can name the worker's logger, returning the effect to run</param>
        member this.AddEffectWorker<'r, 'e>(f, [<ReflectedDefinition(includeValue = true)>] work) =
            this.AddHostedService(fun ctx ->
                let effectName, work = interpret2<'r, unit, 'e> work
                let logger = ctx.GetService<ILoggerFactory>().CreateLogger(effectName)

                new EffectfulBackgroundService<_, _>(f ctx, (fun _ -> work()), logger))

        /// <summary>
        /// Registers a hosted service that runs an effect in the background for the lifetime of the application.
        /// The environment is created from the host's stopping token. The effect is started again whenever it fails, and the worker stops when it completes
        /// successfully, so it is typically written as a loop.
        /// </summary>
        /// <param name="f">Creates the environment from the host's stopping token</param>
        /// <param name="work">The worker: a function, passed directly so that its name can name the worker's logger, returning the effect to run</param>
        member this.AddEffectWorker<'r, 'e>(f, [<ReflectedDefinition(includeValue = true)>] work) =
            this.AddHostedService(fun ctx ->
                let effectName, work = interpret2<'r, unit, 'e> work
                let logger = ctx.GetService<ILoggerFactory>().CreateLogger(effectName)

                new EffectfulBackgroundService<_, _>(f, (fun _ -> work()), logger))

        /// <summary>
        /// Registers a hosted service that runs an effect in the background for the lifetime of the application.
        /// The environment is created from the application's services. The effect is started again whenever it fails, and the worker stops when it completes
        /// successfully, so it is typically written as a loop.
        /// </summary>
        /// <param name="f">Creates the environment from the application's services</param>
        /// <param name="work">The worker: a function, passed directly so that its name can name the worker's logger, returning the effect to run</param>
        member this.AddEffectWorker<'r, 'e>(f, [<ReflectedDefinition(includeValue = true)>] work) =
            this.AddHostedService(fun ctx ->
                let effectName, work = interpret2<'r, unit, 'e> work
                let logger = ctx.GetService<ILoggerFactory>().CreateLogger(effectName)

                new EffectfulBackgroundService<_, _>((fun _ -> f ctx), (fun _ -> work()), logger))

        /// <summary>
        /// Registers a hosted service that runs an effect in the background for the lifetime of the application.
        /// The environment is the one given. The effect is started again whenever it fails, and the worker stops when it completes
        /// successfully, so it is typically written as a loop.
        /// </summary>
        /// <param name="provider">The environment</param>
        /// <param name="work">The worker: a function, passed directly so that its name can name the worker's logger, returning the effect to run</param>
        member this.AddEffectWorker<'r, 'e>(provider, [<ReflectedDefinition(includeValue = true)>] work) =
            this.AddHostedService(fun ctx ->
                let effectName, work = interpret2<'r, unit, 'e> work
                let logger = ctx.GetService<ILoggerFactory>().CreateLogger(effectName)

                new EffectfulBackgroundService<_, _>((fun _ -> provider), (fun _ -> work()), logger))
