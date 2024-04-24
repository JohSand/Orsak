namespace Orsak.AspNetCore

open System
open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection


open Orsak
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Quotations
open Polly.Retry
open Microsoft.FSharp.Linq
open Polly
open FSharp.Control

[<AutoOpen>]
module BackgroundWorker =
    let private interpret<'r, 'e> work =
        match work with
        | Patterns.WithValue(:? Effect<'r, unit, 'e> as work, _, expr) ->
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

    let private interpret2<'r, 'e> work =
        match work with
        | Patterns.WithValue(:? (CancellationToken -> Effect<'r, unit, 'e>) as work, _, expr) ->
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

    let private executeFunc<'r, 'e> (work: Effect<'r, unit, 'e>) (_delay: TimeSpan) (logger: ILogger) ct provider = task {
        let! Safe = work |> Effect.untilCancellation logger ct |> Effect.run provider
        return ()
    }

    type EffectfulBackgroundService<'r, 'e>(runnerFactory: CancellationToken -> 'r, e: Effect<'r, unit, 'e>, logger) =
        inherit BackgroundService()

        override this.ExecuteAsync(ct) = task {
            let runner = runnerFactory ct

            let! Safe = e |> Effect.untilCancellation logger ct |> Effect.run runner

            return ()
        }

    type IServiceCollection with

        member this.AddEffectWorker<'r, 'e>
            ([<ReflectedDefinition(includeValue = true)>] work: Expr<Effect<'r, unit, 'e>>)
            =
            this.AddHostedService(fun ctx ->
                let effectName, work = interpret<'r, 'e> work
                let logger = ctx.GetService<ILoggerFactory>().CreateLogger(effectName)
                let provider = ctx.GetRequiredService<'r>()

                new EffectfulBackgroundService<_,_>((fun _ -> provider), work, logger)
            )
        member this.AddEffectWorker<'r, 'e>
            (
                [<ReflectedDefinition(includeValue = true)>] work: Expr<Effect<'r, unit, 'e>>,
                provider: 'r
            ) =
            this.AddHostedService(fun ctx ->
                let effectName, work = interpret work
                let logger = ctx.GetService<ILoggerFactory>().CreateLogger(effectName)

                { new BackgroundService() with
                    override _.ExecuteAsync ct =
                        executeFunc<'r, 'e> work (TimeSpan.FromSeconds 30) logger ct provider
                })

        member this.AddEffectWorker<'r, 'e>
            (
                [<ReflectedDefinition(includeValue = true)>] work: Expr<Effect<'r, unit, 'e>>,
                providerFactory
            ) =
            this.AddHostedService(fun ctx ->
                let effectName, work = interpret work
                let logger = ctx.GetService<ILoggerFactory>().CreateLogger(effectName)

                { new BackgroundService() with
                    override _.ExecuteAsync ct =
                        executeFunc<'r, 'e> work (TimeSpan.FromSeconds 30) logger ct (providerFactory ct)
                })


        member this.AddEffectWorker<'r, 'e>
            (
                [<ReflectedDefinition(includeValue = true)>] work: Expr<Effect<'r, unit, 'e>>,
                provider: 'r,
                delay: TimeSpan
            ) =
            this.AddHostedService(fun ctx ->
                let effectName, work = interpret work
                let logger = ctx.GetService<ILoggerFactory>().CreateLogger(effectName)

                { new BackgroundService() with
                    override _.ExecuteAsync ct =
                        executeFunc<'r, 'e> work delay logger ct provider
                })

        member this.AddEffectWorker<'r, 'e>
            (
                [<ReflectedDefinition(includeValue = true)>] work: Expr<Effect<'r, unit, 'e>>,
                providerFactory,
                delay: TimeSpan
            ) =
            this.AddHostedService(fun ctx ->
                let effectName, work = interpret work
                let logger = ctx.GetService<ILoggerFactory>().CreateLogger(effectName)

                { new BackgroundService() with
                    override _.ExecuteAsync ct =
                        executeFunc<'r, 'e> work delay logger ct (providerFactory ct)
                })

        member this.AddEffectWorker<'r, 'e>
            ([<ReflectedDefinition(includeValue = true)>] work: Expr<CancellationToken -> Effect<'r, unit, 'e>>)
            =
            this.AddHostedService(fun ctx ->
                let effectName, work = interpret2 work
                let logger = ctx.GetService<ILoggerFactory>().CreateLogger(effectName)
                let provider = ctx.GetRequiredService<'r>()

                { new BackgroundService() with
                    override _.ExecuteAsync ct =
                        executeFunc<'r, 'e> (work ct) (TimeSpan.FromSeconds 30) logger ct provider
                })
