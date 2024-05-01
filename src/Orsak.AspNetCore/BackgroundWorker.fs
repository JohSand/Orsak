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

    type EffectfulBackgroundService<'r, 'e>(runnerFactory: _ -> 'r, e: _ -> Effect<'r, unit, 'e>, logger) =
        inherit BackgroundService()

        override this.ExecuteAsync(ct) = task {
            let runner = runnerFactory ct

            let! Safe = e ct |> Effect.untilCancellation logger ct |> Effect.run runner

            return ()
        }

    type IServiceCollection with

        member this.AddEffectWorker<'r, 'e>(f, [<ReflectedDefinition(includeValue = true)>] work) =
            this.AddHostedService(fun ctx ->
                let effectName, work = interpret2<'r, CancellationToken, 'e> work
                let logger = ctx.GetService<ILoggerFactory>().CreateLogger(effectName)

                new EffectfulBackgroundService<_, _>(f ctx, work, logger))

        member this.AddEffectWorker<'r, 'e>(f, [<ReflectedDefinition(includeValue = true)>] work) =
            this.AddHostedService(fun ctx ->
                let effectName, work = interpret2<'r, CancellationToken, 'e> work
                let logger = ctx.GetService<ILoggerFactory>().CreateLogger(effectName)

                new EffectfulBackgroundService<_, _>(f, work, logger))

        member this.AddEffectWorker<'r, 'e>(f, [<ReflectedDefinition(includeValue = true)>] work) =
            this.AddHostedService(fun ctx ->
                let effectName, work = interpret2<'r, CancellationToken, 'e> work
                let logger = ctx.GetService<ILoggerFactory>().CreateLogger(effectName)

                new EffectfulBackgroundService<_, _>((fun _ -> f ctx), work, logger))

        member this.AddEffectWorker<'r, 'e>(provider, [<ReflectedDefinition(includeValue = true)>] work) =
            this.AddHostedService(fun ctx ->
                let effectName, work = interpret2<'r, CancellationToken, 'e> work
                let logger = ctx.GetService<ILoggerFactory>().CreateLogger(effectName)

                new EffectfulBackgroundService<_, _>((fun _ -> provider), work, logger))

        member this.AddEffectWorker<'r, 'e>(f, [<ReflectedDefinition(includeValue = true)>] work) =
            this.AddHostedService(fun ctx ->
                let effectName, work = interpret2<'r, unit, 'e> work
                let logger = ctx.GetService<ILoggerFactory>().CreateLogger(effectName)

                new EffectfulBackgroundService<_, _>(f ctx, (fun _ -> work()), logger))

        member this.AddEffectWorker<'r, 'e>(f, [<ReflectedDefinition(includeValue = true)>] work) =
            this.AddHostedService(fun ctx ->
                let effectName, work = interpret2<'r, unit, 'e> work
                let logger = ctx.GetService<ILoggerFactory>().CreateLogger(effectName)

                new EffectfulBackgroundService<_, _>(f, (fun _ -> work()), logger))

        member this.AddEffectWorker<'r, 'e>(f, [<ReflectedDefinition(includeValue = true)>] work) =
            this.AddHostedService(fun ctx ->
                let effectName, work = interpret2<'r, unit, 'e> work
                let logger = ctx.GetService<ILoggerFactory>().CreateLogger(effectName)

                new EffectfulBackgroundService<_, _>((fun _ -> f ctx), (fun _ -> work()), logger))

        member this.AddEffectWorker<'r, 'e>(provider, [<ReflectedDefinition(includeValue = true)>] work) =
            this.AddHostedService(fun ctx ->
                let effectName, work = interpret2<'r, unit, 'e> work
                let logger = ctx.GetService<ILoggerFactory>().CreateLogger(effectName)

                new EffectfulBackgroundService<_, _>((fun _ -> provider), (fun _ -> work()), logger))
