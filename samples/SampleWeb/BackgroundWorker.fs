namespace SampleWeb.BackgroundWorker

open System
open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection


open Orsak
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Quotations

module dummy =
    let doWork () = eff {

        return true
    }

    let dymmy () = eff {
        use timer = new PeriodicTimer(TimeSpan.FromSeconds(10))

        while timer.WaitForNextTickAsync() do
            do! doWork () |> Effect.repeatWhileTrue
    }

[<AutoOpen>]
module BackgroundWorker =
    let private interpret work =
        match work with
        | Patterns.WithValue(:? Effect<'r, unit, 'e> as work, _, expr) ->
            let s =
                match expr with
                | Patterns.ValueWithName(a, _, name) -> name
                | Patterns.Application(Patterns.ValueWithName(a, _, name), _) -> name
                | _ -> "guess?"

            s, work
        | _ -> failwith "what type-safety?"

    let private executeFunc work (delay: TimeSpan) (logger: ILogger) ct provider =
        work
        |> Effect.onError (fun e -> eff {
            try
                logger.LogCritical("Error {Error} during background work", e)
                do! Task.Delay(delay, cancellationToken = ct)
                return! work
            with :? TaskCanceledException ->
                return ()
        })
        |> Effect.runOrFail (provider)


    type IServiceCollection with

        member this.AddEffectWorker<'r, 'e>
            ([<ReflectedDefinition(includeValue = true)>] work: Expr<Effect<'r, unit, 'e>>)
            =
            this.AddHostedService(fun ctx ->
                let effectName, work = interpret work
                let logger = ctx.GetService<ILoggerFactory>().CreateLogger(effectName)
                let provider = ctx.GetRequiredService<'r>()

                { new BackgroundService() with
                    override _.ExecuteAsync ct =
                        executeFunc work (TimeSpan.FromSeconds 30) logger ct provider
                })

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
                        executeFunc work (TimeSpan.FromSeconds 30) logger ct provider
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
                        executeFunc work (TimeSpan.FromSeconds 30) logger ct (providerFactory ct)
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
                        executeFunc work delay logger ct provider
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
                        executeFunc work delay logger ct (providerFactory ct)
                })
