open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Orsak
open Orsak.AspNetCore

open Azure.Storage.Queues
open SampleWeb
open Orsak.Extensions.Message
open FSharpPlus

let mkBackgroundEnv queueCLient (ctx: System.IServiceProvider) _ =    
    {
            loggerFactory = ctx.GetRequiredService<_>()
            queueClient = MessageScope queueCLient
    }

let test () = eff {
    return ()
}

[<EntryPoint>]
let main args =
    Transaction.setup ()
    let builder = WebApplication.CreateBuilder(args)
    let queueCLient = QueueClient("UseDevelopmentStorage=true", "my-ku")
    builder.Services
        .AddHttpContextAccessor()
        .AddEffectWorker(mkBackgroundEnv queueCLient, msgWork)
        .AddSignalR()
    |> ignore

    builder.Logging.AddSimpleConsole(fun opts -> opts.IncludeScopes <- true)
    |> ignore

    let app = builder.Build()
    let loggerFactory = app.Services.GetService<ILoggerFactory>()

    let mkEnv ctx = { context = ctx; loggerFactory = loggerFactory; queueClient = MessageScope queueCLient }

    app
        .UseRouting()
        .UseEndpoints(fun builder ->
            builder.MapEffectEndpoints(
                let (r: EffectRunner<_>) = RunWith mkEnv in

                [
                    r.RouteGet("/ping", Application.ping)
                    r.RouteGet("/ping/%i", Application.ping2).WithName("ping")
                    r.RouteGet("/pong/%s/%i", Application.post)
                ]
            ))

    |> ignore

    app.Run()

    0 // Exit code
