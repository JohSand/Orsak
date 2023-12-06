open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open SampleWeb.BackgroundWorker
open Orsak
open Orsak.AspNetCore

open Azure.Storage.Queues // Namespace for Queue storage types
open SampleWeb
open Orsak.Extensions.Message
open FSharpPlus

    //interface ILoggerProvider with
    //    member this.Logger s = this.loggerFactory.CreateLogger(s)

    //interface Scoped.TransactionScopeProvider<MessageScope> with
    //    member this.BeginScope() = vtask {
    //        let! msg = this.queueClient.ReceiveMessageAsync()
    //        return MessageScope(this.queueClient, msg.Value)
    //    }

    //interface Scoped.TransactionScopeProvider<DbTransactional> with
    //    member this.BeginScope() = Transaction.create()

    //interface Scoped.ExceptionHandler<string> with
    //    member this.Handle(arg1: exn): string = 
    //        raise (System.NotImplementedException())

    //interface MessageSink with
    //    member this.Send(bytes: byte array): Task<unit> = 
    //        task {
    //            let! _ = this.queueClient.SendMessageAsync (BinaryData.FromBytes bytes)
    //            return ()
    //        }

    //interface MessageSinkProvider with
    //    member this.Sink: MessageSink = this

let sanity1 () = eff {
    return sprintf "%i"
}

let sanity2 () = eff {
    return 2
}

[<EntryPoint>]
let main args =
    let queueCLient = QueueClient("UseDevelopmentStorage=true", "my-ku")
    Transaction.setup ()
    let builder = WebApplication.CreateBuilder(args)

    builder.Services
        .AddHttpContextAccessor()
        .AddSingleton<BackgroundEnv>(fun ctx -> {
            loggerFactory = ctx.GetRequiredService<_>()
            queueClient = MessageScope queueCLient
        })
        .AddEffectWorker<BackgroundEnv, string>(msgWork)
        .AddSignalR()
    |> ignore

    builder.Logging.AddSimpleConsole(fun opts -> opts.IncludeScopes <- true)
    |> ignore

    let app = builder.Build()
    let loggerFactory = app.Services.GetService<ILoggerFactory>()
    
    let mkEnv ctx = { context = ctx; loggerFactory = loggerFactory; queueClient = MessageScope queueCLient }

    let (x: Effect<unit, string, string>) =
        sanity1 () <*> sanity2()

    app
        .UseRouting()
        .UseEndpoints(fun builder ->
            builder.MapEffectEndpoints(
                let (r: EffectRunner<_>) = RunWith mkEnv in  [
                    r.RouteGet ("/ping", Application.ping)

                    r.RouteGet ("/ping/%i", Application.ping2)

                    r.RouteGet ("/pong/%s/%i", Application.post)
                ]
            )
        )
        
    |> ignore

    app.Run()

    0 // Exit code
