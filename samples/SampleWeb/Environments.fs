namespace SampleWeb

open Azure.Storage.Queues
open Dapper
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.SignalR
open Microsoft.Data.Sqlite
open Microsoft.Extensions.Logging
open Microsoft.Extensions.DependencyInjection

open Orsak
open Orsak.Effects
open Orsak.Extensions

open System
open System.Data.Common
open System.Threading.Tasks

open FSharpPlus
open FSharp.Control

type ChatHub() =
    inherit Hub<IChatHub>()

    //invoked by InvokeAsync
    member this.SendMessage(user, message) =
        //triggers callers On...
        this.Clients.Caller.MessageSent(user, message)

    override this.OnConnectedAsync() =
        let caller = this.Clients.Caller
        let user = this.Context.User
        Task.CompletedTask


type MessageScope(queue: QueueClient) =
    interface MessageSink with
        member this.Send bytes =
            queue.SendMessageAsync(BinaryData bytes) |>> ignore

        member this.Delete(msg) =
            queue.DeleteMessageAsync(msg.MessageId, msg.PopReceipt) |>> ignore

        member this.Receive(token) = taskSeq {
            let! _ = queue.CreateIfNotExistsAsync()

            while not (token.IsCancellationRequested) do
                let! response = queue.ReceiveMessagesAsync(32, cancellationToken = token)

                if response.Value.Length = 0 then
                    //for extra credits, this could increase or shrink depending of prior history
                    do! Task.Delay(TimeSpan.FromMilliseconds 10)
                else
                    yield! response.Value
        }

module Transaction =
    let setup () =
        let conn: DbConnection = new SqliteConnection("Data Source=test.db;Cache=Shared")

        conn.Execute(
            """CREATE TABLE IF NOT EXISTS inbox(
                  id INTEGER PRIMARY KEY,
                  handled DATE DEFAULT CURRENT_TIMESTAMP,
                  orderId TEXT NOT NULL UNIQUE
                );"""
        )
        |> ignore

    let create () = vtask {
        let conn: DbConnection = new SqliteConnection("Data Source=test.db;Cache=Shared")
        do! conn.OpenAsync()
        let! tran = conn.BeginTransactionAsync()
        return DbTransactional(tran)
    }

type MainEnv = {
    context: HttpContext
    loggerFactory: ILoggerFactory
    queueClient: MessageScope
} with

    interface IContextProvider with
        member this.Context = this.context

    interface ILoggerProvider with
        member this.Logger s = this.loggerFactory.CreateLogger(s)

    interface Scoped.CompletableScopeProvider<DbTransactional> with
        member _.BeginScope() = Transaction.create ()

    interface Scoped.ExceptionHandler<string> with
        member _.Handle(arg1: exn) : string =
            raise (System.NotImplementedException())

    interface MessageSinkProvider with
        member this.Sink: MessageSink = this.queueClient

    interface GuidProvider with
        member this.Gen =
            { new GuidGenerator with
                member _.NewGuid() = Guid.NewGuid()
            }

    interface IChatHubProvider with
        member this.Hub =
            let cty =
                this.context.RequestServices.GetRequiredService<IHubContext<ChatHub, IChatHub>>()

            { new IChatHub with
                member this.MessageSent(x, y) = cty.Clients.All.MessageSent(x, y)
            }


type BackgroundEnv = {
    loggerFactory: ILoggerFactory
    queueClient: MessageScope
} with

    interface ILoggerProvider with
        member this.Logger s = this.loggerFactory.CreateLogger(s)

    interface Scoped.CompletableScopeProvider<DbTransactional> with
        member _.BeginScope() = Transaction.create ()

    interface Scoped.ExceptionHandler<string> with
        member _.Handle(arg1: exn) : string =
            raise (System.NotImplementedException())

    interface MessageSinkProvider with
        member this.Sink: MessageSink = this.queueClient
