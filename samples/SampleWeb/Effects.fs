namespace Orsak.Extensions

open System
open Orsak
open Microsoft.Extensions.Logging
open Microsoft.AspNetCore.Http
open System.Security.Cryptography
open System.Threading.Tasks
open System.Text.Json
open Orsak.Effects
open FSharp.Control
open Fleece
open Fleece.SystemTextJson
open FSharpPlus
open Dapper
open Orsak.Scoped
open Azure.Storage.Queues.Models
open System.Collections.Generic
open System.Threading
open System.Runtime.InteropServices
open System.Runtime.CompilerServices

type GuidGenerator =
    abstract member NewGuid: unit -> Guid

type GuidProvider =
    abstract member Gen: GuidGenerator

module GuidGenerator =
    let newGuid () =
        Effect.Create(fun (p: #GuidProvider) -> p.Gen.NewGuid())


type RNGProvider =
    abstract member Gen: RandomNumberGenerator

module RandomNumberGenerator =
    let getBytes (x: byte array) =
        Effect.Create(fun (provider: #RNGProvider) -> provider.Gen.GetBytes(x))


type ILoggerProvider =
    abstract member Logger: string -> ILogger

type Log =
    static member getLogger([<CallerMemberName; Optional; DefaultParameterValue("")>] caller: string) =
        Effect.Create(fun (provider: #ILoggerProvider) -> provider.Logger(caller))

    static member logInformation(message, [<CallerMemberName; Optional; DefaultParameterValue("")>] caller: string) =
        Effect.Create(fun (provider: #ILoggerProvider) -> provider.Logger(caller).LogInformation(message))

type IContextProvider =
    abstract member Context: HttpContext

module HttpContext =
    let current () =
        Effect.Create(fun (provider: #IContextProvider) -> provider.Context)


type IChatHub =
    abstract member MessageSent: user: string * message: string -> Task

type IChatHubProvider =
    abstract member Hub: IChatHub

module ChatHub =
    let sendMessage user message =
        Effect.Create(fun (provider: #IChatHubProvider) -> task { do! provider.Hub.MessageSent(user, message) })


(*
    Abstraction over storage queues
*)
type MessageModel = {
    message: string
    batchId: string
    orderId: string
} with

    static member ToJson(x: MessageModel) =
        jobj [ "message" .= x.message; "batchId" .= x.batchId; "orderId" .= x.orderId ]

    static member OfJson json =
        match json with
        | JObject o -> monad {
            let! message = o .@ "message"
            let! batchId = o .@ "batchId"
            let! orderId = o .@ "orderId"
            return { message = message; batchId = batchId; orderId = orderId }
          }
        | x -> Decode.Fail.objExpected x

type MessageSink =
    abstract member Send: byte array -> Task<unit>
    abstract member Receive: CancellationToken -> IAsyncEnumerable<QueueMessage>
    abstract member Delete: QueueMessage -> Task<unit>

type MessageSinkProvider =
    abstract member Sink: MessageSink

module Message =

    let inline receive (token) =
        EffSeq.Create(fun (provider: #MessageSinkProvider) -> provider.Sink.Receive(token))

    let delete msg =
        Effect.Create(fun (provider: #MessageSinkProvider) -> provider.Sink.Delete msg)

    ///This is mostly for max control, this can be done in much fewer lines of code if so desired.
    let inline send (message) =
        Effect.Create(fun (provider: #MessageSinkProvider) -> task {
            use ms = new IO.MemoryStream()
            use writer = new Utf8JsonWriter(ms)
            let enc = Operators.toJsonValue message
            enc.WriteTo writer
            writer.Flush()
            let result = ms.ToArray()
            do! provider.Sink.Send(result)
        })

    let private transaction (f: DbTransactional -> Result<_,_>) =
        mkEffect (fun tran -> ValueTask<_>(f tran))

    let read<'a, 'err> (sql: string) (p: obj) : Transaction<'a option, 'err> =
        transaction (fun tran -> 
            let b = tran.Connection.QueryFirstOrDefault<'a>(sql, p)
            match box b with
            | null -> Ok None
            | _ -> Ok(Some b)
        )

    let execute (sql: string) (p: obj) : Transaction<unit, 'err> =
        mkEffect (fun (tran: DbTransactional) -> vtask {
            let connection = tran.Connection
            let b = connection.Execute(sql, param = p)
            return Ok()
        })


    let handleMessage (message: MessageModel) = commitEff {
        match!
            read<{| handled: string; id: Int64; orderId: string |}, _>
                ("SELECT handled, id, orderId FROM inbox where orderId = @orderId")
                {| orderId = message.orderId |}
        with
        | Some a -> do! Log.logInformation $"got duplicate message with id %s{a.orderId}"
        | None ->
            //it is possible to do some upsert shenanigans also
            do! execute "INSERT INTO inbox (orderId) VALUES (@orderId)" {| orderId = message.orderId |}

            ()

        return ()
    }

    ///There is probably a nicer way of doing this, depending on prefered json library
    let inline parseAs (bytes: ReadOnlyMemory<byte>) =
        JsonDocument.Parse(bytes).RootElement
        |> Operators.ofJsonValue
        |> Result.mapError string


    let msgWork (ct: CancellationToken) = eff {
        for msgModel in receive (ct) do
            let! (msg: MessageModel) = parseAs (BinaryData.op_Implicit msgModel.Body)
            
            do! Log.logInformation ("Got message {message}", msg.message)
            do! handleMessage msg
            do! delete msgModel

        return ()
    }
