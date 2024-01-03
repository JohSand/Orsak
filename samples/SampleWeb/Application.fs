module SampleWeb.Application

open Orsak
open Orsak.Extensions
open Fleece
open FSharpPlus
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.Logging

type Message = {
    message: string
} with

    static member ToJson(x: Message) = jobj [ "message" .= x.message ]

let ping () : Effect<_, _, _> = eff {
    do! Log.getLogger() |>> _.LogInformation("")
    let! batchId = GuidGenerator.newGuid ()
    let msg = { message = "hi"; batchId = batchId.ToString(); orderId = "2" }
    //do! Message.send msg
    return ()
}

let ping2 (x: int) : Effect<_, _, string> = eff {
    do! Log.logInformation ("Hi")
    let! batchId = GuidGenerator.newGuid ()
    let msg = { message = "hi"; orderId = batchId.ToString(); batchId = "2" }
    //do! Message.send msg
    do! ChatHub.sendMessage "test" "hello"
    return Results.Ok({| test = x |})
}

let post (target, _unused: int) = eff { return { message = target } }
