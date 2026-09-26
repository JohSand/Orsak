module SampleWeb.Application

open Microsoft.AspNetCore.Mvc
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


type Auth = Auth

let ping () : Effect<_, _, _> = eff {
    do! Log.getLogger () |>> _.LogInformation("")
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

let post2 (target: string) (_unused: int) = eff { return { message = target } }

/// <summary>
/// This is text
/// </summary>
/// <param name="target"></param>
/// <param name="_unused"></param>
/// <param name="_auth"></param>
let post3 (target: string) (_unused: int) (_auth: Auth) = eff { return { message = target } }

open Orsak
open Orsak.AspNetCore



// let secret (): int*string -> unit =
//     let f = fun (_i: int) (_s: string)   -> ()
//     check "%i-%s" f
