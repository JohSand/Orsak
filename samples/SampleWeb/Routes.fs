module Routes

open Giraffe
open Orsak
open Orsak.Extensions
open Fleece
open Fleece.SystemTextJson
open Microsoft.AspNetCore.Http

module EffectPipeline =
    let wrap (e: Effect<_,_,_>) = eff {
        use! _a = HttpContext.pushCorrelationId ()
        //other things before the effect is run
        let! result = e
        //stuff after the effect is run
        return result
    }

type Response<'a> =
    | StatusOk of 'a
    | Created of string

[<Struct>]
type EffectRunner<'a> =
    | RunWith of (HttpContext -> 'a)
    static member inline ( *>> )(effect: Effect<_, unit, string>, RunWith runEnv) : HttpHandler =
        fun next ctx -> task {
            match! Effect.run (runEnv ctx) (EffectPipeline.wrap effect) with
            | Ok () -> return! setStatusCode 204 <|| (next, ctx)
            | Error _e -> return! setStatusCode 400 next ctx
        }

type EffectRunner<'T> with
    static member inline ( *>> )(effect: Effect<_, Response<'b>, string>, RunWith runEnv) : HttpHandler =
        fun next ctx -> task {
            match! Effect.run (runEnv ctx) (EffectPipeline.wrap effect) with
            | Ok (StatusOk b) ->
                let payload = toJsonText b

                return!
                    setStatusCode 200 >=> setHttpHeader "Content-Type" "application/json" >=> setBodyFromString payload
                    <|| (next, ctx)
            | Ok (Created href) ->
                return!
                    setStatusCode 201
                    >=> setHttpHeader "Content-Type" "application/json"
                    >=> setHttpHeader "Location" href
                    <|| (next, ctx)
            | Error _e -> return! setStatusCode 400 next ctx
        }


type Ping = {
        message: string
} with
    static member ToJson(x: Ping) = jobj [ "message" .= x.message ]

module Handlers =
    let ping () : Effect<_, _, _> = eff {
        return ()
    }

    let post () = eff { return () }

    open FSharpPlus
    //let blaHa () : Effect<unit,_,unit> =
    //    let (e: Effect<unit,int,unit>) = result 1
    //    let (fn: Effect<unit,int -> string,unit>) =
    //            eff {
    //                return (fun i -> i.ToString())
    //            }
    //    fn <*> e

let webApp runEnv : HttpHandler =
    let inline (==>) httpHandler effect =
        httpHandler >=> effect *>> RunWith runEnv

    choose [
        GET >=> choose [
                route "/ping" ==> Handlers.ping ()
            ]
        POST >=> route "/pong" >=> Handlers.post () *>> RunWith runEnv
        setStatusCode 404 >=> text "Not Found"
    ]
