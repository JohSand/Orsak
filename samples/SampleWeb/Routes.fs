namespace Routes

open Giraffe
open Orsak
open Orsak.Extensions
open Fleece
open Fleece.SystemTextJson
open Microsoft.AspNetCore.Http

module EffectPipeline =
    let wrap (e: Effect<_, _, _>) =
        eff {
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
        fun next ctx ->
            task {
                match! Effect.run (runEnv ctx) (EffectPipeline.wrap effect) with
                | Ok() -> return! setStatusCode 204 <|| (next, ctx)
                | Error _e -> return! setStatusCode 400 next ctx
            }

    static member inline ( *>> )(effect: Effect<_, Response<'b>, string>, RunWith runEnv) : HttpHandler =
        fun next ctx ->
            task {
                match! Effect.run (runEnv ctx) (EffectPipeline.wrap effect) with
                | Ok(StatusOk b) ->
                    let payload = toJsonText b

                    return!
                        setStatusCode 200
                        >=> setHttpHeader "Content-Type" "application/json"
                        >=> setBodyFromString payload
                        <|| (next, ctx)
                | Ok(Created href) ->
                    return!
                        setStatusCode 201
                        >=> setHttpHeader "Content-Type" "application/json"
                        >=> setHttpHeader "Location" href
                        <|| (next, ctx)
                | Error _e -> return! setStatusCode 400 next ctx
            }



module R =
    let ping () : Effect<_, _, _> = eff { return () }

    let post () = eff { return () }


    let webApp runEnv : HttpHandler =
        let inline (==>) httpHandler effect =
            httpHandler >=> effect *>> RunWith runEnv

        choose [
            GET
            >=> choose [
                route "/ping" ==> ping ()
                route "/ping2" ==> ping ()
                route "/ping3" ==> ping ()
            ]
            POST >=> route "/pong" >=> post () *>> RunWith runEnv
            setStatusCode 404 >=> text "Not Found"
        ]
