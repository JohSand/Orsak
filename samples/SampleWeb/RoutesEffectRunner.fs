namespace SampleWeb

open Application
open Microsoft.AspNetCore.Http

open Orsak
open Fleece
open Fleece.SystemTextJson
open System.Text.Json

module EndpointRouting =
    let wrap (e: Effect<_, _, _>) = eff {
        //other things before the effect is run
        let! result = e
        //stuff after the effect is run
        return result
    }

    let inline writeTo (writer: System.Buffers.IBufferWriter<byte>) b =
        use writer = new Utf8JsonWriter(writer)
        Operators.toJsonValue(b).WriteTo writer
        do writer.Flush()
        writer.BytesCommitted


type EffectRunner<'a> =
    | RunWith of (HttpContext -> 'a)

    static member inline ( *>> )(effect: Effect<'a, IResult, string>, RunWith runEnv) : RequestDelegate =
        RequestDelegate(fun ctx -> task {
            match! Effect.run (runEnv ctx) (EndpointRouting.wrap effect) with
            | Ok result -> return! result.ExecuteAsync ctx
            | Error e -> return ()
        })

    static member inline ( *>> )(effect: Effect<'a, 'b, string>, RunWith runEnv) : RequestDelegate =
        RequestDelegate(fun ctx -> task {
            match! Effect.run (runEnv ctx) (EndpointRouting.wrap effect) with
            | Ok b ->
                let written = b |> EndpointRouting.writeTo ctx.Response.BodyWriter
                ctx.Response.Headers.ContentLength <- written
                ctx.Response.Headers.ContentType <- "application/json; charset=UTF-8"
                do! ctx.Response.CompleteAsync()
            | Error e -> return ()
        })
