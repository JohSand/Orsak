open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open OpenTelemetry
open OpenTelemetry.Metrics
open OpenTelemetry.Trace
open Orsak
open Orsak.AspNetCore
open Orsak.AspNetCore.V2
open LoadTest.Orsak.Handlers

type Env =
    { clock: IClock
      guidGen: IGuidGen }

    interface IClockProvider with
        member this.Clock = this.clock

    interface IGuidGenProvider with
        member this.GuidGen = this.guidGen

type EffectRunner =
    | RunWith of (HttpContext -> Env)

    static member inline ( *>> )(effect: Effect<Env, IResult, string>, RunWith runEnv) : RequestDelegate =
        RequestDelegate(fun ctx -> task {
            match! Effect.run (runEnv ctx) effect with
            | Ok result -> return! result.ExecuteAsync ctx
            | Error _e -> ctx.Response.StatusCode <- 500
        })

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)

    builder.Services
        .AddOpenTelemetry()
        .WithMetrics(fun m ->
            m.AddAspNetCoreInstrumentation()
             .AddMeter(EffectDiagnostics.MeterName)
            |> ignore)
        .WithTracing(fun t ->
            t.AddAspNetCoreInstrumentation()
             .AddSource(EffectDiagnostics.SourceName)
            |> ignore)
    |> ignore

    let app = builder.Build()

    let clock =
        { new IClock with
            member _.Now() = DateTimeOffset.UtcNow }

    let guidGen =
        { new IGuidGen with
            member _.NewGuid() = Guid.NewGuid() }

    let mkEnv (_ctx: HttpContext) = { clock = clock; guidGen = guidGen }

    app
        .UseRouting()
        .UseEndpoints(fun builder ->
            builder.MapEffectEndpoints(
                let (r: EffectRunner) = RunWith mkEnv in
                [
                    r.RouteGet("/ping", ping)
                    r.RouteGet("/async/completed", asyncCompleted)
                    r.RouteGet("/async/yield", asyncYield)
                    r.RouteGet("/ping/%i", pingById)
                    r.RouteGet("/json", json)
                    r.RouteGet("/time", time)
                    r.RouteGet("/guid", guid)
                    r.RouteGet("/combined", combined)
                    r.RouteGet("/sum/%i/%i", sum)
                    r.RouteGet2("/sum2/%i/%i", sumCurried)
                    // no route values: a unit handler, called for each request
                    r.RouteGet2("/ping2", ping)
                ]))
    |> ignore

    app.Run()
    0
