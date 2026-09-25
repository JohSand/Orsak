open System
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open OpenTelemetry
open OpenTelemetry.Metrics
open OpenTelemetry.Trace

type IClock =
    abstract Now: unit -> DateTimeOffset

type IGuidGen =
    abstract NewGuid: unit -> Guid

type SystemClock() =
    interface IClock with
        member _.Now() = DateTimeOffset.UtcNow

type SystemGuidGen() =
    interface IGuidGen with
        member _.NewGuid() = Guid.NewGuid()

/// Work that has already completed, bound by /async/completed.
let completed = Task.FromResult "pong"

[<EntryPoint>]
let main args =
    let builder = WebApplication.CreateBuilder(args)

    builder.Services
        .AddSingleton<IClock, SystemClock>()
        .AddSingleton<IGuidGen, SystemGuidGen>()
    |> ignore

    builder.Services
        .AddOpenTelemetry()
        .WithMetrics(fun m -> m.AddAspNetCoreInstrumentation() |> ignore)
        .WithTracing(fun t -> t.AddAspNetCoreInstrumentation() |> ignore)
    |> ignore

    let app = builder.Build()

    app.MapGet("/ping", Func<IResult>(fun () -> Results.Ok("pong")))
    |> ignore

    // an async handler whose work has already completed: the task it binds is shared by all requests
    app.MapGet(
        "/async/completed",
        Func<Task<IResult>>(fun () -> task {
            let! message = completed
            return Results.Ok(message)
        })
    )
    |> ignore

    // an async handler that suspends, and resumes on the thread pool
    app.MapGet(
        "/async/yield",
        Func<Task<IResult>>(fun () -> task {
            do! Task.Yield()
            return Results.Ok("pong")
        })
    )
    |> ignore

    app.MapGet(
        "/ping/{id:int}",
        Func<int, IResult>(fun id -> Results.Json({| id = id; message = "pong" |}))
    )
    |> ignore

    app.MapGet(
        "/json",
        Func<IResult>(fun () ->
            Results.Json(
                {|
                    message = "hello"
                    items = [| 1; 2; 3 |]
                    nested = {| name = "test"; value = 42 |}
                |}
            ))
    )
    |> ignore

    app.MapGet(
        "/time",
        Func<IClock, IResult>(fun clock ->
            Results.Json({| time = clock.Now() |}))
    )
    |> ignore

    app.MapGet(
        "/guid",
        Func<IGuidGen, IResult>(fun gen ->
            Results.Json({| id = gen.NewGuid() |}))
    )
    |> ignore

    app.MapGet(
        "/combined",
        Func<IClock, IGuidGen, IResult>(fun clock gen ->
            Results.Json({| time = clock.Now(); id = gen.NewGuid() |}))
    )
    |> ignore

    app.MapGet(
        "/sum/{a:int}/{b:int}",
        Func<int, int, IResult>(fun a b -> Results.Json({| sum = a + b |}))
    )
    |> ignore

    app.Run()
    0
