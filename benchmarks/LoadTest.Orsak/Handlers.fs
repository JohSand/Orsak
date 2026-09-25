module LoadTest.Orsak.Handlers

open System
open Microsoft.AspNetCore.Http
open Orsak

type IClock =
    abstract Now: unit -> DateTimeOffset

type IClockProvider =
    abstract Clock: IClock

type IGuidGen =
    abstract NewGuid: unit -> Guid

type IGuidGenProvider =
    abstract GuidGen: IGuidGen

let ping () : Effect<#IClockProvider, IResult, string> = eff {
    return Results.Ok("pong")
}

/// Work that has already completed, bound by asyncCompleted.
let completed = System.Threading.Tasks.Task.FromResult "pong"

/// An effect whose async work has already completed: the task it binds is shared by all requests.
let asyncCompleted () : Effect<#IClockProvider, IResult, string> = eff {
    let! message = completed
    return Results.Ok(message)
}

/// An effect that suspends, and resumes on the thread pool.
let asyncYield () : Effect<#IClockProvider, IResult, string> = eff {
    do! System.Threading.Tasks.Task.Yield()
    return Results.Ok("pong")
}

let pingById (id: int) : Effect<#IClockProvider, IResult, string> = eff {
    return Results.Json({| id = id; message = "pong" |})
}

let json () : Effect<#IClockProvider, IResult, string> = eff {
    return Results.Json(
        {|
            message = "hello"
            items = [| 1; 2; 3 |]
            nested = {| name = "test"; value = 42 |}
        |})
}

let time () : Effect<#IClockProvider, IResult, string> =
    Effect.Create(fun (p: #IClockProvider) ->
        Results.Json({| time = p.Clock.Now() |}))

let guid () : Effect<#IGuidGenProvider, IResult, string> =
    Effect.Create(fun (p: #IGuidGenProvider) ->
        Results.Json({| id = p.GuidGen.NewGuid() |}))

let combined () : Effect<#IClockProvider & #IGuidGenProvider, IResult, string> = eff {
    let! clock = Effect.Create(fun (p: #IClockProvider) -> p.Clock.Now())
    let! id = Effect.Create(fun (p: #IGuidGenProvider) -> p.GuidGen.NewGuid())
    return Results.Json({| time = clock; id = id |})
}

/// Two route values, tupled: RouteGet.
let sum (a: int, b: int) : Effect<#IClockProvider, IResult, string> = eff {
    return Results.Json({| sum = a + b |})
}

/// Two route values, curried: RouteGet2.
let sumCurried (a: int) (b: int) : Effect<#IClockProvider, IResult, string> = eff {
    return Results.Json({| sum = a + b |})
}
