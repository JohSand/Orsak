/// Per-request costs of Orsak.AspNetCore's endpoint routing, measured on the library's own code:
///
///     dotnet run -c Release --project benchmarks/Orsak.Benchmarks -- --filter "*Routing*" --affinity 1
///
/// On a CPU with performance and efficiency cores, pin to a performance core (--affinity), or results
/// vary between runs.
module Orsak.Benchmarks.RoutingBenchmarks

open System
open System.Threading.Tasks
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Routing
open Microsoft.AspNetCore.Routing.Patterns
open Orsak
open Orsak.AspNetCore
open Orsak.AspNetCore.V2
open Orsak.AspNetCore.Helpers

let handler (a: int) (b: string) (c: Guid) = a + b.Length + c.GetHashCode()
let handlerTupled (a: int, b: string, c: Guid) = a + b.Length + c.GetHashCode()

/// Calling a handler with the parsed route values, once they are in an array.
[<MemoryDiagnoser>]
type RoutingInvokers() =
    let f: int -> string -> Guid -> int = handler
    let fTupled: int * string * Guid -> int = handlerTupled
    let witness = Unchecked.defaultof<RouteHandler>
    let ctor = createCtorFunc<int * string * Guid> ()
    let id = Guid.NewGuid()
    let args: obj array = [| box 42; box "route"; box id |]

    [<Benchmark(Baseline = true)>]
    member _.Direct() = f 42 "route" id

    /// v2, RouteGet2: RouteHandler.Apply unboxes the values into the curried handler, without a tuple.
    [<Benchmark>]
    member _.V2Curried() =
        RouteHandler.Invoke(f, Unchecked.defaultof<int * string * Guid>, args, witness)

    /// v1, RouteGet: constructing the tuple, then one call.
    [<Benchmark>]
    member _.V1Tupled() = fTupled (ctor.Invoke args)

/// Reading and parsing a request's route values with the parsers chosen for the endpoint.
[<MemoryDiagnoser>]
type RoutingRouteValues() =
    let names = [| "a"; "b"; "c" |]
    let parsers = routeValueParsers "/items/%i/%s/%O"
    let ctx = DefaultHttpContext()

    do
        ctx.Request.RouteValues["a"] <- "42"
        ctx.Request.RouteValues["b"] <- "route"
        ctx.Request.RouteValues["c"] <- string (Guid.NewGuid())

    [<Benchmark>]
    member _.ThreeValues() =
        let args = Array.zeroCreate 3

        for i = 0 to 2 do
            args[i] <- readRouteValue parsers[i] names[i] ctx

        args

/// Runs a route handler's effect, standing in for an application's runner.
type Runner =
    | Runner

    static member inline ( *>> )(effect: Effect<unit, int, string>, Runner) : RequestDelegate =
        RequestDelegate(fun ctx ->
            let result = Effect.run () effect

            if result.IsCompletedSuccessfully then
                ctx.Items["result"] <- box result.Result
                Task.CompletedTask
            else
                result.AsTask() :> Task)

let sum (a: int) (b: int) : Effect<unit, int, string> = eff { return a + b }
let sumTupled (a: int, b: int) : Effect<unit, int, string> = eff { return a + b }
let double (a: int) : Effect<unit, int, string> = eff { return a * 2 }
let constant () : Effect<unit, int, string> = eff { return 42 }

/// A whole request delegate as built by RouteGet (v1) and RouteGet2 (v2), invoked with a request's route
/// values set: Orsak's per-request overhead, without Kestrel or ASP.NET routing. Grouped by the number of
/// route values, with v1 as the baseline of each group.
[<MemoryDiagnoser>]
[<CategoriesColumn>]
[<GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)>]
type RoutingRequestDelegates() =
    let delegateOf (Endpoint e) = e.requestDelegate
    let v1None = delegateOf (Runner.RouteGet("/constant", constant))
    let v2None = delegateOf (Runner.RouteGet2("/constant", constant))
    let v1One = delegateOf (Runner.RouteGet("/double/%i", double))
    let v2One = delegateOf (Runner.RouteGet2("/double/%i", double))
    let v1Two = delegateOf (Runner.RouteGet("/sum/%i/%i", sumTupled))
    let v2Two = delegateOf (Runner.RouteGet2("/sum/%i/%i", sum))
    let ctx = DefaultHttpContext()

    do
        ctx.Request.RouteValues["a"] <- "1"
        ctx.Request.RouteValues["b"] <- "2"

    /// No route values: v1 runs the effect its handler returned when the endpoint was created.
    [<Benchmark(Baseline = true); BenchmarkCategory("0 values")>]
    member _.V1NoValues() = v1None.Invoke ctx

    /// No route values: v2 calls its unit handler for each request.
    [<Benchmark; BenchmarkCategory("0 values")>]
    member _.V2NoValues() = v2None.Invoke ctx

    [<Benchmark(Baseline = true); BenchmarkCategory("1 value")>]
    member _.V1OneValue() = v1One.Invoke ctx

    [<Benchmark; BenchmarkCategory("1 value")>]
    member _.V2OneValue() = v2One.Invoke ctx

    [<Benchmark(Baseline = true); BenchmarkCategory("2 values")>]
    member _.V1TwoValues() = v1Two.Invoke ctx

    [<Benchmark; BenchmarkCategory("2 values")>]
    member _.V2TwoValues() = v2Two.Invoke ctx
