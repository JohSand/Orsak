/// Per-request costs of Orsak.Remoting.AspNetCore's MapRemote endpoints, called in process, without a server, for
/// each kind of member a contract has:
///
///     dotnet run -c Release --project benchmarks/Orsak.Benchmarks -- --filter "*Remoting*" --affinity 1
///
/// On a CPU with performance and efficiency cores, pin to a performance core (--affinity), or results
/// vary between runs.
module Orsak.Benchmarks.RemotingBenchmarks

open System.IO
open System.Threading.Tasks
open BenchmarkDotNet.Attributes
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Routing
open Microsoft.Extensions.Logging
open Orsak.Remoting
open Orsak.Remoting.AspNetCore

type IBenchApi =
    abstract TaskOf: n: int -> Task<int>
    abstract ValueTaskOf: n: int -> ValueTask<int>
    abstract AsyncOf: n: int -> Async<int>
    abstract TaskOfNone: n: int -> Task
    abstract Value: n: int -> int

/// Answers at once, so what's measured is the endpoint's own work.
type BenchApi() =
    interface IBenchApi with
        member _.TaskOf n = Task.FromResult(n + 1)
        member _.ValueTaskOf n = ValueTask<int>(n + 1)
        member _.AsyncOf n = async { return n + 1 }
        member _.TaskOfNone _ = Task.CompletedTask
        member _.Value n = n + 1

/// One call to a member's endpoint: reading its argument, calling it, awaiting it and writing its result.
[<MemoryDiagnoser>]
type RemotingEndpoints() =
    let format = JsonFormat.Instance :> IWireFormat
    let body = format.ArgumentsCodecFor([| typeof<int> |]).Encode([| box 41 |])
    let mutable app = Unchecked.defaultof<WebApplication>
    let mutable endpoints = Map.empty<string, RequestDelegate>

    let context () =
        let ctx = DefaultHttpContext()
        ctx.RequestServices <- app.Services
        ctx.Request.ContentType <- format.ContentType
        ctx.Request.Body <- new MemoryStream(body: byte array)
        ctx.Response.Body <- Stream.Null
        ctx

    let call name = endpoints[name].Invoke(context ())

    [<GlobalSetup>]
    member _.Setup() =
        let builder = WebApplication.CreateSlimBuilder()
        builder.Logging.ClearProviders() |> ignore
        app <- builder.Build()
        let api = BenchApi()
        app.MapRemote<IBenchApi>("/api", [ format ], fun _ -> api :> IBenchApi) |> ignore

        endpoints <-
            (app :> IEndpointRouteBuilder).DataSources
            |> Seq.collect _.Endpoints
            |> Seq.choose (function
                | :? RouteEndpoint as e -> Some(e.RoutePattern.RawText.Split('/') |> Array.last, e.RequestDelegate)
                | _ -> None)
            |> Map.ofSeq

        // what's measured is a call that succeeds, not a 415 or a 500
        for KeyValue(name, endpoint) in endpoints do
            let ctx = context ()
            endpoint.Invoke(ctx).Wait()

            if ctx.Response.StatusCode <> 200 then
                failwith $"{name} answered {ctx.Response.StatusCode}"

    [<GlobalCleanup>]
    member _.Cleanup() = (app :> System.IAsyncDisposable).DisposeAsync().AsTask().Wait()

    [<Benchmark(Baseline = true)>]
    member _.Value() = call "Value"

    [<Benchmark>]
    member _.TaskOf() = call "TaskOf"

    [<Benchmark>]
    member _.ValueTaskOf() = call "ValueTaskOf"

    [<Benchmark>]
    member _.AsyncOf() = call "AsyncOf"

    [<Benchmark>]
    member _.TaskOfNone() = call "TaskOfNone"
