open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

open Orsak
open System.Threading.Tasks

#nowarn "3511"

module Old =
    open FSharp.Control
    let inline par (eff: Effect<'r, 'a, 'e> seq) =
        mkEffect (fun rEnv -> vtask {
            let! results =
                eff
                |> Seq.map (Effect.run rEnv)
                |> Seq.map (fun (t: AsyncResult<'a, 'e>) -> t.AsTask())
                |> Task.WhenAll

            return
                Ok []
                |> Array.foldBack
                    (fun curr agg ->
                        match agg with
                        | Ok(list: 'a list) ->
                            match curr with
                            | Ok a -> Ok(a :: list)
                            | Error e -> Error e
                        | Error e -> Error e)
                    results
        })
[<MemoryDiagnoser>]
type SyncBenchmarks() =
    static member CompletedEffectsSource() =
        [
            [| for _ in 1..10 -> Effect.ret<unit, unit, unit> () |]
            [| for _ in 1..100 -> Effect.ret<unit, unit, unit> () |]
            [| for _ in 1..1000 -> Effect.ret<unit, unit, unit> () |]

        ] :> seq<_>


    [<DefaultValue>]
    [< ParamsSource("CompletedEffectsSource" ) >]
    val mutable CompletedEffects: Effect<unit, unit, unit> array

    [<Benchmark(Baseline=true)>]
    member this.CompletedOld() = task {
        let! a = Old.par this.CompletedEffects |> Effect.run ()
        return ()
    }

    [<Benchmark()>]
    member this.CompletedNew() = task {
        let! a = Effect.whenAll this.CompletedEffects |> Effect.run ()
        return ()
    }

module AsyncBenchmarks =
    let yieldEffect : Effect<unit, int, unit> =
        eff {
            do! Task.Yield()
            return 1
        }

    let asyncEffect : Effect<unit, int, unit> =
        eff {
            do! Task.Delay 100
            return 1
        }



[<MemoryDiagnoser>]
type AsyncYieldBenchmarks() =
    static member Sauce() : seq<Effect<unit, int, unit> array> = [
        [| for _ in 1..10 -> AsyncBenchmarks.yieldEffect |]
        [| for _ in 1..100 -> AsyncBenchmarks.yieldEffect |]
        [| for _ in 1..1000 -> AsyncBenchmarks.yieldEffect |]
    ]

    [<Benchmark(Baseline=true)>]
    [<ArgumentsSource(nameof(AsyncYieldBenchmarks.Sauce))>]
    member this.CompletedOld(effects: Effect<unit, int, unit> array) = task {
        let! a = Old.par effects |> Effect.run ()
        match a with
        | Ok a ->
            return ()
        | Error _ ->
            return()
    }

    [<Benchmark>]
    [<ArgumentsSource(nameof(AsyncYieldBenchmarks.Sauce))>]
    member this.CompletedNew(effects: Effect<unit, int, unit> array) = task {
        let! a = Effect.whenAll effects |> Effect.run ()
        match a with
        | Ok a ->
            return ()
        | Error _ ->
            return()
    }

[<MemoryDiagnoser>]
type AsyncBenchmarks() =
    static member Sauce() : seq<Effect<unit, int, unit> array> = [
        [| for _ in 1..10 -> AsyncBenchmarks.asyncEffect |]
        [| for _ in 1..20 -> AsyncBenchmarks.asyncEffect |]
    ]

    [<Benchmark(Baseline=true)>]
    [<ArgumentsSource(nameof(AsyncBenchmarks.Sauce))>]
    member this.CompletedOld(effects: Effect<unit, int, unit> array) = task {
        let! a = Old.par effects |> Effect.run ()
        match a with
        | Ok a ->
            return ()
        | Error _ ->
            return()
    }

    [<Benchmark>]
    [<ArgumentsSource(nameof(AsyncBenchmarks.Sauce))>]
    member this.CompletedNew(effects: Effect<unit, int, unit> array) = task {
        let! a = Effect.whenAll effects |> Effect.run ()
        match a with
        | Ok a ->
            return ()
        | Error _ ->
            return()
    }

[<AutoOpen>]
module Helpers2 =
    let runOrFail (e: Effect<_, _, _>) = e.RunOrFail()

    let evaluatesToSequence (es: EffSeq<unit, 'a, string>) = task {
        let res = ResizeArray<_>()

        do!
            eff {
                for a in es do
                    res.Add(a)

                return ()
            }
            |> runOrFail

        return List.ofSeq res
    }
[<MemoryDiagnoser>]
type AsyncSeqYieldBenchmarks() =
    [<Benchmark(Baseline=true)>]
    member this.CompletedOld() = task {

        let! _unused = evaluatesToSequence (effSeq {
            yield 1
            do! Task.Yield()
            yield 2
            do! Task.Yield()
            yield 3
        })

        return ()
    }

[<EntryPoint>]
let main argv =
    //asyncMain().GetAwaiter().GetResult()
    BenchmarkSwitcher
        .FromAssembly(typeof<AsyncBenchmarks>.Assembly).Run(argv)
        |> ignore

    0
