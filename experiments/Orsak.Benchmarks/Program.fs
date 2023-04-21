open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

open Orsak
open System.Threading.Tasks



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
        let! a = Effect.par this.CompletedEffects |> Effect.run ()
        return ()
    }

    [<Benchmark()>]
    member this.CompletedNew() = task {
        let! a = Experiment.par this.CompletedEffects |> Effect.run ()
        return ()
    }


[<MemoryDiagnoser>]
type AsyncBenchmarks() =
    [<DefaultValue>]
    [<Params(10, 100)>]
    val mutable N: int

    static member MkEffect () =
        eff {
            do! Task.Yield()
            return 1
        }


    [<Benchmark(Baseline=true)>]
    member this.CompletedOld() = task {
        let effects =
            [|
                for _ in 1..this.N -> AsyncBenchmarks.MkEffect ()
            |]
        let! a = Effect.par effects |> Effect.run ()
        match a with
        | Ok a ->
            return ()
        | Error _ ->
            return()
    }

    [<Benchmark()>]
    member this.CompletedNew() = task {
        let effects =
            [|
                for _ in 1..this.N -> AsyncBenchmarks.MkEffect ()
            |]
        let! a = Experiment.par effects|> Effect.run ()
        match a with
        | Ok a ->
            return ()
        | Error _ ->
            return()
    }


[<EntryPoint>]
let main argv =
    

    BenchmarkSwitcher
        .FromAssembly(typeof<AsyncBenchmarks>.Assembly).Run(argv)
        |> ignore

    0
