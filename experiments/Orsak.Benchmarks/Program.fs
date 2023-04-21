open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Running

open Orsak
open System.Threading.Tasks


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
        let! a = Old.par effects |> Effect.run ()
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
        let! a = Effect.whenAll effects|> Effect.run ()
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
