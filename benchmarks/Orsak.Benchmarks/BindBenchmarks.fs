/// What binding an effect in eff { } costs today, against a bind on the delegate itself, which the compiler can
/// inline into the state machine:
///
///     dotnet run -c Release --project benchmarks/Orsak.Benchmarks -- --filter "*BindInlining*" --affinity 1
///
/// On a CPU with performance and efficiency cores, pin to a performance core (--affinity), or results
/// vary between runs.
module Orsak.Benchmarks.BindBenchmarks

open System.Threading.Tasks
open Microsoft.FSharp.Core.CompilerServices
open BenchmarkDotNet.Attributes
open BenchmarkDotNet.Configs
open FSharp.Control
open Orsak

type IFoo =
    abstract Foo: unit -> int
    abstract Add: int -> int

[<Sealed>]
type Env() =
    let mutable n = 0

    interface IFoo with
        member _.Foo() =
            n <- n + 1
            n

        member _.Add x =
            n <- n + x
            n

// The candidate, not part of Orsak: Effect is a struct around the delegate, and InlineIfLambda can't be put on
// a parameter of that type, so the bind takes the delegate. It only exists in this file.
type EffBuilderBase with
    member inline _.Bind<'Env, 'TOverall, 'TResult1, 'TResult2, 'Err>
        (
            [<InlineIfLambda>] d: EffectDelegate<'Env, 'TResult1, 'Err>,
            [<InlineIfLambda>] continuation: 'TResult1 -> EffectCode<'Env, 'TOverall, 'TResult2, 'Err>
        ) : EffectCode<'Env, 'TOverall, 'TResult2, 'Err> =
        EffectCode<'Env, 'TOverall, _, 'Err>(fun sm ->
            let task = d.Invoke sm.Data.Environment

            if __useResumableCode<obj> then
                let mutable awaiter = task.GetAwaiter()
                let mutable __stack_fin = true

                if not awaiter.IsCompleted then
                    let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                    __stack_fin <- __stack_yield_fin

                if __stack_fin then
                    match awaiter.GetResult() with
                    | Ok result -> (continuation result).Invoke(&sm)
                    | Error error ->
                        sm.Data.Result <- Error error
                        true
                else
                    sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                    false
            else
                failwith "The delegate bind has no dynamic implementation, build with Optimize=true.")

let inline mkDelegate ([<InlineIfLambda>] f: 'r -> ValueTask<Result<'a, 'e>>) =
    EffectDelegate<'r, 'a, 'e>(fun r -> f r)

/// Effects from ordinary functions, as they are written today.
module Current =
    let foo () : Effect<IFoo, int, string> =
        mkEffect (fun (env: IFoo) -> ValueTask<Result<int, string>>(Ok(env.Foo())))

    let add (x: int) : Effect<IFoo, int, string> =
        mkEffect (fun (env: IFoo) -> ValueTask<Result<int, string>>(Ok(env.Add x)))

    let fooVtask () : Effect<IFoo, int, string> =
        mkEffect (fun (env: IFoo) -> vtask { return Ok(env.Foo()) })

/// Today's mkEffect behind an inline function, which the compiler doesn't see through.
module CurrentInline =
    let inline foo () : Effect<IFoo, int, string> =
        mkEffect (fun (env: IFoo) -> ValueTask<Result<int, string>>(Ok(env.Foo())))

/// The delegate behind inline functions.
module Inlined =
    let inline foo () : EffectDelegate<IFoo, int, string> =
        mkDelegate (fun (env: IFoo) -> ValueTask<Result<int, string>>(Ok(env.Foo())))

    let inline add (x: int) : EffectDelegate<IFoo, int, string> =
        mkDelegate (fun (env: IFoo) -> ValueTask<Result<int, string>>(Ok(env.Add x)))

    let inline fooVtask () : EffectDelegate<IFoo, int, string> =
        mkDelegate (fun (env: IFoo) -> vtask { return Ok(env.Foo()) })

module Cases =
    let oneEffect () : Effect<IFoo, int, string> = eff {
        let! x = Current.foo ()
        return x + 1
    }

    let oneEffectInlineHelper () : Effect<IFoo, int, string> = eff {
        let! x = CurrentInline.foo ()
        return x + 1
    }

    let oneDelegate () : Effect<IFoo, int, string> = eff {
        let! x = Inlined.foo ()
        return x + 1
    }

    let fiveEffect () : Effect<IFoo, int, string> = eff {
        let! a = Current.foo ()
        let! b = Current.foo ()
        let! c = Current.foo ()
        let! d = Current.foo ()
        let! e = Current.foo ()
        return a + b + c + d + e
    }

    let fiveDelegate () : Effect<IFoo, int, string> = eff {
        let! a = Inlined.foo ()
        let! b = Inlined.foo ()
        let! c = Inlined.foo ()
        let! d = Inlined.foo ()
        let! e = Inlined.foo ()
        return a + b + c + d + e
    }

    let capturingEffect (n: int) : Effect<IFoo, int, string> = eff {
        let! x = Current.add n
        return x + 1
    }

    let capturingDelegate (n: int) : Effect<IFoo, int, string> = eff {
        let! x = Inlined.add n
        return x + 1
    }

    let vtaskEffect () : Effect<IFoo, int, string> = eff {
        let! x = Current.fooVtask ()
        return x + 1
    }

    let vtaskDelegate () : Effect<IFoo, int, string> = eff {
        let! x = Inlined.fooVtask ()
        return x + 1
    }

/// Creating and running an eff { } whose binds all complete synchronously, with today's bind as the baseline
/// of each group.
[<MemoryDiagnoser>]
[<CategoriesColumn>]
[<GroupBenchmarksBy(BenchmarkLogicalGroupRule.ByCategory)>]
type BindInlining() =
    let env = Env()
    let mutable n = 3

    [<Benchmark(Baseline = true); BenchmarkCategory("1 bind")>]
    member _.OneEffect() = (Cases.oneEffect ()).Run(env).Result

    [<Benchmark; BenchmarkCategory("1 bind")>]
    member _.OneEffectInlineHelper() =
        (Cases.oneEffectInlineHelper ()).Run(env).Result

    [<Benchmark; BenchmarkCategory("1 bind")>]
    member _.OneDelegate() = (Cases.oneDelegate ()).Run(env).Result

    [<Benchmark(Baseline = true); BenchmarkCategory("5 binds")>]
    member _.FiveEffect() = (Cases.fiveEffect ()).Run(env).Result

    [<Benchmark; BenchmarkCategory("5 binds")>]
    member _.FiveDelegate() = (Cases.fiveDelegate ()).Run(env).Result

    /// The effect's function closes over an argument.
    [<Benchmark(Baseline = true); BenchmarkCategory("capturing")>]
    member _.CapturingEffect() = (Cases.capturingEffect n).Run(env).Result

    [<Benchmark; BenchmarkCategory("capturing")>]
    member _.CapturingDelegate() = (Cases.capturingDelegate n).Run(env).Result

    /// The effect's function is a vtask { }, a state machine of its own.
    [<Benchmark(Baseline = true); BenchmarkCategory("vtask body")>]
    member _.VtaskBodyEffect() = (Cases.vtaskEffect ()).Run(env).Result

    [<Benchmark; BenchmarkCategory("vtask body")>]
    member _.VtaskBodyDelegate() = (Cases.vtaskDelegate ()).Run(env).Result
