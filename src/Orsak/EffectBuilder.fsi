namespace Orsak

open System.Buffers

#nowarn "57"
#nowarn "3513"
#nowarn "3511"

open System
open System.Threading.Tasks
open System.Collections.Generic
open System.Runtime.CompilerServices

open Microsoft.FSharp.Core
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Core.CompilerServices.StateMachineHelpers
open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators

/// <exclude/>
[<Class>]
[<Extension>]
type TaskHelper =
    [<Extension>]
    static member inline Merge< ^a, ^err when ^err: (static member (+): ^err * ^err -> ^err)> :
        err: ^err * res: Result< ^a, ^err > -> ^err when ^err: (static member (+): ^err * ^err -> ^err)

type AsyncResult<'a, 'e> = ValueTask<Result<'a, 'e>>

type EffectDelegate<'r, 'a, 'e> = delegate of 'r -> AsyncResult<'a, 'e>

/// <summary>
/// Describes an effect that can either succeed with <typeparamref name="'r"/>, or fails with <typeparamref name="'e"/>.
/// The effect is is 'cold', and only starts when run with an <typeparamref name="'r"/>.
/// </summary>
/// <typeparam name="'r" > The environment required to run the effect </typeparam>
/// <typeparam name="'a" > The resulting type when the effect runs successfully </typeparam>
/// <typeparam name="'e" > The resulting type when the effect fails</typeparam>
/// <returns></returns>
[<Struct; NoComparison; NoEquality>]
type Effect<'r, 'a, 'e> =
    | Effect of EffectDelegate<'r, 'a, 'e>

    /// <summary>
    /// Starts the effect.
    /// <returns>A <see cref="ValueTask"/> that can be awaited to get the result of the effect</returns>
    /// </summary>
    member inline Run: r: 'R -> AsyncResult<'T, 'E>
    /// <summary>
    /// Starts the effect, and raises an exception in case of error. Prefer <see cref="Run"/>.
    /// <returns>A <see cref="ValueTask"/> that can be awaited to get the result of the effect</returns>
    /// </summary>
    member inline RunOrFail: r: 'R -> Task<'T>

/// <exclude/>
[<Struct; NoComparison; NoEquality>]
type EffectStateMachineData<'Env, 'T, 'Err> =
    [<DefaultValue(false)>]
    val mutable Result: Result<'T, 'Err>

    [<DefaultValue(false)>]
    val mutable Environment: 'Env

    [<DefaultValue(false)>]
    val mutable MethodBuilder: AsyncValueTaskMethodBuilder<Result<'T, 'Err>>

/// <exclude/>
and EffectStateMachine<'Env, 'TOverall, 'Err> = ResumableStateMachine<EffectStateMachineData<'Env, 'TOverall, 'Err>>
/// <exclude/>
and EffectResumptionFunc<'Env, 'TOverall, 'Err> = ResumptionFunc<EffectStateMachineData<'Env, 'TOverall, 'Err>>

/// <exclude/>
and EffectResumptionDynamicInfo<'Env, 'TOverall, 'Err> =
    ResumptionDynamicInfo<EffectStateMachineData<'Env, 'TOverall, 'Err>>

/// <exclude/>
and EffectCode<'Env, 'TOverall, 'T, 'Err> = ResumableCode<EffectStateMachineData<'Env, 'TOverall, 'Err>, 'T>

/// <exclude/>
type EffBuilderBase =
    new: unit -> EffBuilderBase
    static member inline GetResumptionFunc: sm: byref<ResumableStateMachine<'Data>> -> ResumptionFunc<'Data>

    member inline Delay:
        generator: (unit -> EffectCode<'Env, 'TOverall, 'T, 'Err>) -> EffectCode<'Env, 'TOverall, 'T, 'Err>

    [<DefaultValue>]
    member inline Zero: unit -> EffectCode<'Env, 'TOverall, unit, 'Err>

    static member CombineDynamic:
        sm: byref<ResumableStateMachine<EffectStateMachineData<'a42, 'TOverall, 'Err>>> *
        code1: EffectCode<'a42, 'TOverall, unit, 'Err> *
        code2: EffectCode<'a42, 'TOverall, 'T, 'Err> ->
            bool

    member inline Combine:
        task1: EffectCode<'a41, 'TOverall, unit, 'Err> * task2: EffectCode<'a41, 'TOverall, 'T, 'Err> ->
            EffectCode<'a41, 'TOverall, 'T, 'Err>

    static member WhileDynamic:
        sm: byref<EffectStateMachine<'Env, 'TOverall, 'Err>> *
        condition: (unit -> bool) *
        body: EffectCode<'Env, 'TOverall, unit, 'Err> ->
            bool

    static member WhileBodyDynamicAux:
        sm: byref<EffectStateMachine<'Env, 'TOverall, 'Err>> *
        condition: (unit -> bool) *
        body: EffectCode<'Env, 'TOverall, unit, 'Err> *
        rf: ResumptionFunc<EffectStateMachineData<'Env, 'TOverall, 'Err>> ->
            bool

    member inline While:
        [<InlineIfLambda>] condition: (unit -> bool) * body: EffectCode<'Env, 'TOverall, unit, 'Err> ->
            EffectCode<'Env, 'TOverall, unit, 'Err>

    member inline TryWith:
        body: EffectCode<'a39, 'TOverall, 'T, 'a40> * catch: (exn -> EffectCode<'a39, 'TOverall, 'T, 'a40>) ->
            EffectCode<'a39, 'TOverall, 'T, 'a40>

    member inline TryFinally:
        body: EffectCode<'a37, 'TOverall, 'T, 'a38> * [<InlineIfLambda>] compensation: (unit -> unit) ->
            EffectCode<'a37, 'TOverall, 'T, 'a38>

    member inline internal TryFinallyAsync:
        body: EffectCode<'Env, 'TOverall, 'T, 'Err> * compensation: (unit -> ValueTask) ->
            EffectCode<'Env, 'TOverall, 'T, 'Err>

    member inline Using<'Resource, 'TOverall, 'T, 'Env, 'Err when 'Resource :> IAsyncDisposable> :
        resource: 'Resource * body: ('Resource -> EffectCode<'Env, 'TOverall, 'T, 'Err>) ->
            EffectCode<'Env, 'TOverall, 'T, 'Err>
            when 'Resource :> IAsyncDisposable

    member inline Return: value: 'T -> EffectCode<'Env, 'T, 'T, 'Err>

    static member inline RecoverDynamic:
        sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TResult, 'Err>>> *
        task: ValueTask<Result<'TResult, 'Err>> *
        continuation: ('Err -> 'TResult) ->
            bool

    member inline Recover:
        eff: Effect<'Env, 'TResult, 'Err> * [<InlineIfLambda>] continuation: ('Err -> 'TResult) ->
            EffectCode<'Env, 'TResult, 'TResult, 'Err>

    static member inline ChangeErrorDynamic<'Env, 'TResult, 'Err1, 'Err2> :
        sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TResult, 'Err2>>> *
        task: ValueTask<Result<'TResult, 'Err1>> *
        f: ('Err1 -> 'Err2) ->
            bool

    member inline ChangeError:
        eff: Effect<'Env, 'TResult, 'Err1> * f: ('Err1 -> 'Err2) -> EffectCode<'Env, 'TResult, 'TResult, 'Err2>

    static member inline TryRecoverDynamic:
        sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TResult, 'Err>>> *
        task: ValueTask<Result<'TResult, 'Err>> *
        continuation: ('Err -> Result<'TResult, 'Err>) ->
            bool

    member inline TryRecover:
        eff: Effect<'Env, 'TResult, 'Err> * [<InlineIfLambda>] continuation: ('Err -> Result<'TResult, 'Err>) ->
            EffectCode<'Env, 'TResult, 'TResult, 'Err>

    static member inline TryRecoverDynamic:
        sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TResult, 'Err>>> *
        task: ValueTask<Result<'TResult, 'Err>> *
        continuation: ('Err -> Effect<'Env, 'TResult, 'Err>) ->
            bool

    member inline TryRecover:
        eff: Effect<'Env, 'TResult, 'Err> * [<InlineIfLambda>] continuation: ('Err -> Effect<'Env, 'TResult, 'Err>) ->
            EffectCode<'Env, 'TResult, 'TResult, 'Err>

    static member inline ZipDynamic<'Env, 'TOverall, 'TResult1, 'TResult2, ^Err
        when ^Err: (static member (+): ^Err * ^Err -> ^Err)> :
        sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TOverall, ^Err>>> *
        task1: ValueTask<Result<'TResult1, ^Err>> *
        task2: ValueTask<Result<'TResult2, ^Err>> *
        f: ('TResult1 * 'TResult2 -> 'TOverall) ->
            bool
            when ^Err: (static member (+): ^Err * ^Err -> ^Err)

    member inline Zip<'Env, 'TOverall, 'TResult1, 'TResult2, ^Err when ^Err: (static member (+): ^Err * ^Err -> ^Err)> :
        eff: Effect<'Env, 'TResult1, ^Err> *
        eff2: Effect<'Env, 'TResult2, ^Err> *
        f: ('TResult1 * 'TResult2 -> 'TOverall) ->
            EffectCode<'Env, 'TOverall, 'TOverall, ^Err>
            when ^Err: (static member (+): ^Err * ^Err -> ^Err)

    static member inline ZipDynamic<'Env, 'TOverall, 'TResult1, 'TResult2, 'TResult3, ^Err
        when ^Err: (static member (+): ^Err * ^Err -> ^Err)> :
        sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TOverall, ^Err>>> *
        task1: ValueTask<Result<'TResult1, ^Err>> *
        task2: ValueTask<Result<'TResult2, ^Err>> *
        task3: ValueTask<Result<'TResult3, ^Err>> *
        f: ('TResult1 * 'TResult2 * 'TResult3 -> 'TOverall) ->
            bool
            when ^Err: (static member (+): ^Err * ^Err -> ^Err)

    member inline Zip<'Env, 'TOverall, 'TResult1, 'TResult2, 'TResult3, ^Err
        when ^Err: (static member (+): ^Err * ^Err -> ^Err)> :
        eff: Effect<'Env, 'TResult1, ^Err> *
        eff2: Effect<'Env, 'TResult2, ^Err> *
        eff3: Effect<'Env, 'TResult3, ^Err> *
        f: ('TResult1 * 'TResult2 * 'TResult3 -> 'TOverall) ->
            EffectCode<'Env, 'TOverall, 'TOverall, ^Err>
            when ^Err: (static member (+): ^Err * ^Err -> ^Err)

    static member inline ZipDynamic<'Env, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'TResult4, ^Err
        when ^Err: (static member (+): ^Err * ^Err -> ^Err)> :
        sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TOverall, ^Err>>> *
        task1: ValueTask<Result<'TResult1, ^Err>> *
        task2: ValueTask<Result<'TResult2, ^Err>> *
        task3: ValueTask<Result<'TResult3, ^Err>> *
        task4: ValueTask<Result<'TResult4, ^Err>> *
        f: ('TResult1 * 'TResult2 * 'TResult3 * 'TResult4 -> 'TOverall) ->
            bool
            when ^Err: (static member (+): ^Err * ^Err -> ^Err)

    member inline Zip<'Env, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'TResult4, ^Err
        when ^Err: (static member (+): ^Err * ^Err -> ^Err)> :
        eff1: Effect<'Env, 'TResult1, ^Err> *
        eff2: Effect<'Env, 'TResult2, ^Err> *
        eff3: Effect<'Env, 'TResult3, ^Err> *
        eff4: Effect<'Env, 'TResult4, ^Err> *
        f: ('TResult1 * 'TResult2 * 'TResult3 * 'TResult4 -> 'TOverall) ->
            EffectCode<'Env, 'TOverall, 'TOverall, ^Err>
            when ^Err: (static member (+): ^Err * ^Err -> ^Err)

    static member inline ZipDynamic<'Env, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'TResult4, 'TResult5, ^Err
        when ^Err: (static member (+): ^Err * ^Err -> ^Err)> :
        sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TOverall, ^Err>>> *
        task1: ValueTask<Result<'TResult1, ^Err>> *
        task2: ValueTask<Result<'TResult2, ^Err>> *
        task3: ValueTask<Result<'TResult3, ^Err>> *
        task4: ValueTask<Result<'TResult4, ^Err>> *
        task5: ValueTask<Result<'TResult5, ^Err>> *
        f: ('TResult1 * 'TResult2 * 'TResult3 * 'TResult4 * 'TResult5 -> 'TOverall) ->
            bool
            when ^Err: (static member (+): ^Err * ^Err -> ^Err)

    member inline Zip<'Env, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'TResult4, 'TResult5, ^Err
        when ^Err: (static member (+): ^Err * ^Err -> ^Err)> :
        eff1: Effect<'Env, 'TResult1, ^Err> *
        eff2: Effect<'Env, 'TResult2, ^Err> *
        eff3: Effect<'Env, 'TResult3, ^Err> *
        eff4: Effect<'Env, 'TResult4, ^Err> *
        eff5: Effect<'Env, 'TResult5, ^Err> *
        f: ('TResult1 * 'TResult2 * 'TResult3 * 'TResult4 * 'TResult5 -> 'TOverall) ->
            EffectCode<'Env, 'TOverall, 'TOverall, ^Err>
            when ^Err: (static member (+): ^Err * ^Err -> ^Err)

    static member inline ZipDynamic<'Env, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'TResult4, 'TResult5, 'TResult6, ^Err
        when ^Err: (static member (+): ^Err * ^Err -> ^Err)> :
        sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TOverall, ^Err>>> *
        task1: ValueTask<Result<'TResult1, ^Err>> *
        task2: ValueTask<Result<'TResult2, ^Err>> *
        task3: ValueTask<Result<'TResult3, ^Err>> *
        task4: ValueTask<Result<'TResult4, ^Err>> *
        task5: ValueTask<Result<'TResult5, ^Err>> *
        task6: ValueTask<Result<'TResult6, ^Err>> *
        f: ('TResult1 * 'TResult2 * 'TResult3 * 'TResult4 * 'TResult5 * 'TResult6 -> 'TOverall) ->
            bool
            when ^Err: (static member (+): ^Err * ^Err -> ^Err)

    member inline Zip<'Env, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'TResult4, 'TResult5, 'TResult6, ^Err
        when ^Err: (static member (+): ^Err * ^Err -> ^Err)> :
        eff1: Effect<'Env, 'TResult1, ^Err> *
        eff2: Effect<'Env, 'TResult2, ^Err> *
        eff3: Effect<'Env, 'TResult3, ^Err> *
        eff4: Effect<'Env, 'TResult4, ^Err> *
        eff5: Effect<'Env, 'TResult5, ^Err> *
        eff6: Effect<'Env, 'TResult6, ^Err> *
        f: ('TResult1 * 'TResult2 * 'TResult3 * 'TResult4 * 'TResult5 * 'TResult6 -> 'TOverall) ->
            EffectCode<'Env, 'TOverall, 'TOverall, ^Err>
            when ^Err: (static member (+): ^Err * ^Err -> ^Err)

    static member inline HandleResults:
        sm: byref<ResumableStateMachine<EffectStateMachineData<'a34, 'a35, ^a36>>> *
        result1: Result<'TResult1, ^a36> *
        result2: Result<'TResult2, ^a36> *
        result3: Result<'TResult3, ^a36> *
        result4: Result<'TResult4, ^a36> *
        result5: Result<'TResult5, ^a36> *
        result6: Result<'TResult6, ^a36> *
        result7: Result<'TResult7, ^a36> *
        f: ('TResult1 * 'TResult2 * 'TResult3 * 'TResult4 * 'TResult5 * 'TResult6 * 'TResult7 -> 'a35) ->
            bool
            when ^a36: (static member (+): ^a36 * ^a36 -> ^a36)

    static member inline HandleResultsBind:
        sm: byref<ResumableStateMachine<EffectStateMachineData<'a30, 'a31, ^a32>>> *
        result1: Result<'TResult1, ^a32> *
        result2: Result<'TResult2, ^a32> *
        result3: Result<'TResult3, ^a32> *
        result4: Result<'TResult4, ^a32> *
        result5: Result<'TResult5, ^a32> *
        result6: Result<'TResult6, ^a32> *
        result7: Result<'TResult7, ^a32> *
        f:
            ('TResult1 * 'TResult2 * 'TResult3 * 'TResult4 * 'TResult5 * 'TResult6 * 'TResult7
                -> EffectCode<'a30, 'a31, 'a33, ^a32>) ->
            bool
            when ^a32: (static member (+): ^a32 * ^a32 -> ^a32)

    static member inline ZipDynamic<'Env, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'TResult4, 'TResult5, 'TResult6, 'TResult7, ^Err
        when ^Err: (static member (+): ^Err * ^Err -> ^Err)> :
        sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TOverall, ^Err>>> *
        task1: ValueTask<Result<'TResult1, ^Err>> *
        task2: ValueTask<Result<'TResult2, ^Err>> *
        task3: ValueTask<Result<'TResult3, ^Err>> *
        task4: ValueTask<Result<'TResult4, ^Err>> *
        task5: ValueTask<Result<'TResult5, ^Err>> *
        task6: ValueTask<Result<'TResult6, ^Err>> *
        task7: ValueTask<Result<'TResult7, ^Err>> *
        f: ('TResult1 * 'TResult2 * 'TResult3 * 'TResult4 * 'TResult5 * 'TResult6 * 'TResult7 -> 'TOverall) ->
            bool
            when ^Err: (static member (+): ^Err * ^Err -> ^Err)

    member inline Zip<'Env, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'TResult4, 'TResult5, 'TResult6, 'TResult7, ^Err
        when ^Err: (static member (+): ^Err * ^Err -> ^Err)> :
        eff1: Effect<'Env, 'TResult1, ^Err> *
        eff2: Effect<'Env, 'TResult2, ^Err> *
        eff3: Effect<'Env, 'TResult3, ^Err> *
        eff4: Effect<'Env, 'TResult4, ^Err> *
        eff5: Effect<'Env, 'TResult5, ^Err> *
        eff6: Effect<'Env, 'TResult6, ^Err> *
        eff7: Effect<'Env, 'TResult7, ^Err> *
        f: ('TResult1 * 'TResult2 * 'TResult3 * 'TResult4 * 'TResult5 * 'TResult6 * 'TResult7 -> 'TOverall) ->
            EffectCode<'Env, 'TOverall, 'TOverall, ^Err>
            when ^Err: (static member (+): ^Err * ^Err -> ^Err)

    member inline Bind2Return:
        m1: Effect<'a25, 'a26, ^a27> * m2: Effect<'a25, 'a28, ^a27> * [<InlineIfLambda>] f: ('a26 * 'a28 -> 'a29) ->
            EffectCode<'a25, 'a29, 'a29, ^a27>
            when ^a27: (static member (+): ^a27 * ^a27 -> ^a27)

    member inline Bind3Return:
        m1: Effect<'a19, 'a20, ^a21> *
        m2: Effect<'a19, 'a22, ^a21> *
        m3: Effect<'a19, 'a23, ^a21> *
        [<InlineIfLambda>] f: ('a20 * 'a22 * 'a23 -> 'a24) ->
            EffectCode<'a19, 'a24, 'a24, ^a21>
            when ^a21: (static member (+): ^a21 * ^a21 -> ^a21)

    member inline Bind4Return:
        m1: Effect<'a12, 'a13, ^a14> *
        m2: Effect<'a12, 'a15, ^a14> *
        m3: Effect<'a12, 'a16, ^a14> *
        m4: Effect<'a12, 'a17, ^a14> *
        [<InlineIfLambda>] f: ('a13 * 'a15 * 'a16 * 'a17 -> 'a18) ->
            EffectCode<'a12, 'a18, 'a18, ^a14>
            when ^a14: (static member (+): ^a14 * ^a14 -> ^a14)

    member inline Bind5Return:
        m1: Effect<'a4, 'a5, ^a6> *
        m2: Effect<'a4, 'a7, ^a6> *
        m3: Effect<'a4, 'a8, ^a6> *
        m4: Effect<'a4, 'a9, ^a6> *
        m5: Effect<'a4, 'a10, ^a6> *
        [<InlineIfLambda>] f: ('a5 * 'a7 * 'a8 * 'a9 * 'a10 -> 'a11) ->
            EffectCode<'a4, 'a11, 'a11, ^a6>
            when ^a6: (static member (+): ^a6 * ^a6 -> ^a6)

    member inline Bind6Return:
        m1: Effect<'o, 'p, ^q> *
        m2: Effect<'o, 'r, ^q> *
        m3: Effect<'o, 's, ^q> *
        m4: Effect<'o, 't, ^q> *
        m5: Effect<'o, 'a1, ^q> *
        m6: Effect<'o, 'a2, ^q> *
        [<InlineIfLambda>] f: ('p * 'r * 's * 't * 'a1 * 'a2 -> 'a3) ->
            EffectCode<'o, 'a3, 'a3, ^q>
            when ^q: (static member (+): ^q * ^q -> ^q)

    member inline Bind7Return:
        m1: Effect<'e, 'f, ^g> *
        m2: Effect<'e, 'h, ^g> *
        m3: Effect<'e, 'i, ^g> *
        m4: Effect<'e, 'j, ^g> *
        m5: Effect<'e, 'k, ^g> *
        m6: Effect<'e, 'l, ^g> *
        m7: Effect<'e, 'm, ^g> *
        [<InlineIfLambda>] f: ('f * 'h * 'i * 'j * 'k * 'l * 'm -> 'n) ->
            EffectCode<'e, 'n, 'n, ^g>
            when ^g: (static member (+): ^g * ^g -> ^g)

    static member inline Bind2Dynamic<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, ^Err
        when ^Err: (static member (+): ^Err * ^Err -> ^Err)> :
        sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TOverall, ^Err>>> *
        task1: ValueTask<Result<'TResult1, ^Err>> *
        task2: ValueTask<Result<'TResult2, ^Err>> *
        f: (struct ('TResult1 * 'TResult2) -> EffectCode<'Env, 'TOverall, 'T, ^Err>) ->
            bool
            when ^Err: (static member (+): ^Err * ^Err -> ^Err)

    member inline Bind2<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, ^Err
        when ^Err: (static member (+): ^Err * ^Err -> ^Err)> :
        eff: Effect<'Env, 'TResult1, ^Err> *
        eff2: Effect<'Env, 'TResult2, ^Err> *
        [<InlineIfLambda>] f: (struct ('TResult1 * 'TResult2) -> EffectCode<'Env, 'TOverall, 'T, ^Err>) ->
            EffectCode<'Env, 'TOverall, 'T, ^Err>
            when ^Err: (static member (+): ^Err * ^Err -> ^Err)

    static member inline Bind3<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'TResult3, ^Err
        when ^Err: (static member (+): ^Err * ^Err -> ^Err)> :
        sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TOverall, ^Err>>> *
        task1: ValueTask<Result<'TResult1, ^Err>> *
        task2: ValueTask<Result<'TResult2, ^Err>> *
        task3: ValueTask<Result<'TResult3, ^Err>> *
        [<InlineIfLambda>] f: ('TResult1 * 'TResult2 * 'TResult3 -> EffectCode<'Env, 'TOverall, 'T, ^Err>) ->
            bool
            when ^Err: (static member (+): ^Err * ^Err -> ^Err)

    member inline Bind3<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'TResult3, ^Err
        when ^Err: (static member (+): ^Err * ^Err -> ^Err)> :
        eff: Effect<'Env, 'TResult1, ^Err> *
        eff2: Effect<'Env, 'TResult2, ^Err> *
        eff3: Effect<'Env, 'TResult3, ^Err> *
        [<InlineIfLambda>] f: ('TResult1 * 'TResult2 * 'TResult3 -> EffectCode<'Env, 'TOverall, 'T, ^Err>) ->
            EffectCode<'Env, 'TOverall, 'T, ^Err>
            when ^Err: (static member (+): ^Err * ^Err -> ^Err)

    static member inline Bind4<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'TResult4, ^Err
        when ^Err: (static member (+): ^Err * ^Err -> ^Err)> :
        sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TOverall, ^Err>>> *
        task1: ValueTask<Result<'TResult1, ^Err>> *
        task2: ValueTask<Result<'TResult2, ^Err>> *
        task3: ValueTask<Result<'TResult3, ^Err>> *
        task4: ValueTask<Result<'TResult4, ^Err>> *
        [<InlineIfLambda>] f: ('TResult1 * 'TResult2 * 'TResult3 * 'TResult4 -> EffectCode<'Env, 'TOverall, 'T, ^Err>) ->
            bool
            when ^Err: (static member (+): ^Err * ^Err -> ^Err)

    member inline Bind4<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'TResult4, ^Err
        when ^Err: (static member (+): ^Err * ^Err -> ^Err)> :
        eff1: Effect<'Env, 'TResult1, ^Err> *
        eff2: Effect<'Env, 'TResult2, ^Err> *
        eff3: Effect<'Env, 'TResult3, ^Err> *
        eff4: Effect<'Env, 'TResult4, ^Err> *
        [<InlineIfLambda>] f: ('TResult1 * 'TResult2 * 'TResult3 * 'TResult4 -> EffectCode<'Env, 'TOverall, 'T, ^Err>) ->
            EffectCode<'Env, 'TOverall, 'T, ^Err>
            when ^Err: (static member (+): ^Err * ^Err -> ^Err)

    static member inline Bind5<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'TResult4, 'TResult5, ^Err
        when ^Err: (static member (+): ^Err * ^Err -> ^Err)> :
        sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TOverall, ^Err>>> *
        task1: ValueTask<Result<'TResult1, ^Err>> *
        task2: ValueTask<Result<'TResult2, ^Err>> *
        task3: ValueTask<Result<'TResult3, ^Err>> *
        task4: ValueTask<Result<'TResult4, ^Err>> *
        task5: ValueTask<Result<'TResult5, ^Err>> *
        [<InlineIfLambda>] f:
            ('TResult1 * 'TResult2 * 'TResult3 * 'TResult4 * 'TResult5 -> EffectCode<'Env, 'TOverall, 'T, ^Err>) ->
            bool
            when ^Err: (static member (+): ^Err * ^Err -> ^Err)

    member inline Bind5<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'TResult4, 'TResult5, ^Err
        when ^Err: (static member (+): ^Err * ^Err -> ^Err)> :
        eff1: Effect<'Env, 'TResult1, ^Err> *
        eff2: Effect<'Env, 'TResult2, ^Err> *
        eff3: Effect<'Env, 'TResult3, ^Err> *
        eff4: Effect<'Env, 'TResult4, ^Err> *
        eff5: Effect<'Env, 'TResult5, ^Err> *
        [<InlineIfLambda>] f:
            ('TResult1 * 'TResult2 * 'TResult3 * 'TResult4 * 'TResult5 -> EffectCode<'Env, 'TOverall, 'T, ^Err>) ->
            EffectCode<'Env, 'TOverall, 'T, ^Err>
            when ^Err: (static member (+): ^Err * ^Err -> ^Err)

    static member inline Bind6Dynamic<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'TResult4, 'TResult5, 'TResult6, ^Err
        when ^Err: (static member (+): ^Err * ^Err -> ^Err)> :
        sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TOverall, ^Err>>> *
        task1: ValueTask<Result<'TResult1, ^Err>> *
        task2: ValueTask<Result<'TResult2, ^Err>> *
        task3: ValueTask<Result<'TResult3, ^Err>> *
        task4: ValueTask<Result<'TResult4, ^Err>> *
        task5: ValueTask<Result<'TResult5, ^Err>> *
        task6: ValueTask<Result<'TResult6, ^Err>> *
        [<InlineIfLambda>] f:
            ('TResult1 * 'TResult2 * 'TResult3 * 'TResult4 * 'TResult5 * 'TResult6
                -> EffectCode<'Env, 'TOverall, 'T, ^Err>) ->
            bool
            when ^Err: (static member (+): ^Err * ^Err -> ^Err)

    member inline Bind6<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'TResult4, 'TResult5, 'TResult6, ^Err
        when ^Err: (static member (+): ^Err * ^Err -> ^Err)> :
        eff1: Effect<'Env, 'TResult1, ^Err> *
        eff2: Effect<'Env, 'TResult2, ^Err> *
        eff3: Effect<'Env, 'TResult3, ^Err> *
        eff4: Effect<'Env, 'TResult4, ^Err> *
        eff5: Effect<'Env, 'TResult5, ^Err> *
        eff6: Effect<'Env, 'TResult6, ^Err> *
        [<InlineIfLambda>] f:
            ('TResult1 * 'TResult2 * 'TResult3 * 'TResult4 * 'TResult5 * 'TResult6
                -> EffectCode<'Env, 'TOverall, 'T, ^Err>) ->
            EffectCode<'Env, 'TOverall, 'T, ^Err>
            when ^Err: (static member (+): ^Err * ^Err -> ^Err)

    static member inline Bind7Dynamic<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'TResult4, 'TResult5, 'TResult6, 'TResult7, ^Err
        when ^Err: (static member (+): ^Err * ^Err -> ^Err)> :
        sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TOverall, ^Err>>> *
        task1: ValueTask<Result<'TResult1, ^Err>> *
        task2: ValueTask<Result<'TResult2, ^Err>> *
        task3: ValueTask<Result<'TResult3, ^Err>> *
        task4: ValueTask<Result<'TResult4, ^Err>> *
        task5: ValueTask<Result<'TResult5, ^Err>> *
        task6: ValueTask<Result<'TResult6, ^Err>> *
        task7: ValueTask<Result<'TResult7, ^Err>> *
        [<InlineIfLambda>] f:
            ('TResult1 * 'TResult2 * 'TResult3 * 'TResult4 * 'TResult5 * 'TResult6 * 'TResult7
                -> EffectCode<'Env, 'TOverall, 'T, ^Err>) ->
            bool
            when ^Err: (static member (+): ^Err * ^Err -> ^Err)

    member inline Bind7<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'TResult4, 'TResult5, 'TResult6, 'TResult7, ^Err
        when ^Err: (static member (+): ^Err * ^Err -> ^Err)> :
        eff1: Effect<'Env, 'TResult1, ^Err> *
        eff2: Effect<'Env, 'TResult2, ^Err> *
        eff3: Effect<'Env, 'TResult3, ^Err> *
        eff4: Effect<'Env, 'TResult4, ^Err> *
        eff5: Effect<'Env, 'TResult5, ^Err> *
        eff6: Effect<'Env, 'TResult6, ^Err> *
        eff7: Effect<'Env, 'TResult7, ^Err> *
        [<InlineIfLambda>] f:
            ('TResult1 * 'TResult2 * 'TResult3 * 'TResult4 * 'TResult5 * 'TResult6 * 'TResult7
                -> EffectCode<'Env, 'TOverall, 'T, ^Err>) ->
            EffectCode<'Env, 'TOverall, 'T, ^Err>
            when ^Err: (static member (+): ^Err * ^Err -> ^Err)

    static member WhenAllDynamic:
        sm: byref<EffectStateMachine<'b, 'c array, 'd>> *
        waits: Memory<ValueTaskAwaiter<Result<'c, 'd>>> *
        results: ResizeArray<Result<'c, 'd>> ->
            bool

    static member WhenAllDynamicAux:
        sm: byref<ResumableStateMachine<EffectStateMachineData<'b, 'c array, 'd>>> *
        waits: Memory<ValueTaskAwaiter<Result<'c, 'd>>> *
        results: ResizeArray<Result<'c, 'd>> *
        rf: ResumptionFunc<EffectStateMachineData<'b, 'c array, 'd>> ->
            bool

    static member inline AwaiterNotComplete: v: inref<ValueTaskAwaiter<'a>> -> bool

    static member inline CreateWaiters:
        effects: Effect<'Env, 'T, 'Err> seq ->
        env: 'Env ->
            struct (ValueTaskAwaiter<Result<'T, 'Err>> array * int * bool)

    member inline WhenAll: effects: Effect<'Env, 'T, 'Err> seq -> EffectCode<'Env, 'T array, 'T array, 'Err>

/// <exclude/>
type EffBuilder =
    new: unit -> EffBuilder
    inherit EffBuilderBase
    static member inline RunDynamic: code: EffectCode<'Env, 'T, 'T, 'Err> -> Effect<'Env, 'T, 'Err>

    [<NoEagerConstraintApplication>]
    member inline Run: code: EffectCode<'Env, 'T, 'T, 'Err> -> Effect<'Env, 'T, 'Err>

/// <exclude/>
[<AutoOpen>]
module Builder =
    val inline mkEffect: d: ('a -> AsyncResult<'b, 'c>) -> Effect<'a, 'b, 'c>
    val eff: EffBuilder

/// <exclude/>
[<AutoOpen>]
module LowPriority =
    type EffBuilderBase with
        [<NoEagerConstraintApplication>]
        static member inline BindDynamic:
            sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TOverall, 'Err>>> *
            task: ^TaskLike *
            continuation: ('TResult1 -> EffectCode<'Env, 'TOverall, 'TResult2, 'Err>) ->
                bool
                when ^TaskLike: (member GetAwaiter: unit -> ^Awaiter)
                and ^Awaiter :> ICriticalNotifyCompletion
                and ^Awaiter: (member get_IsCompleted: unit -> bool)
                and ^Awaiter: (member GetResult: unit -> 'TResult1)

        [<NoEagerConstraintApplication>]
        member inline Bind:
            task: ^TaskLike * continuation: ('TResult1 -> EffectCode<'Env, 'TOverall, 'TResult2, 'Err>) ->
                EffectCode<'Env, 'TOverall, 'TResult2, 'Err>
                when ^TaskLike: (member GetAwaiter: unit -> ^Awaiter)
                and ^Awaiter :> ICriticalNotifyCompletion
                and ^Awaiter: (member get_IsCompleted: unit -> bool)
                and ^Awaiter: (member GetResult: unit -> 'TResult1)

/// <exclude/>
[<AutoOpen>]
module Medium =
    open FSharp.Control

    type EffBuilderBase with
        member inline Using:
            resource: ^Resource * body: (^Resource -> EffectCode<'Env, 'TOverall, 'T, 'Err>) ->
                EffectCode<'Env, 'TOverall, 'T, 'Err>
                when ^Resource: (member DisposeAsync: unit -> ^TaskLike)
                and ^Resource: struct
                and ^TaskLike: (member GetAwaiter: unit -> ^Awaiter)
                and ^Awaiter :> ICriticalNotifyCompletion
                and ^Awaiter: (member get_IsCompleted: unit -> bool)
                and ^Awaiter: (member GetResult: unit -> unit)

        static member inline BindDynamic:
            sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TOverall, 'Err>>> *
            task: ValueTask<'TResult1> *
            continuation: ('TResult1 -> EffectCode<'Env, 'TOverall, 'TResult2, 'Err>) ->
                bool

        static member inline BindDynamic:
            sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TOverall, 'Err>>> *
            task: ValueTask<Result<'TResult1, 'Err>> *
            continuation: ('TResult1 -> EffectCode<'Env, 'TOverall, 'TResult2, 'Err>) ->
                bool

        [<NoEagerConstraintApplication>]
        member inline Bind<'Env, 'TOverall, 'TResult1, 'TResult2, 'Err> :
            eff: Effect<'Env, 'TResult1, 'Err> *
            continuation: ('TResult1 -> EffectCode<'Env, 'TOverall, 'TResult2, 'Err>) ->
                EffectCode<'Env, 'TOverall, 'TResult2, 'Err>

        member inline Bind<'Env, 'TOverall, 'TResult1, 'TResult2, 'Err> :
            task: ValueTask<'TResult1> * continuation: ('TResult1 -> EffectCode<'Env, 'TOverall, 'TResult2, 'Err>) ->
                EffectCode<'Env, 'TOverall, 'TResult2, 'Err>

        member inline Bind<'Env, 'TOverall, 'TResult1, 'TResult2, 'Err> :
            t: Task<'TResult1> * continuation: ('TResult1 -> EffectCode<'Env, 'TOverall, 'TResult2, 'Err>) ->
                EffectCode<'Env, 'TOverall, 'TResult2, 'Err>

        member inline Bind<'Env, 'TOverall, 'TResult1, 'TResult2, 'Err> :
            a: Async<'TResult1> * continuation: ('TResult1 -> EffectCode<'Env, 'TOverall, 'TResult2, 'Err>) ->
                EffectCode<'Env, 'TOverall, 'TResult2, 'Err>

        member inline Bind:
            result: Result<'T, 'Err> * [<InlineIfLambda>] f: ('T -> EffectCode<'Env, 'TOverall, 'T2, 'Err>) ->
                EffectCode<'Env, 'TOverall, 'T2, 'Err>

        member inline ReturnFrom: task: Effect<'Env, 'T, 'Err> -> EffectCode<'Env, 'T, 'T, 'Err>
        member inline ReturnFrom: a: Async<'T> -> EffectCode<'Env, 'T, 'T, 'Err>
        member inline ReturnFrom: result: Result<'T, 'Err> -> EffectCode<'Env, 'T, 'T, 'Err>

        member inline Using<'Resource, 'TOverall, 'T, 'Env, 'Err when 'Resource :> IDisposable> :
            resource: 'Resource * [<InlineIfLambda>] body: ('Resource -> EffectCode<'Env, 'TOverall, 'T, 'Err>) ->
                ResumableCode<EffectStateMachineData<'Env, 'TOverall, 'Err>, 'T>
                when 'Resource :> IDisposable

        member inline For:
            s: IAsyncEnumerable<'a> * [<InlineIfLambda>] f1: ('a -> EffectCode<'a0, 'b, unit, 'c>) ->
                EffectCode<'a0, 'b, unit, 'c>

        member inline For:
            s: ConfiguredCancelableAsyncEnumerable<'a> * [<InlineIfLambda>] f: ('a -> EffectCode<'a0, 'b, unit, 'c>) ->
                EffectCode<'a0, 'b, unit, 'c>

        member inline While<'a, 'c, 'b> :
            [<InlineIfLambda>] condition: (unit -> Effect<'a, bool, 'c>) * body: EffectCode<'a, 'b, unit, 'c> ->
                EffectCode<'a, 'b, unit, 'c>

        member inline While<'a, 'b, 'c> :
            [<InlineIfLambda>] condition: (unit -> ValueTask<bool>) * body: EffectCode<'a, 'b, unit, 'c> ->
                EffectCode<'a, 'b, unit, 'c>

        member inline For:
            s: IEnumerable<'a> * [<InlineIfLambda>] f1: ('a -> EffectCode<'Env, 'TOverall, unit, 'Err>) ->
                EffectCode<'Env, 'TOverall, unit, 'Err>

        member inline For:
            struct ('a * 'a) * [<InlineIfLambda>] f: ('a -> EffectCode<'Env, 'TOverall, unit, 'Err>) ->
                EffectCode<'Env, 'TOverall, unit, 'Err>

        member inline For:
            struct ('a * 'a * 'a) * [<InlineIfLambda>] f: ('a -> EffectCode<'Env, 'TOverall, unit, 'Err>) ->
                EffectCode<'Env, 'TOverall, unit, 'Err>

        member inline For:
            struct ('a * 'a * 'a * 'a) * [<InlineIfLambda>] f: ('a -> EffectCode<'Env, 'TOverall, unit, 'Err>) ->
                EffectCode<'Env, 'TOverall, unit, 'Err>

        member inline For:
            struct ('a * 'a * 'a * 'a * 'a) * [<InlineIfLambda>] f: ('a -> EffectCode<'Env, 'TOverall, unit, 'Err>) ->
                EffectCode<'Env, 'TOverall, unit, 'Err>

        member inline For:
            struct ('a * 'a * 'a * 'a * 'a * 'a) *
            [<InlineIfLambda>] fn: ('a -> EffectCode<'Env, 'TOverall, unit, 'Err>) ->
                EffectCode<'Env, 'TOverall, unit, 'Err>

        member inline For:
            struct ('a * 'a * 'a * 'a * 'a * 'a * 'a) *
            [<InlineIfLambda>] fn: ('a -> EffectCode<'Env, 'TOverall, unit, 'Err>) ->
                EffectCode<'Env, 'TOverall, unit, 'Err>

[<NoComparison>]
type Effect<'R, 'T, 'E> with
    ///Implements Map on the effect, making it a Functor
    static member inline Map: h: Effect<'r, 'a, 'e> * f: ('a -> 'b) -> Effect<'r, 'b, 'e>
    ///Implements Return on the effect
    static member inline Return: a: 'a -> Effect<'b, 'a, 'c>
    ///Implements Bind on the effect, which together with the functor Return makes it a Monad
    static member inline (>>=): h: Effect<'r, 'a, 'e> * f: ('a -> Effect<'r, 'b, 'e>) -> Effect<'r, 'b, 'e>

    /// <exclude/>
    static member inline ap<'r, 'a, 'b, 'e> :
        applicative: Effect<'r, ('b -> 'a), 'e> -> e: Effect<'r, 'b, 'e> -> Effect<'r, 'a, 'e>

    ///Implements Apply on the effect, making it an applicative
    static member inline (<*>):
        f: Effect<'r, ('b -> 'a), ^e> * e: Effect<'r, 'b, ^e> -> Effect<'r, 'a, ^e>
            when ^e: (static member (+): ^e * ^e -> ^e)
