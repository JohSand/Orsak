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
[<Extension>]
type TaskHelper =
    [<Extension>]
    static member inline Merge< ^a, ^err when ^err: (static member (+): ^err -> ^err -> ^err)>
        (
            err: ^err,
            res: Result< ^a, ^err >
        ) =
        match res with
        | Ok _ -> err
        | Error e -> err + e

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
    member inline this.Run(r: 'r) = let (Effect d) = this in d.Invoke r

    /// <summary>
    /// Starts the effect, and raises an exception in case of error. Prefer <see cref="Run"/>.
    /// <returns>A <see cref="ValueTask"/> that can be awaited to get the result of the effect</returns>
    /// </summary>
    member inline this.RunOrFail(r: 'r) =
        let (Effect d) = this

        task {
            match! d.Invoke r with
            | Ok a -> return a
            | Error e -> return failwith (e.ToString())
        }


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
type EffBuilderBase() =
    static member inline GetResumptionFunc(sm: byref<ResumableStateMachine<'Data>>) =
        sm.ResumptionDynamicInfo.ResumptionFunc

    member inline _.Delay
        (generator: unit -> EffectCode<'Env, 'TOverall, 'T, 'Err>)
        : EffectCode<'Env, 'TOverall, 'T, 'Err> =
        EffectCode<'Env, 'TOverall, 'T, 'Err>(fun sm -> (generator ()).Invoke(&sm))

    [<DefaultValue>]
    member inline _.Zero() : EffectCode<'Env, 'TOverall, unit, 'Err> = ResumableCode.Zero()

    static member CombineDynamic
        (
            sm: byref<ResumableStateMachine<EffectStateMachineData<_, _, _>>>,
            code1: EffectCode<_, 'TOverall, unit, 'Err>,
            code2: EffectCode<_, 'TOverall, 'T, 'Err>
        ) : bool =
        if code1.Invoke(&sm) then
            code2.Invoke(&sm)
        else
            let rec resume (mf: ResumptionFunc<EffectStateMachineData<_, _, _>>) =
                ResumptionFunc<EffectStateMachineData<_, _, _>>(fun sm ->
                    if mf.Invoke(&sm) then
                        match sm.Data.Result with
                        | Ok _ -> code2.Invoke(&sm)
                        | Error _ -> true
                    else
                        sm.ResumptionDynamicInfo.ResumptionFunc <- (resume (EffBuilderBase.GetResumptionFunc &sm))
                        false)

            sm.ResumptionDynamicInfo.ResumptionFunc <- (resume (EffBuilderBase.GetResumptionFunc &sm))
            false

    member inline _.Combine
        (
            task1: EffectCode<_, 'TOverall, unit, 'Err>,
            task2: EffectCode<_, 'TOverall, 'T, 'Err>
        ) : EffectCode<_, 'TOverall, 'T, 'Err> =
        ResumableCode(fun sm ->
            if __useResumableCode then
                let __stack_fin = task1.Invoke(&sm)

                if __stack_fin then
                    match sm.Data.Result with
                    | Ok _ -> task2.Invoke(&sm)
                    | Error _ -> true

                else
                    false
            else
                EffBuilderBase.CombineDynamic(&sm, task1, task2))

    static member WhileDynamic
        (
            sm: byref<EffectStateMachine<'Env, 'TOverall, 'Err>>,
            condition: unit -> bool,
            body: EffectCode<'Env, 'TOverall, unit, 'Err>
        ) : bool =
        if condition () then
            if body.Invoke(&sm) then
                match sm.Data.Result with
                | Ok _ -> EffBuilderBase.WhileDynamic(&sm, condition, body)
                | Error _ -> true //early break out
            else
                let rf = EffBuilderBase.GetResumptionFunc &sm

                sm.ResumptionDynamicInfo.ResumptionFunc <-
                    ResumptionFunc<_>(fun sm -> EffBuilderBase.WhileBodyDynamicAux(&sm, condition, body, rf))

                false
        else
            true

    static member WhileBodyDynamicAux
        (
            sm: byref<EffectStateMachine<'Env, 'TOverall, 'Err>>,
            condition: unit -> bool,
            body: EffectCode<'Env, 'TOverall, unit, 'Err>,
            rf: ResumptionFunc<_>
        ) : bool =
        if rf.Invoke(&sm) then
            EffBuilderBase.WhileDynamic(&sm, condition, body)
        else
            let rf = EffBuilderBase.GetResumptionFunc &sm

            sm.ResumptionDynamicInfo.ResumptionFunc <-
                ResumptionFunc<_>(fun sm -> EffBuilderBase.WhileBodyDynamicAux(&sm, condition, body, rf))

            false

    member inline _.While
        (
            [<InlineIfLambda>] condition: unit -> bool,
            body: EffectCode<'Env, 'TOverall, unit, 'Err>
        ) : EffectCode<'Env, 'TOverall, unit, 'Err> =
        EffectCode<'Env, 'TOverall, unit, 'Err>(fun sm ->
            if __useResumableCode then
                let mutable __stack_go = true
                let mutable cc = true

                while __stack_go && condition () && cc do
                    let __stack_body_fin = body.Invoke(&sm)

                    if __stack_body_fin then
                        __stack_go <- __stack_body_fin

                        match sm.Data.Result with
                        | Ok _ -> ()
                        | Error _ -> cc <- false
                    else
                        __stack_go <- false

                __stack_go
            else
                EffBuilderBase.WhileDynamic(&sm, condition, body))

    member inline _.TryWith
        (
            body: EffectCode<_, 'TOverall, 'T, _>,
            catch: exn -> EffectCode<_, 'TOverall, 'T, _>
        ) : EffectCode<_, 'TOverall, 'T, _> =
        ResumableCode.TryWith(body, catch)

    member inline _.TryFinally
        (
            body: EffectCode<_, 'TOverall, 'T, _>,
            [<InlineIfLambda>] compensation: unit -> unit
        ) : EffectCode<_, 'TOverall, 'T, _> =
        ResumableCode.TryFinally(
            body,
            ResumableCode<_, _>(fun _sm ->
                compensation ()
                true)
        )

    member inline internal this.TryFinallyAsync
        (
            body: EffectCode<'Env, 'TOverall, 'T, 'Err>,
            compensation: unit -> ValueTask
        ) : EffectCode<'Env, 'TOverall, 'T, 'Err> =
        ResumableCode.TryFinallyAsync(
            body,
            ResumableCode<_, _>(fun sm ->
                if __useResumableCode then
                    let mutable __stack_condition_fin = true
                    let __stack_vtask = compensation ()

                    if not __stack_vtask.IsCompleted then
                        let mutable awaiter = __stack_vtask.GetAwaiter()
                        let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                        __stack_condition_fin <- __stack_yield_fin

                        if not __stack_condition_fin then
                            sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)

                    __stack_condition_fin
                else
                    let vtask = compensation ()
                    let mutable awaiter = vtask.GetAwaiter()

                    let cont =
                        EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->
                            awaiter.GetResult()
                            true)

                    // shortcut to continue immediately
                    if awaiter.IsCompleted then
                        true
                    else
                        sm.ResumptionDynamicInfo.ResumptionData <- (awaiter :> ICriticalNotifyCompletion)
                        sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                        false)
        )

    member inline this.Using<'Resource, 'TOverall, 'T, 'Env, 'Err when 'Resource :> IAsyncDisposable>
        (
            resource: 'Resource,
            body: 'Resource -> EffectCode<'Env, 'TOverall, 'T, 'Err>
        ) : EffectCode<'Env, 'TOverall, 'T, 'Err> =
        this.TryFinallyAsync(
            (fun sm -> (body resource).Invoke(&sm)),
            (fun () ->
                if not (isNull (box resource)) then
                    resource.DisposeAsync()
                else
                    ValueTask())
        )

    member inline _.Return(value: 'T) : EffectCode<'Env, 'T, 'T, 'Err> =
        EffectCode<'Env, 'T, _, 'Err>(fun sm ->
            sm.Data.Result <- Ok value
            true)

    //--------------------------------------------------------------------
    // Errors and recovery
    //--------------------------------------------------------------------
    static member inline RecoverDynamic<'Env, 'T, 'TOverall, 'TResult, 'Err>
        (
            sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TResult, 'Err>>>,
            task: ValueTask<_>,
            continuation: 'Err -> 'TResult
        ) : bool =
        let mutable awaiter = task.GetAwaiter()

        let cont =
            EffectResumptionFunc<'Env, 'TResult, 'Err>(fun sm ->
                let result = awaiter.GetResult()

                match result with
                | Ok result ->
                    sm.Data.Result <- Ok result
                    true
                | Error error ->
                    sm.Data.Result <- Ok(continuation error)
                    true)

        if awaiter.IsCompleted then
            cont.Invoke(&sm)
        else
            sm.ResumptionDynamicInfo.ResumptionData <- (awaiter :> ICriticalNotifyCompletion)
            sm.ResumptionDynamicInfo.ResumptionFunc <- cont
            false

    member inline _.Recover<'Env, 'T, 'TOverall, 'TResult, 'Err>
        (
            eff: Effect<'Env, 'TResult, 'Err>,
            [<InlineIfLambda>] continuation: 'Err -> 'TResult
        ) : EffectCode<'Env, 'TResult, 'TResult, 'Err> =
        EffectCode<'Env, 'TResult, _, 'Err>(fun sm ->
            let task = eff.Run sm.Data.Environment

            if __useResumableCode then
                let mutable awaiter = task.GetAwaiter()

                let mutable __stack_fin = true

                if not awaiter.IsCompleted then
                    let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                    __stack_fin <- __stack_yield_fin

                if __stack_fin then
                    let result = awaiter.GetResult()

                    match result with
                    | Ok result ->
                        sm.Data.Result <- Ok result
                        true
                    | Error error ->
                        sm.Data.Result <- Ok(continuation error)
                        true
                else
                    sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                    false
            else
                EffBuilderBase.RecoverDynamic(&sm, task, continuation))

    static member inline ChangeErrorDynamic<'Env, 'T, 'TOverall, 'TResult, 'Err1, 'Err2>
        (
            sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TResult, 'Err2>>>,
            task: ValueTask<_>,
            f: 'Err1 -> 'Err2
        ) : bool =
        let mutable awaiter = task.GetAwaiter()

        let cont =
            EffectResumptionFunc<'Env, 'TResult, 'Err2>(fun sm ->
                let result = awaiter.GetResult()

                match result with
                | Ok result ->
                    sm.Data.Result <- Result<_, 'Err2>.Ok result
                    true
                | Error(error: 'Err1) ->
                    sm.Data.Result <- Result<_, 'Err2>.Error(f error)
                    true)

        if awaiter.IsCompleted then
            cont.Invoke(&sm)
        else
            sm.ResumptionDynamicInfo.ResumptionData <- (awaiter :> ICriticalNotifyCompletion)
            sm.ResumptionDynamicInfo.ResumptionFunc <- cont
            false

    member inline _.ChangeError<'Env, 'T, 'TOverall, 'TResult, 'Err1, 'Err2>
        (
            eff: Effect<'Env, 'TResult, 'Err1>,
            f: 'Err1 -> 'Err2
        ) : EffectCode<'Env, 'TResult, 'TResult, 'Err2> =
        EffectCode<'Env, 'TResult, _, 'Err2>(fun sm ->
            let task = eff.Run sm.Data.Environment

            if __useResumableCode then
                let mutable awaiter = task.GetAwaiter()

                let mutable __stack_fin = true

                if not awaiter.IsCompleted then
                    let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                    __stack_fin <- __stack_yield_fin

                if __stack_fin then
                    let result = awaiter.GetResult()

                    match result with
                    | Ok result ->
                        sm.Data.Result <- Result<_, 'Err2>.Ok result
                        true
                    | Error(error: 'Err1) ->
                        sm.Data.Result <- Result<_, 'Err2>.Error(f error)
                        true
                else
                    sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                    false
            else
                EffBuilderBase.ChangeErrorDynamic(&sm, task, f))

    static member inline TryRecoverDynamic<'Env, 'T, 'TOverall, 'TResult, 'Err>
        (
            sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TResult, 'Err>>>,
            task: ValueTask<_>,
            continuation: 'Err -> Result<'TResult, 'Err>
        ) : bool =
        let mutable awaiter = task.GetAwaiter()

        let cont =
            EffectResumptionFunc<'Env, 'TResult, 'Err>(fun sm ->
                let result = awaiter.GetResult()

                match result with
                | Ok result ->
                    sm.Data.Result <- Ok result
                    true
                | Error error ->
                    sm.Data.Result <- continuation error
                    true)

        if awaiter.IsCompleted then
            cont.Invoke(&sm)
        else
            sm.ResumptionDynamicInfo.ResumptionData <- (awaiter :> ICriticalNotifyCompletion)
            sm.ResumptionDynamicInfo.ResumptionFunc <- cont
            false

    member inline _.TryRecover<'Env, 'T, 'TOverall, 'TResult, 'Err>
        (
            eff: Effect<'Env, 'TResult, 'Err>,
            [<InlineIfLambda>] continuation: 'Err -> Result<'TResult, 'Err>
        ) : EffectCode<'Env, 'TResult, 'TResult, 'Err> =
        EffectCode<'Env, 'TResult, _, 'Err>(fun sm ->
            let task = eff.Run sm.Data.Environment

            if __useResumableCode then
                let mutable awaiter = task.GetAwaiter()

                let mutable __stack_fin = true

                if not awaiter.IsCompleted then
                    let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                    __stack_fin <- __stack_yield_fin

                if __stack_fin then
                    let result = awaiter.GetResult()

                    match result with
                    | Ok result ->
                        sm.Data.Result <- Ok result
                        true
                    | Error error ->
                        sm.Data.Result <- continuation error
                        true
                else
                    sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                    false
            else
                EffBuilderBase.TryRecoverDynamic(&sm, task, continuation))

    static member inline TryRecoverDynamic<'Env, 'T, 'TOverall, 'TResult, 'Err>
        (
            sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TResult, 'Err>>>,
            task: ValueTask<_>,
            continuation: 'Err -> Effect<'Env, 'TResult, 'Err>
        ) : bool =
        let mutable awaiter = task.GetAwaiter()

        let cont =
            EffectResumptionFunc<'Env, 'TResult, 'Err>(fun sm ->
                let result = awaiter.GetResult()

                match result with
                | Ok result ->
                    sm.Data.Result <- Ok result
                    true
                | Error error ->
                    let next = continuation error
                    let task = next.Run sm.Data.Environment
                    let mutable awaiter = task.GetAwaiter()

                    let cont =
                        EffectResumptionFunc<'Env, 'TResult, 'Err>(fun sm ->
                            sm.Data.Result <- awaiter.GetResult()
                            true)

                    if awaiter.IsCompleted then
                        cont.Invoke(&sm)
                    else
                        sm.ResumptionDynamicInfo.ResumptionData <- (awaiter :> ICriticalNotifyCompletion)
                        sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                        false)

        if awaiter.IsCompleted then
            cont.Invoke(&sm)
        else
            sm.ResumptionDynamicInfo.ResumptionData <- (awaiter :> ICriticalNotifyCompletion)
            sm.ResumptionDynamicInfo.ResumptionFunc <- cont
            false

    member inline _.TryRecover<'Env, 'T, 'TOverall, 'TResult, 'Err>
        (
            eff: Effect<'Env, 'TResult, 'Err>,
            [<InlineIfLambda>] continuation: 'Err -> Effect<'Env, 'TResult, 'Err>
        ) : EffectCode<'Env, 'TResult, 'TResult, 'Err> =
        EffectCode<'Env, 'TResult, _, 'Err>(fun sm ->
            let task1 = eff.Run sm.Data.Environment

            if __useResumableCode then
                let mutable awaiter = task1.GetAwaiter()

                let mutable __stack_fin = true

                if not awaiter.IsCompleted then
                    let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                    __stack_fin <- __stack_yield_fin

                if __stack_fin then
                    let result = awaiter.GetResult()

                    match result with
                    | Ok result ->
                        sm.Data.Result <- Ok result
                        true
                    | Error error ->
                        let task2 = (continuation error).Run sm.Data.Environment
                        let mutable awaiter = task2.GetAwaiter()

                        let mutable __stack_fin = true

                        if not awaiter.IsCompleted then
                            let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                            __stack_fin <- __stack_yield_fin

                        if __stack_fin then
                            let result = awaiter.GetResult()
                            sm.Data.Result <- result
                            true
                        else
                            sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                            false

                else
                    sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                    false
            else
                EffBuilderBase.TryRecoverDynamic(&sm, task1, continuation))


    //--------------------------------------------------------------------------
    // Zips and merges
    //--------------------------------------------------------------------------
    static member inline ZipDynamic<'Env, 'TOverall, 'TResult1, 'TResult2, 'Err
        when 'Err: (static member (+): 'Err -> 'Err -> 'Err)>
        (
            sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TOverall, 'Err>>>,
            task1: ValueTask<Result<'TResult1, 'Err>>,
            task2: ValueTask<Result<'TResult2, 'Err>>,
            f
        ) : bool =
        let mutable awaiter1 = task1.GetAwaiter()
        let mutable awaiter2 = task2.GetAwaiter()

        let cont =
            EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->
                let cont =
                    EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->
                        let result = awaiter1.GetResult()
                        let result2 = awaiter2.GetResult()

                        match result, result2 with
                        | Ok result, Ok result2 ->
                            sm.Data.Result <- Ok(f (result, result2))
                            true
                        | Error error, Ok _ ->
                            sm.Data.Result <- Error error
                            true
                        | Ok _, Error result ->
                            sm.Data.Result <- Error result
                            true
                        | Error error, Error result ->
                            sm.Data.Result <- Error(error + result)
                            true)

                if awaiter2.IsCompleted then
                    cont.Invoke(&sm)
                else
                    sm.ResumptionDynamicInfo.ResumptionData <- (awaiter2 :> ICriticalNotifyCompletion)
                    sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                    false)

        if awaiter1.IsCompleted then
            cont.Invoke(&sm)
        else
            sm.ResumptionDynamicInfo.ResumptionData <- (awaiter1 :> ICriticalNotifyCompletion)
            sm.ResumptionDynamicInfo.ResumptionFunc <- cont
            false


    member inline _.Zip<'Env, 'TOverall, 'TResult1, 'TResult2, 'Err when 'Err: (static member (+): 'Err -> 'Err -> 'Err)>
        (
            eff: Effect<'Env, 'TResult1, 'Err>,
            eff2: Effect<'Env, 'TResult2, 'Err>,
            f
        ) : EffectCode<'Env, 'TOverall, 'TOverall, 'Err> =
        EffectCode<'Env, 'TOverall, 'TOverall, 'Err>(fun sm ->
            let task = eff.Run sm.Data.Environment
            let task2 = eff2.Run sm.Data.Environment

            if __useResumableCode then
                let mutable awaiter = task.GetAwaiter()

                let mutable __stack_fin = true

                if not awaiter.IsCompleted then
                    let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                    __stack_fin <- __stack_yield_fin

                if __stack_fin then
                    let mutable awaiter2 = task2.GetAwaiter()

                    if not awaiter2.IsCompleted then
                        let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                        __stack_fin <- __stack_yield_fin

                    if __stack_fin then
                        let result = awaiter.GetResult()
                        let result2 = awaiter2.GetResult()

                        match result, result2 with
                        | Ok result, Ok result2 ->
                            sm.Data.Result <- Ok(f (result, result2))
                            true
                        | Error error, _ ->
                            sm.Data.Result <- Error(error.Merge(result2))
                            true
                        | _, Error error ->
                            sm.Data.Result <- Error(error.Merge(result))
                            true
                    else
                        sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter2, &sm)
                        false
                else
                    sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                    false
            else
                EffBuilderBase.ZipDynamic(&sm, task, task2, f))

    static member inline ZipDynamic<'Env, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'Err
        when 'Err: (static member (+): 'Err -> 'Err -> 'Err)>
        (
            sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TOverall, 'Err>>>,
            task1: ValueTask<Result<'TResult1, 'Err>>,
            task2: ValueTask<Result<'TResult2, 'Err>>,
            task3: ValueTask<Result<'TResult3, 'Err>>,
            f
        ) : bool =
        let mutable awaiter1 = task1.GetAwaiter()

        let cont =
            EffectResumptionFunc<_, _, _>(fun sm ->
                let mutable awaiter2 = task2.GetAwaiter()

                let cont =
                    EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->
                        let mutable awaiter3 = task3.GetAwaiter()

                        let cont =
                            EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->
                                let result = awaiter1.GetResult()
                                let result2 = awaiter2.GetResult()
                                let result3 = awaiter3.GetResult()

                                match result, result2, result3 with
                                | Ok result, Ok result2, Ok result3 ->
                                    sm.Data.Result <- Ok(f (result, result2, result3))
                                    true
                                | Error error, _, _ ->
                                    sm.Data.Result <- Error(error.Merge(result2).Merge(result3))
                                    true
                                | _, Error error, _ ->
                                    sm.Data.Result <- Error(error.Merge(result).Merge(result3))
                                    true
                                | _, _, Error error ->
                                    sm.Data.Result <- Error(error.Merge(result).Merge(result2))
                                    true)

                        if awaiter3.IsCompleted then
                            cont.Invoke(&sm)
                        else
                            sm.ResumptionDynamicInfo.ResumptionData <- (awaiter3 :> ICriticalNotifyCompletion)
                            sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                            false)

                if awaiter2.IsCompleted then
                    cont.Invoke(&sm)
                else
                    sm.ResumptionDynamicInfo.ResumptionData <- (awaiter2 :> ICriticalNotifyCompletion)
                    sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                    false)

        if awaiter1.IsCompleted then
            cont.Invoke(&sm)
        else
            sm.ResumptionDynamicInfo.ResumptionData <- (awaiter1 :> ICriticalNotifyCompletion)
            sm.ResumptionDynamicInfo.ResumptionFunc <- cont

            false


    member inline _.Zip<'Env, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'Err
        when 'Err: (static member (+): 'Err -> 'Err -> 'Err)>
        (
            eff: Effect<'Env, 'TResult1, 'Err>,
            eff2: Effect<'Env, 'TResult2, 'Err>,
            eff3: Effect<'Env, 'TResult3, 'Err>,
            f
        ) : EffectCode<'Env, 'TOverall, 'TOverall, 'Err> =
        EffectCode<'Env, 'TOverall, 'TOverall, 'Err>(fun sm ->
            let task = eff.Run sm.Data.Environment
            let task2 = eff2.Run sm.Data.Environment
            let task3 = eff3.Run sm.Data.Environment

            if __useResumableCode then
                let mutable awaiter = task.GetAwaiter()
                let mutable __stack_fin = true

                if not awaiter.IsCompleted then
                    let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                    __stack_fin <- __stack_yield_fin

                if __stack_fin then
                    let mutable awaiter2 = task2.GetAwaiter()

                    if not awaiter2.IsCompleted then
                        let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                        __stack_fin <- __stack_yield_fin

                    if __stack_fin then
                        let mutable awaiter3 = task3.GetAwaiter()

                        if not awaiter3.IsCompleted then
                            let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                            __stack_fin <- __stack_yield_fin

                        if __stack_fin then

                            let result1 = awaiter.GetResult()
                            let result2 = awaiter2.GetResult()
                            let result3 = awaiter3.GetResult()

                            match result1, result2, result3 with
                            | Ok result, Ok result2, Ok result3 ->
                                sm.Data.Result <- Ok(f (result, result2, result3))
                                true
                            | Error error, _, _ ->
                                sm.Data.Result <- Error(error.Merge(result2).Merge(result3))
                                true
                            | _, Error error, _ ->
                                sm.Data.Result <- Error(error.Merge(result1).Merge(result3))
                                true
                            | _, _, Error error ->
                                sm.Data.Result <- Error(error.Merge(result1).Merge(result2))
                                true
                        else
                            sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter3, &sm)
                            false

                    else
                        sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter2, &sm)
                        false
                else
                    sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                    false
            else
                EffBuilderBase.ZipDynamic(&sm, task, task2, task3, f))

    static member inline ZipDynamic<'Env, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'TResult4, 'Err
        when 'Err: (static member (+): 'Err -> 'Err -> 'Err)>
        (
            sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TOverall, 'Err>>>,
            task1: ValueTask<Result<'TResult1, 'Err>>,
            task2: ValueTask<Result<'TResult2, 'Err>>,
            task3: ValueTask<Result<'TResult3, 'Err>>,
            task4: ValueTask<Result<'TResult4, 'Err>>,
            f
        ) : bool =
        let mutable awaiter1 = task1.GetAwaiter()

        let cont =
            EffectResumptionFunc<_, _, _>(fun sm ->
                let mutable awaiter2 = task2.GetAwaiter()

                let cont =
                    EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->
                        let mutable awaiter3 = task3.GetAwaiter()

                        let cont =
                            EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->

                                let mutable awaiter4 = task4.GetAwaiter()

                                let cont =
                                    EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->
                                        let result1 = awaiter1.GetResult()
                                        let result2 = awaiter2.GetResult()
                                        let result3 = awaiter3.GetResult()
                                        let result4 = awaiter4.GetResult()

                                        match result1, result2, result3, result4 with
                                        | Ok result, Ok result2, Ok result3, Ok result4 ->
                                            sm.Data.Result <- Ok(f (result, result2, result3, result4))
                                            true
                                        | Error error, _, _, _ ->
                                            sm.Data.Result <-
                                                Error(error.Merge(result2).Merge(result3).Merge(result4))

                                            true
                                        | _, Error error, _, _ ->
                                            sm.Data.Result <-
                                                Error(error.Merge(result1).Merge(result3).Merge(result4))

                                            true
                                        | _, _, Error error, _ ->
                                            sm.Data.Result <-
                                                Error(error.Merge(result1).Merge(result2).Merge(result4))

                                            true
                                        | _, _, _, Error error ->
                                            sm.Data.Result <-
                                                Error(error.Merge(result1).Merge(result2).Merge(result3))

                                            true)

                                if awaiter4.IsCompleted then
                                    cont.Invoke(&sm)
                                else
                                    sm.ResumptionDynamicInfo.ResumptionData <-
                                        (awaiter4 :> ICriticalNotifyCompletion)

                                    sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                                    false)

                        if awaiter3.IsCompleted then
                            cont.Invoke(&sm)
                        else
                            sm.ResumptionDynamicInfo.ResumptionData <- (awaiter3 :> ICriticalNotifyCompletion)
                            sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                            false)

                if awaiter2.IsCompleted then
                    cont.Invoke(&sm)
                else
                    sm.ResumptionDynamicInfo.ResumptionData <- (awaiter2 :> ICriticalNotifyCompletion)
                    sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                    false)

        if awaiter1.IsCompleted then
            cont.Invoke(&sm)
        else
            sm.ResumptionDynamicInfo.ResumptionData <- (awaiter1 :> ICriticalNotifyCompletion)
            sm.ResumptionDynamicInfo.ResumptionFunc <- cont

            false

    member inline _.Zip<'Env, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'TResult4, 'Err
        when 'Err: (static member (+): 'Err -> 'Err -> 'Err)>
        (
            eff1: Effect<'Env, 'TResult1, 'Err>,
            eff2: Effect<'Env, 'TResult2, 'Err>,
            eff3: Effect<'Env, 'TResult3, 'Err>,
            eff4: Effect<'Env, 'TResult4, 'Err>,
            f
        ) : EffectCode<'Env, 'TOverall, 'TOverall, 'Err> =
        EffectCode<'Env, 'TOverall, 'TOverall, 'Err>(fun sm ->
            let task1 = eff1.Run sm.Data.Environment
            let task2 = eff2.Run sm.Data.Environment
            let task3 = eff3.Run sm.Data.Environment
            let task4 = eff4.Run sm.Data.Environment

            if __useResumableCode then
                let mutable awaiter = task1.GetAwaiter()
                let mutable __stack_fin = true

                if not awaiter.IsCompleted then
                    let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                    __stack_fin <- __stack_yield_fin

                if __stack_fin then
                    let mutable awaiter2 = task2.GetAwaiter()

                    if not awaiter2.IsCompleted then
                        let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                        __stack_fin <- __stack_yield_fin

                    if __stack_fin then
                        let mutable awaiter3 = task3.GetAwaiter()

                        if not awaiter3.IsCompleted then
                            let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                            __stack_fin <- __stack_yield_fin

                        if __stack_fin then
                            let mutable awaiter4 = task4.GetAwaiter()

                            if not awaiter4.IsCompleted then
                                let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                                __stack_fin <- __stack_yield_fin

                            if __stack_fin then
                                let result1 = awaiter.GetResult()
                                let result2 = awaiter2.GetResult()
                                let result3 = awaiter3.GetResult()
                                let result4 = awaiter4.GetResult()

                                match result1, result2, result3, result4 with
                                | Ok result, Ok result2, Ok result3, Ok result4 ->
                                    sm.Data.Result <- Ok(f (result, result2, result3, result4))
                                    true
                                | Error error, _, _, _ ->
                                    sm.Data.Result <- Error(error.Merge(result2).Merge(result3).Merge(result4))
                                    true
                                | _, Error error, _, _ ->
                                    sm.Data.Result <- Error(error.Merge(result1).Merge(result3).Merge(result4))
                                    true
                                | _, _, Error error, _ ->
                                    sm.Data.Result <- Error(error.Merge(result1).Merge(result2).Merge(result4))
                                    true
                                | _, _, _, Error error ->
                                    sm.Data.Result <- Error(error.Merge(result1).Merge(result2).Merge(result3))
                                    true
                            else
                                sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter4, &sm)
                                false
                        else
                            sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter3, &sm)
                            false

                    else
                        sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter2, &sm)
                        false
                else
                    sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                    false
            else
                EffBuilderBase.ZipDynamic(&sm, task1, task2, task3, task4, f))

    static member inline ZipDynamic<'Env, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'TResult4, 'TResult5, 'Err
        when 'Err: (static member (+): 'Err -> 'Err -> 'Err)>
        (
            sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TOverall, 'Err>>>,
            task1: ValueTask<Result<'TResult1, 'Err>>,
            task2: ValueTask<Result<'TResult2, 'Err>>,
            task3: ValueTask<Result<'TResult3, 'Err>>,
            task4: ValueTask<Result<'TResult4, 'Err>>,
            task5: ValueTask<Result<'TResult5, 'Err>>,
            f
        ) : bool =
        let mutable awaiter1 = task1.GetAwaiter()

        let cont =
            EffectResumptionFunc<_, _, _>(fun sm ->
                let mutable awaiter2 = task2.GetAwaiter()

                let cont =
                    EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->
                        let mutable awaiter3 = task3.GetAwaiter()

                        let cont =
                            EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->
                                let mutable awaiter4 = task4.GetAwaiter()

                                let cont =
                                    EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->
                                        let mutable awaiter5 = task5.GetAwaiter()

                                        let cont =
                                            EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->
                                                let result1 = awaiter1.GetResult()
                                                let result2 = awaiter2.GetResult()
                                                let result3 = awaiter3.GetResult()
                                                let result4 = awaiter4.GetResult()
                                                let result5 = awaiter5.GetResult()

                                                match result1, result2, result3, result4, result5 with
                                                | Ok result, Ok result2, Ok result3, Ok result4, Ok result5 ->
                                                    sm.Data.Result <-
                                                        Ok(f (result, result2, result3, result4, result5))

                                                    true
                                                | Error error, _, _, _, _ ->
                                                    sm.Data.Result <-
                                                        Error(
                                                            error
                                                                .Merge(result2)
                                                                .Merge(result3)
                                                                .Merge(result4)
                                                                .Merge(result5)
                                                        )

                                                    true
                                                | _, Error error, _, _, _ ->
                                                    sm.Data.Result <-
                                                        Error(
                                                            error
                                                                .Merge(result1)
                                                                .Merge(result3)
                                                                .Merge(result4)
                                                                .Merge(result5)
                                                        )

                                                    true
                                                | _, _, Error error, _, _ ->
                                                    sm.Data.Result <-
                                                        Error(
                                                            error
                                                                .Merge(result1)
                                                                .Merge(result2)
                                                                .Merge(result4)
                                                                .Merge(result5)
                                                        )

                                                    true
                                                | _, _, _, Error error, _ ->
                                                    sm.Data.Result <-
                                                        Error(
                                                            error
                                                                .Merge(result1)
                                                                .Merge(result2)
                                                                .Merge(result3)
                                                                .Merge(result5)
                                                        )

                                                    true
                                                | _, _, _, _, Error error ->
                                                    sm.Data.Result <-
                                                        Error(
                                                            error
                                                                .Merge(result1)
                                                                .Merge(result2)
                                                                .Merge(result3)
                                                                .Merge(result4)
                                                        )

                                                    true

                                            )

                                        if awaiter5.IsCompleted then
                                            cont.Invoke(&sm)
                                        else
                                            sm.ResumptionDynamicInfo.ResumptionData <-
                                                (awaiter5 :> ICriticalNotifyCompletion)

                                            sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                                            false

                                    )

                                if awaiter4.IsCompleted then
                                    cont.Invoke(&sm)
                                else
                                    sm.ResumptionDynamicInfo.ResumptionData <-
                                        (awaiter4 :> ICriticalNotifyCompletion)

                                    sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                                    false

                            )

                        if awaiter3.IsCompleted then
                            cont.Invoke(&sm)
                        else
                            sm.ResumptionDynamicInfo.ResumptionData <- (awaiter3 :> ICriticalNotifyCompletion)
                            sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                            false)

                if awaiter2.IsCompleted then
                    cont.Invoke(&sm)
                else
                    sm.ResumptionDynamicInfo.ResumptionData <- (awaiter2 :> ICriticalNotifyCompletion)
                    sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                    false)

        if awaiter1.IsCompleted then
            cont.Invoke(&sm)
        else
            sm.ResumptionDynamicInfo.ResumptionData <- (awaiter1 :> ICriticalNotifyCompletion)
            sm.ResumptionDynamicInfo.ResumptionFunc <- cont

            false


    member inline _.Zip<'Env, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'TResult4, 'TResult5, 'Err
        when 'Err: (static member (+): 'Err -> 'Err -> 'Err)>
        (
            eff1: Effect<'Env, 'TResult1, 'Err>,
            eff2: Effect<'Env, 'TResult2, 'Err>,
            eff3: Effect<'Env, 'TResult3, 'Err>,
            eff4: Effect<'Env, 'TResult4, 'Err>,
            eff5: Effect<'Env, 'TResult5, 'Err>,
            f
        ) : EffectCode<'Env, 'TOverall, 'TOverall, 'Err> =
        EffectCode<'Env, 'TOverall, 'TOverall, 'Err>(fun sm ->
            let task1 = eff1.Run sm.Data.Environment
            let task2 = eff2.Run sm.Data.Environment
            let task3 = eff3.Run sm.Data.Environment
            let task4 = eff4.Run sm.Data.Environment
            let task5 = eff5.Run sm.Data.Environment

            if __useResumableCode then
                let mutable awaiter = task1.GetAwaiter()
                let mutable __stack_fin = true

                if not awaiter.IsCompleted then
                    let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                    __stack_fin <- __stack_yield_fin

                if __stack_fin then
                    let mutable awaiter2 = task2.GetAwaiter()

                    if not awaiter2.IsCompleted then
                        let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                        __stack_fin <- __stack_yield_fin

                    if __stack_fin then
                        let mutable awaiter3 = task3.GetAwaiter()

                        if not awaiter3.IsCompleted then
                            let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                            __stack_fin <- __stack_yield_fin

                        if __stack_fin then
                            let mutable awaiter4 = task4.GetAwaiter()

                            if not awaiter4.IsCompleted then
                                let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                                __stack_fin <- __stack_yield_fin

                            if __stack_fin then
                                let mutable awaiter5 = task5.GetAwaiter()

                                if not awaiter5.IsCompleted then
                                    let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                                    __stack_fin <- __stack_yield_fin

                                if __stack_fin then
                                    let result1 = awaiter.GetResult()
                                    let result2 = awaiter2.GetResult()
                                    let result3 = awaiter3.GetResult()
                                    let result4 = awaiter4.GetResult()
                                    let result5 = awaiter5.GetResult()

                                    match result1, result2, result3, result4, result5 with
                                    | Ok result, Ok result2, Ok result3, Ok result4, Ok result5 ->
                                        sm.Data.Result <- Ok(f (result, result2, result3, result4, result5))
                                        true
                                    | Error error, _, _, _, _ ->
                                        sm.Data.Result <-
                                            Error(error.Merge(result2).Merge(result3).Merge(result4).Merge(result5))

                                        true
                                    | _, Error error, _, _, _ ->
                                        sm.Data.Result <-
                                            Error(error.Merge(result1).Merge(result3).Merge(result4).Merge(result5))

                                        true
                                    | _, _, Error error, _, _ ->
                                        sm.Data.Result <-
                                            Error(error.Merge(result1).Merge(result2).Merge(result4).Merge(result5))

                                        true
                                    | _, _, _, Error error, _ ->
                                        sm.Data.Result <-
                                            Error(error.Merge(result1).Merge(result2).Merge(result3).Merge(result5))

                                        true
                                    | _, _, _, _, Error error ->
                                        sm.Data.Result <-
                                            Error(error.Merge(result1).Merge(result2).Merge(result3).Merge(result4))

                                        true
                                else
                                    sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter5, &sm)
                                    false
                            else
                                sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter4, &sm)
                                false
                        else
                            sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter3, &sm)
                            false

                    else
                        sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter2, &sm)
                        false
                else
                    sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                    false
            else
                EffBuilderBase.ZipDynamic(&sm, task1, task2, task3, task4, task5, f))

    member inline this.Bind2Return(m1, m2, [<InlineIfLambda>] f) = this.Zip(m1, m2, f)

    member inline this.Bind3Return(m1, m2, m3, [<InlineIfLambda>] f) = this.Zip(m1, m2, m3, f)

    member inline this.Bind4Return(m1, m2, m3, m4, [<InlineIfLambda>] f) = this.Zip(m1, m2, m3, m4, f)

    member inline this.Bind5Return(m1, m2, m3, m4, m5, [<InlineIfLambda>] f) = this.Zip(m1, m2, m3, m4, m5, f)

    static member inline Bind2Dynamic<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'Err
        when 'Err: (static member (+): 'Err -> 'Err -> 'Err)>
        (
            sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TOverall, 'Err>>>,
            task1: ValueTask<Result<'TResult1, 'Err>>,
            task2: ValueTask<Result<'TResult2, 'Err>>,
            f: _ -> EffectCode<'Env, 'TOverall, 'T, 'Err>
        ) : bool =
        let mutable awaiter1 = task1.GetAwaiter()
        let mutable awaiter2 = task2.GetAwaiter()

        let cont =
            EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->
                let cont =
                    EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->
                        let result = awaiter1.GetResult()
                        let result2 = awaiter2.GetResult()

                        match result, result2 with
                        | Ok result, Ok result2 -> (f struct (result, result2)).Invoke(&sm)
                        | Error error, Ok _ ->
                            sm.Data.Result <- Error error
                            true
                        | Ok _, Error result ->
                            sm.Data.Result <- Error result
                            true
                        | Error error, Error result ->
                            sm.Data.Result <- Error(error + result)
                            true)

                if awaiter2.IsCompleted then
                    cont.Invoke(&sm)
                else
                    sm.ResumptionDynamicInfo.ResumptionData <- (awaiter2 :> ICriticalNotifyCompletion)
                    sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                    false)

        if awaiter1.IsCompleted then
            cont.Invoke(&sm)
        else
            sm.ResumptionDynamicInfo.ResumptionData <- (awaiter1 :> ICriticalNotifyCompletion)
            sm.ResumptionDynamicInfo.ResumptionFunc <- cont
            false


    member inline _.Bind2<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'Err
        when 'Err: (static member (+): 'Err -> 'Err -> 'Err)>
        (
            eff: Effect<'Env, 'TResult1, 'Err>,
            eff2: Effect<'Env, 'TResult2, 'Err>,
            [<InlineIfLambda>]f: _ -> EffectCode<'Env, 'TOverall, 'T, 'Err>
        ) : EffectCode<'Env, 'TOverall, 'T, 'Err> =
        EffectCode<'Env, 'TOverall, 'T, 'Err>(fun sm ->
            let task = eff.Run sm.Data.Environment
            let task2 = eff2.Run sm.Data.Environment

            if __useResumableCode then
                let mutable awaiter = task.GetAwaiter()

                let mutable __stack_fin = true

                if not awaiter.IsCompleted then
                    let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                    __stack_fin <- __stack_yield_fin

                if __stack_fin then
                    let mutable awaiter2 = task2.GetAwaiter()

                    if not awaiter2.IsCompleted then
                        let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                        __stack_fin <- __stack_yield_fin

                    if __stack_fin then
                        let result = awaiter.GetResult()
                        let result2 = awaiter2.GetResult()

                        match result, result2 with
                        | Ok result, Ok result2 -> (f struct (result, result2)).Invoke(&sm)

                        | Error error, _ ->
                            sm.Data.Result <- Error(error.Merge(result2))
                            true
                        | _, Error error ->
                            sm.Data.Result <- Error(error.Merge(result))
                            true
                    else
                        sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter2, &sm)
                        false
                else
                    sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                    false
            else
                EffBuilderBase.Bind2Dynamic(&sm, task, task2, f))

    static member inline Bind3<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'Err
        when 'Err: (static member (+): 'Err -> 'Err -> 'Err)>
        (
            sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TOverall, 'Err>>>,
            task1: ValueTask<Result<'TResult1, 'Err>>,
            task2: ValueTask<Result<'TResult2, 'Err>>,
            task3: ValueTask<Result<'TResult3, 'Err>>,
            [<InlineIfLambda>]f: _ -> EffectCode<'Env, 'TOverall, 'T, 'Err>
        ) : bool =
        let mutable awaiter1 = task1.GetAwaiter()

        let cont =
            EffectResumptionFunc<_, _, _>(fun sm ->
                let mutable awaiter2 = task2.GetAwaiter()

                let cont =
                    EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->
                        let mutable awaiter3 = task3.GetAwaiter()

                        let cont =
                            EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->
                                let result = awaiter1.GetResult()
                                let result2 = awaiter2.GetResult()
                                let result3 = awaiter3.GetResult()

                                match result, result2, result3 with
                                | Ok result, Ok result2, Ok result3 -> (f (result, result2, result3)).Invoke(&sm)

                                | Error error, _, _ ->
                                    sm.Data.Result <- Error(error.Merge(result2).Merge(result3))
                                    true
                                | _, Error error, _ ->
                                    sm.Data.Result <- Error(error.Merge(result).Merge(result3))
                                    true
                                | _, _, Error error ->
                                    sm.Data.Result <- Error(error.Merge(result).Merge(result2))
                                    true)

                        if awaiter3.IsCompleted then
                            cont.Invoke(&sm)
                        else
                            sm.ResumptionDynamicInfo.ResumptionData <- (awaiter3 :> ICriticalNotifyCompletion)
                            sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                            false)

                if awaiter2.IsCompleted then
                    cont.Invoke(&sm)
                else
                    sm.ResumptionDynamicInfo.ResumptionData <- (awaiter2 :> ICriticalNotifyCompletion)
                    sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                    false)

        if awaiter1.IsCompleted then
            cont.Invoke(&sm)
        else
            sm.ResumptionDynamicInfo.ResumptionData <- (awaiter1 :> ICriticalNotifyCompletion)
            sm.ResumptionDynamicInfo.ResumptionFunc <- cont

            false


    member inline _.Bind3<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'Err
        when 'Err: (static member (+): 'Err -> 'Err -> 'Err)>
        (
            eff: Effect<'Env, 'TResult1, 'Err>,
            eff2: Effect<'Env, 'TResult2, 'Err>,
            eff3: Effect<'Env, 'TResult3, 'Err>,
            [<InlineIfLambda>]f: _ -> EffectCode<'Env, 'TOverall, 'T, 'Err>
        ) : EffectCode<'Env, 'TOverall, 'T, 'Err> =
        EffectCode<'Env, 'TOverall, 'T, 'Err>(fun sm ->
            let task = eff.Run sm.Data.Environment
            let task2 = eff2.Run sm.Data.Environment
            let task3 = eff3.Run sm.Data.Environment

            if __useResumableCode then
                let mutable awaiter = task.GetAwaiter()
                let mutable __stack_fin = true

                if not awaiter.IsCompleted then
                    let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                    __stack_fin <- __stack_yield_fin

                if __stack_fin then
                    let mutable awaiter2 = task2.GetAwaiter()

                    if not awaiter2.IsCompleted then
                        let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                        __stack_fin <- __stack_yield_fin

                    if __stack_fin then
                        let mutable awaiter3 = task3.GetAwaiter()

                        if not awaiter3.IsCompleted then
                            let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                            __stack_fin <- __stack_yield_fin

                        if __stack_fin then

                            let result1 = awaiter.GetResult()
                            let result2 = awaiter2.GetResult()
                            let result3 = awaiter3.GetResult()

                            match result1, result2, result3 with
                            | Ok result, Ok result2, Ok result3 -> (f (result, result2, result3)).Invoke(&sm)

                            | Error error, _, _ ->
                                sm.Data.Result <- Error(error.Merge(result2).Merge(result3))
                                true
                            | _, Error error, _ ->
                                sm.Data.Result <- Error(error.Merge(result1).Merge(result3))
                                true
                            | _, _, Error error ->
                                sm.Data.Result <- Error(error.Merge(result1).Merge(result2))
                                true
                        else
                            sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter3, &sm)
                            false

                    else
                        sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter2, &sm)
                        false
                else
                    sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                    false
            else
                EffBuilderBase.Bind3(&sm, task, task2, task3, f))

    static member inline Bind4<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'TResult4, 'Err
        when 'Err: (static member (+): 'Err -> 'Err -> 'Err)>
        (
            sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TOverall, 'Err>>>,
            task1: ValueTask<Result<'TResult1, 'Err>>,
            task2: ValueTask<Result<'TResult2, 'Err>>,
            task3: ValueTask<Result<'TResult3, 'Err>>,
            task4: ValueTask<Result<'TResult4, 'Err>>,
            [<InlineIfLambda>]f: _ -> EffectCode<'Env, 'TOverall, 'T, 'Err>
        ) : bool =
        let mutable awaiter1 = task1.GetAwaiter()

        let cont =
            EffectResumptionFunc<_, _, _>(fun sm ->
                let mutable awaiter2 = task2.GetAwaiter()

                let cont =
                    EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->
                        let mutable awaiter3 = task3.GetAwaiter()

                        let cont =
                            EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->

                                let mutable awaiter4 = task4.GetAwaiter()

                                let cont =
                                    EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->
                                        let result1 = awaiter1.GetResult()
                                        let result2 = awaiter2.GetResult()
                                        let result3 = awaiter3.GetResult()
                                        let result4 = awaiter4.GetResult()

                                        match result1, result2, result3, result4 with
                                        | Ok result, Ok result2, Ok result3, Ok result4 ->
                                            (f (result, result2, result3, result4)).Invoke(&sm)

                                        | Error error, _, _, _ ->
                                            sm.Data.Result <-
                                                Error(error.Merge(result2).Merge(result3).Merge(result4))

                                            true
                                        | _, Error error, _, _ ->
                                            sm.Data.Result <-
                                                Error(error.Merge(result1).Merge(result3).Merge(result4))

                                            true
                                        | _, _, Error error, _ ->
                                            sm.Data.Result <-
                                                Error(error.Merge(result1).Merge(result2).Merge(result4))

                                            true
                                        | _, _, _, Error error ->
                                            sm.Data.Result <-
                                                Error(error.Merge(result1).Merge(result2).Merge(result3))

                                            true)

                                if awaiter4.IsCompleted then
                                    cont.Invoke(&sm)
                                else
                                    sm.ResumptionDynamicInfo.ResumptionData <-
                                        (awaiter4 :> ICriticalNotifyCompletion)

                                    sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                                    false)

                        if awaiter3.IsCompleted then
                            cont.Invoke(&sm)
                        else
                            sm.ResumptionDynamicInfo.ResumptionData <- (awaiter3 :> ICriticalNotifyCompletion)
                            sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                            false)

                if awaiter2.IsCompleted then
                    cont.Invoke(&sm)
                else
                    sm.ResumptionDynamicInfo.ResumptionData <- (awaiter2 :> ICriticalNotifyCompletion)
                    sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                    false)

        if awaiter1.IsCompleted then
            cont.Invoke(&sm)
        else
            sm.ResumptionDynamicInfo.ResumptionData <- (awaiter1 :> ICriticalNotifyCompletion)
            sm.ResumptionDynamicInfo.ResumptionFunc <- cont

            false

    member inline _.Bind4<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'TResult4, 'Err
        when 'Err: (static member (+): 'Err -> 'Err -> 'Err)>
        (
            eff1: Effect<'Env, 'TResult1, 'Err>,
            eff2: Effect<'Env, 'TResult2, 'Err>,
            eff3: Effect<'Env, 'TResult3, 'Err>,
            eff4: Effect<'Env, 'TResult4, 'Err>,
            [<InlineIfLambda>]f: _ -> EffectCode<'Env, 'TOverall, 'T, 'Err>
        ) : EffectCode<'Env, 'TOverall, 'T, 'Err> =
        EffectCode<'Env, 'TOverall, 'T, 'Err>(fun sm ->
            let task1 = eff1.Run sm.Data.Environment
            let task2 = eff2.Run sm.Data.Environment
            let task3 = eff3.Run sm.Data.Environment
            let task4 = eff4.Run sm.Data.Environment

            if __useResumableCode then
                let mutable awaiter = task1.GetAwaiter()
                let mutable __stack_fin = true

                if not awaiter.IsCompleted then
                    let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                    __stack_fin <- __stack_yield_fin

                if __stack_fin then
                    let mutable awaiter2 = task2.GetAwaiter()

                    if not awaiter2.IsCompleted then
                        let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                        __stack_fin <- __stack_yield_fin

                    if __stack_fin then
                        let mutable awaiter3 = task3.GetAwaiter()

                        if not awaiter3.IsCompleted then
                            let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                            __stack_fin <- __stack_yield_fin

                        if __stack_fin then
                            let mutable awaiter4 = task4.GetAwaiter()

                            if not awaiter4.IsCompleted then
                                let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                                __stack_fin <- __stack_yield_fin

                            if __stack_fin then
                                let result1 = awaiter.GetResult()
                                let result2 = awaiter2.GetResult()
                                let result3 = awaiter3.GetResult()
                                let result4 = awaiter4.GetResult()

                                match result1, result2, result3, result4 with
                                | Ok result, Ok result2, Ok result3, Ok result4 ->
                                    (f (result, result2, result3, result4)).Invoke(&sm)

                                | Error error, _, _, _ ->
                                    sm.Data.Result <- Error(error.Merge(result2).Merge(result3).Merge(result4))
                                    true
                                | _, Error error, _, _ ->
                                    sm.Data.Result <- Error(error.Merge(result1).Merge(result3).Merge(result4))
                                    true
                                | _, _, Error error, _ ->
                                    sm.Data.Result <- Error(error.Merge(result1).Merge(result2).Merge(result4))
                                    true
                                | _, _, _, Error error ->
                                    sm.Data.Result <- Error(error.Merge(result1).Merge(result2).Merge(result3))
                                    true
                            else
                                sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter4, &sm)
                                false
                        else
                            sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter3, &sm)
                            false

                    else
                        sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter2, &sm)
                        false
                else
                    sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                    false
            else
                EffBuilderBase.Bind4(&sm, task1, task2, task3, task4, f))

    static member inline Bind5<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'TResult4, 'TResult5, 'Err
        when 'Err: (static member (+): 'Err -> 'Err -> 'Err)>
        (
            sm: byref<ResumableStateMachine<EffectStateMachineData<'Env, 'TOverall, 'Err>>>,
            task1: ValueTask<Result<'TResult1, 'Err>>,
            task2: ValueTask<Result<'TResult2, 'Err>>,
            task3: ValueTask<Result<'TResult3, 'Err>>,
            task4: ValueTask<Result<'TResult4, 'Err>>,
            task5: ValueTask<Result<'TResult5, 'Err>>,
            [<InlineIfLambda>]f: _ -> EffectCode<'Env, 'TOverall, 'T, 'Err>
        ) : bool =
        let mutable awaiter1 = task1.GetAwaiter()

        let cont =
            EffectResumptionFunc<_, _, _>(fun sm ->
                let mutable awaiter2 = task2.GetAwaiter()

                let cont =
                    EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->
                        let mutable awaiter3 = task3.GetAwaiter()

                        let cont =
                            EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->
                                let mutable awaiter4 = task4.GetAwaiter()

                                let cont =
                                    EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->
                                        let mutable awaiter5 = task5.GetAwaiter()

                                        let cont =
                                            EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->
                                                let result1 = awaiter1.GetResult()
                                                let result2 = awaiter2.GetResult()
                                                let result3 = awaiter3.GetResult()
                                                let result4 = awaiter4.GetResult()
                                                let result5 = awaiter5.GetResult()

                                                match result1, result2, result3, result4, result5 with
                                                | Ok result, Ok result2, Ok result3, Ok result4, Ok result5 ->
                                                    (f (result, result2, result3, result4, result5)).Invoke(&sm)

                                                | Error error, _, _, _, _ ->
                                                    sm.Data.Result <-
                                                        Error(
                                                            error
                                                                .Merge(result2)
                                                                .Merge(result3)
                                                                .Merge(result4)
                                                                .Merge(result5)
                                                        )

                                                    true
                                                | _, Error error, _, _, _ ->
                                                    sm.Data.Result <-
                                                        Error(
                                                            error
                                                                .Merge(result1)
                                                                .Merge(result3)
                                                                .Merge(result4)
                                                                .Merge(result5)
                                                        )

                                                    true
                                                | _, _, Error error, _, _ ->
                                                    sm.Data.Result <-
                                                        Error(
                                                            error
                                                                .Merge(result1)
                                                                .Merge(result2)
                                                                .Merge(result4)
                                                                .Merge(result5)
                                                        )

                                                    true
                                                | _, _, _, Error error, _ ->
                                                    sm.Data.Result <-
                                                        Error(
                                                            error
                                                                .Merge(result1)
                                                                .Merge(result2)
                                                                .Merge(result3)
                                                                .Merge(result5)
                                                        )

                                                    true
                                                | _, _, _, _, Error error ->
                                                    sm.Data.Result <-
                                                        Error(
                                                            error
                                                                .Merge(result1)
                                                                .Merge(result2)
                                                                .Merge(result3)
                                                                .Merge(result4)
                                                        )

                                                    true

                                            )

                                        if awaiter5.IsCompleted then
                                            cont.Invoke(&sm)
                                        else
                                            sm.ResumptionDynamicInfo.ResumptionData <-
                                                (awaiter5 :> ICriticalNotifyCompletion)

                                            sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                                            false

                                    )

                                if awaiter4.IsCompleted then
                                    cont.Invoke(&sm)
                                else
                                    sm.ResumptionDynamicInfo.ResumptionData <-
                                        (awaiter4 :> ICriticalNotifyCompletion)

                                    sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                                    false

                            )

                        if awaiter3.IsCompleted then
                            cont.Invoke(&sm)
                        else
                            sm.ResumptionDynamicInfo.ResumptionData <- (awaiter3 :> ICriticalNotifyCompletion)
                            sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                            false)

                if awaiter2.IsCompleted then
                    cont.Invoke(&sm)
                else
                    sm.ResumptionDynamicInfo.ResumptionData <- (awaiter2 :> ICriticalNotifyCompletion)
                    sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                    false)

        if awaiter1.IsCompleted then
            cont.Invoke(&sm)
        else
            sm.ResumptionDynamicInfo.ResumptionData <- (awaiter1 :> ICriticalNotifyCompletion)
            sm.ResumptionDynamicInfo.ResumptionFunc <- cont

            false


    member inline _.Bind5<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'TResult3, 'TResult4, 'TResult5, 'Err
        when 'Err: (static member (+): 'Err -> 'Err -> 'Err)>
        (
            eff1: Effect<'Env, 'TResult1, 'Err>,
            eff2: Effect<'Env, 'TResult2, 'Err>,
            eff3: Effect<'Env, 'TResult3, 'Err>,
            eff4: Effect<'Env, 'TResult4, 'Err>,
            eff5: Effect<'Env, 'TResult5, 'Err>,
            [<InlineIfLambda>]f: _ -> EffectCode<'Env, 'TOverall, 'T, 'Err>
        ) : EffectCode<'Env, 'TOverall, 'T, 'Err> =
        EffectCode<'Env, 'TOverall, 'T, 'Err>(fun sm ->
            let task1 = eff1.Run sm.Data.Environment
            let task2 = eff2.Run sm.Data.Environment
            let task3 = eff3.Run sm.Data.Environment
            let task4 = eff4.Run sm.Data.Environment
            let task5 = eff5.Run sm.Data.Environment

            if __useResumableCode then
                let mutable awaiter = task1.GetAwaiter()
                let mutable __stack_fin = true

                if not awaiter.IsCompleted then
                    let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                    __stack_fin <- __stack_yield_fin

                if __stack_fin then
                    let mutable awaiter2 = task2.GetAwaiter()

                    if not awaiter2.IsCompleted then
                        let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                        __stack_fin <- __stack_yield_fin

                    if __stack_fin then
                        let mutable awaiter3 = task3.GetAwaiter()

                        if not awaiter3.IsCompleted then
                            let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                            __stack_fin <- __stack_yield_fin

                        if __stack_fin then
                            let mutable awaiter4 = task4.GetAwaiter()

                            if not awaiter4.IsCompleted then
                                let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                                __stack_fin <- __stack_yield_fin

                            if __stack_fin then
                                let mutable awaiter5 = task5.GetAwaiter()

                                if not awaiter5.IsCompleted then
                                    let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                                    __stack_fin <- __stack_yield_fin

                                if __stack_fin then
                                    let result1 = awaiter.GetResult()
                                    let result2 = awaiter2.GetResult()
                                    let result3 = awaiter3.GetResult()
                                    let result4 = awaiter4.GetResult()
                                    let result5 = awaiter5.GetResult()

                                    match result1, result2, result3, result4, result5 with
                                    | Ok result, Ok result2, Ok result3, Ok result4, Ok result5 ->
                                        (f (result, result2, result3, result4, result5)).Invoke(&sm)

                                    | Error error, _, _, _, _ ->
                                        sm.Data.Result <-
                                            Error(error.Merge(result2).Merge(result3).Merge(result4).Merge(result5))

                                        true
                                    | _, Error error, _, _, _ ->
                                        sm.Data.Result <-
                                            Error(error.Merge(result1).Merge(result3).Merge(result4).Merge(result5))

                                        true
                                    | _, _, Error error, _, _ ->
                                        sm.Data.Result <-
                                            Error(error.Merge(result1).Merge(result2).Merge(result4).Merge(result5))

                                        true
                                    | _, _, _, Error error, _ ->
                                        sm.Data.Result <-
                                            Error(error.Merge(result1).Merge(result2).Merge(result3).Merge(result5))

                                        true
                                    | _, _, _, _, Error error ->
                                        sm.Data.Result <-
                                            Error(error.Merge(result1).Merge(result2).Merge(result3).Merge(result4))

                                        true
                                else
                                    sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter5, &sm)
                                    false
                            else
                                sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter4, &sm)
                                false
                        else
                            sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter3, &sm)
                            false

                    else
                        sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter2, &sm)
                        false
                else
                    sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                    false
            else
                EffBuilderBase.Bind5(&sm, task1, task2, task3, task4, task5, f))

    static member WhenAllDynamic
        (
            sm: byref<EffectStateMachine<_, _, _>>,
            waits: ValueTaskAwaiter<Result<_, _>> Memory,
            results: ResizeArray<_>
        ) : bool =
        if waits.Length = 0 then
            let final = Array.zeroCreate results.Count
            let mutable value = Ok final

            for i = 0 to results.Count - 1 do
                match results[i] with
                | Ok ok -> final[i] <- ok
                | Error err -> value <- Error err

            sm.Data.Result <- value
            true
        else
            let awaiter = waits.Span[0]
            let rest = waits.Slice(1)

            if awaiter.IsCompleted then
                results.Add(awaiter.GetResult())
                EffBuilderBase.WhenAllDynamic(&sm, rest, results)
            else
                let rf = EffBuilderBase.GetResumptionFunc &sm

                sm.ResumptionDynamicInfo.ResumptionFunc <-
                    ResumptionFunc<_>(fun sm -> EffBuilderBase.WhenAllDynamicAux(&sm, waits, results, rf))

                false

    static member WhenAllDynamicAux
        (
            sm: byref<ResumableStateMachine<_>>,
            waits: ValueTaskAwaiter<_> Memory,
            results: ResizeArray<_>,
            rf: ResumptionFunc<_>
        ) : bool =

        if rf.Invoke(&sm) then
            EffBuilderBase.WhenAllDynamic(&sm, waits, results)
        else
            let rf = EffBuilderBase.GetResumptionFunc &sm

            sm.ResumptionDynamicInfo.ResumptionFunc <-
                ResumptionFunc<_>(fun sm -> EffBuilderBase.WhenAllDynamicAux(&sm, waits, results, rf))

            false

    static member inline AwaiterNotComplete(v: ValueTaskAwaiter<_> inref) = not v.IsCompleted

    static member inline CreateWaiters (effects: Effect<'Env, 'T, 'Err> seq) env : ValueTuple<_, _> =
        match effects with
        | :? (Effect<'Env, 'T, 'Err> array) as effects ->
            let waits = ArrayPool<_>.Shared.Rent effects.Length

            for i = 0 to effects.Length - 1 do
                let e = &effects[i]
                waits[i] <- e.Run(env).GetAwaiter()

            waits, true
        | :? IReadOnlyList<Effect<'Env, 'T, 'Err>> as effects ->
            let waits = ArrayPool<_>.Shared.Rent effects.Count

            for i = 0 to effects.Count - 1 do
                waits[i] <- effects[i].Run(env).GetAwaiter()

            waits, true
        | :? IReadOnlyCollection<Effect<'Env, 'T, 'Err>> as effects ->
            let waits = ArrayPool<_>.Shared.Rent effects.Count
            effects |> Seq.iteri (fun i e -> waits[i] <- e.Run(env).GetAwaiter())
            waits, true
        | _ -> [| for e in effects -> e.Run(env).GetAwaiter() |], false

    (* Extra*)
    member inline _.WhenAll(effects: Effect<'Env, 'T, 'Err> seq) : EffectCode<'Env, 'T array, 'T array, 'Err> =
        EffectCode<'Env, 'T array, 'T array, 'Err>(fun sm ->
            if __useResumableCode then
                let env = sm.Data.Environment
                let struct (waits, rented) = EffBuilderBase.CreateWaiters effects env
                let mutable i = 0
                let mutable __stack_fin = true

                while (i < waits.Length) && __stack_fin do
                    if EffBuilderBase.AwaiterNotComplete(&waits[i]) then
                        let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                        __stack_fin <- __stack_yield_fin

                    if __stack_fin then
                        i <- i + 1
                    else
                        sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&waits[i], &sm)

                if __stack_fin then
                    let result = Array.zeroCreate waits.Length
                    let mutable final = Ok result

                    try
                        let mutable cont = true
                        let mutable j = 0

                        while (j < waits.Length) && cont do
                            let awaiter = &waits[j]

                            match awaiter.GetResult() with
                            | Ok a -> result[j] <- a
                            | Error err ->
                                final <- Error err
                                cont <- false

                            j <- j + 1
                    finally
                        if rented then
                            ArrayPool<_>.Shared.Return waits

                    sm.Data.Result <- final
                    true
                else
                    false

            else
                let env = sm.Data.Environment
                let waits = [| for e in effects -> e.Run(env).GetAwaiter() |]
                EffBuilderBase.WhenAllDynamic(&sm, Memory waits, ResizeArray()))



/// <exclude/>
type EffBuilder() =
    inherit EffBuilderBase()

    static member inline RunDynamic(code: EffectCode<'Env, 'T, 'T, 'Err>) : Effect<'Env, 'T, 'Err> =

        let mutable sm = EffectStateMachine<'Env, 'T, 'Err>()

        let initialResumptionFunc =
            EffectResumptionFunc<'Env, 'T, 'Err>(fun sm -> code.Invoke(&sm))

        let resumptionInfo =
            { new EffectResumptionDynamicInfo<'Env, 'T, 'Err>(initialResumptionFunc) with
                member info.MoveNext(sm) =
                    let mutable savedExn = null

                    try
                        sm.ResumptionDynamicInfo.ResumptionData <- null
                        let step = info.ResumptionFunc.Invoke(&sm)

                        if step then
                            sm.Data.MethodBuilder.SetResult(sm.Data.Result)
                        else
                            let mutable awaiter =
                                sm.ResumptionDynamicInfo.ResumptionData :?> ICriticalNotifyCompletion

                            assert not (isNull awaiter)
                            sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)

                    with exn ->
                        savedExn <- exn
                    // Run SetException outside the stack unwind, see https://github.com/dotnet/roslyn/issues/26567
                    match savedExn with
                    | null -> ()
                    | exn -> sm.Data.MethodBuilder.SetException exn

                member _.SetStateMachine(sm, state) =
                    sm.Data.MethodBuilder.SetStateMachine(state)
            }

        Effect(
            EffectDelegate(fun env ->
                sm.ResumptionPoint <- -1
                sm.Data.Environment <- env
                sm.ResumptionDynamicInfo <- resumptionInfo
                sm.Data.MethodBuilder <- AsyncValueTaskMethodBuilder<Result<'T, 'Err>>.Create()
                sm.Data.MethodBuilder.Start(&sm)
                sm.Data.MethodBuilder.Task)
        )

    [<NoEagerConstraintApplication>]
    member inline _.Run<'Env, 'T, 'Err>(code: EffectCode<'Env, 'T, 'T, 'Err>) : Effect<'Env, 'T, 'Err> =
        if __useResumableCode then
            __stateMachine<EffectStateMachineData<'Env, 'T, 'Err>, Effect<'Env, 'T, 'Err>>
                (MoveNextMethodImpl<_>(fun sm ->
                    __resumeAt sm.ResumptionPoint
                    let mutable __stack_exn: Exception = null

                    try
                        let __stack_code_fin = code.Invoke(&sm)

                        if __stack_code_fin then
                            sm.Data.MethodBuilder.SetResult(sm.Data.Result)
                    with exn ->
                        __stack_exn <- exn
                    // Run SetException outside the stack unwind, see https://github.com/dotnet/roslyn/issues/26567
                    match __stack_exn with
                    | null -> ()
                    | exn -> sm.Data.MethodBuilder.SetException exn
                //-- RESUMABLE CODE END
                ))
                (SetStateMachineMethodImpl<_>(fun sm state -> sm.Data.MethodBuilder.SetStateMachine(state)))
                (AfterCode<_, _>(fun sm ->
                    let mutable sm = sm

                    Effect(
                        EffectDelegate(fun env ->
                            sm.ResumptionPoint <- -1
                            sm.Data.Environment <- env
                            sm.Data.MethodBuilder <- AsyncValueTaskMethodBuilder<Result<'T, 'Err>>.Create()
                            sm.Data.MethodBuilder.Start(&sm)
                            sm.Data.MethodBuilder.Task)
                    )))
        else
            EffBuilder.RunDynamic(code)

/// <exclude/>
[<AutoOpen>]
module Builder =
    let inline mkEffect d = Effect(EffectDelegate d)
    let eff = EffBuilder()

/// <exclude/>
[<AutoOpen>]
module LowPriority =
    type EffBuilderBase with

        [<NoEagerConstraintApplication>]
        static member inline BindDynamic< ^TaskLike, 'Env, 'TResult1, 'TResult2, ^Awaiter, 'TOverall, 'Err
            when ^TaskLike: (member GetAwaiter: unit -> ^Awaiter)
            and ^Awaiter :> ICriticalNotifyCompletion
            and ^Awaiter: (member get_IsCompleted: unit -> bool)
            and ^Awaiter: (member GetResult: unit -> 'TResult1)>
            (
                sm: byref<_>,
                task: ^TaskLike,
                continuation: 'TResult1 -> EffectCode<'Env, 'TOverall, 'TResult2, 'Err>
            ) : bool =

            let mutable awaiter = (^TaskLike: (member GetAwaiter: unit -> ^Awaiter) task)

            let cont =
                EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->
                    let result = (^Awaiter: (member GetResult: unit -> 'TResult1) awaiter)
                    (continuation result).Invoke(&sm))

            // shortcut to continue immediately
            if (^Awaiter: (member get_IsCompleted: unit -> bool) awaiter) then
                cont.Invoke(&sm)
            else
                sm.ResumptionDynamicInfo.ResumptionData <- (awaiter :> ICriticalNotifyCompletion)
                sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                false

        [<NoEagerConstraintApplication>]
        member inline _.Bind< ^TaskLike, 'Env, 'TResult1, 'TResult2, ^Awaiter, 'TOverall, 'Err
            when ^TaskLike: (member GetAwaiter: unit -> ^Awaiter)
            and ^Awaiter :> ICriticalNotifyCompletion
            and ^Awaiter: (member get_IsCompleted: unit -> bool)
            and ^Awaiter: (member GetResult: unit -> 'TResult1)>
            (
                task: ^TaskLike,
                continuation: 'TResult1 -> EffectCode<'Env, 'TOverall, 'TResult2, 'Err>
            ) : EffectCode<'Env, 'TOverall, 'TResult2, 'Err> =

            EffectCode<'Env, 'TOverall, 'TResult2, 'Err>(fun sm ->
                if __useResumableCode then
                    let mutable awaiter = (^TaskLike: (member GetAwaiter: unit -> ^Awaiter) task)

                    let mutable __stack_fin = true

                    if not (^Awaiter: (member get_IsCompleted: unit -> bool) awaiter) then
                        let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                        __stack_fin <- __stack_yield_fin

                    if __stack_fin then
                        let result = (^Awaiter: (member GetResult: unit -> 'TResult1) awaiter)
                        (continuation result).Invoke(&sm)
                    else
                        sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                        false
                else
                    EffBuilder.BindDynamic(&sm, task, continuation))

/// <exclude/>
[<AutoOpen>]
module Medium =
    open FSharp.Control

    type EffBuilderBase with
        (* DO WANT *)
        static member inline BindDynamic
            (
                sm: byref<_>,
                task: ValueTask<'TResult1>,
                continuation: 'TResult1 -> EffectCode<'Env, 'TOverall, 'TResult2, 'Err>
            ) : bool =
            let mutable awaiter = task.GetAwaiter()

            let cont =
                EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->
                    let result = awaiter.GetResult()
                    (continuation result).Invoke(&sm))

            // shortcut to continue immediately
            if awaiter.IsCompleted then
                cont.Invoke(&sm)
            else
                sm.ResumptionDynamicInfo.ResumptionData <- (awaiter :> ICriticalNotifyCompletion)
                sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                false

        static member inline BindDynamic
            (
                sm: byref<_>,
                task: ValueTask<Result<'TResult1, 'Err>>,
                continuation: 'TResult1 -> EffectCode<'Env, 'TOverall, 'TResult2, 'Err>
            ) : bool =
            let mutable awaiter = task.GetAwaiter()

            let cont =
                EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->
                    let result = awaiter.GetResult()

                    match result with
                    | Ok result -> (continuation result).Invoke(&sm)
                    | Error error ->
                        sm.Data.Result <- Error error
                        true

                )

            // shortcut to continue immediately
            if awaiter.IsCompleted then
                cont.Invoke(&sm)
            else
                sm.ResumptionDynamicInfo.ResumptionData <- (awaiter :> ICriticalNotifyCompletion)
                sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                false

        [<NoEagerConstraintApplication>]
        member inline _.Bind<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'Err>
            (
                eff: Effect<'Env, 'TResult1, 'Err>,
                continuation: 'TResult1 -> EffectCode<'Env, 'TOverall, 'TResult2, 'Err>

            ) : EffectCode<'Env, 'TOverall, 'TResult2, 'Err> =

            EffectCode<'Env, 'TOverall, _, 'Err>(fun sm ->
                let task = eff.Run sm.Data.Environment

                if __useResumableCode then
                    let mutable awaiter = task.GetAwaiter()

                    let mutable __stack_fin = true

                    if not awaiter.IsCompleted then
                        let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                        __stack_fin <- __stack_yield_fin

                    if __stack_fin then
                        let result = awaiter.GetResult()

                        match result with
                        | Ok result -> (continuation result).Invoke(&sm)
                        | Error error ->
                            sm.Data.Result <- Error error
                            true
                    else
                        sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                        false
                else
                    EffBuilder.BindDynamic(&sm, task, continuation))

        member inline this.Bind<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'Err>
            (
                task: ValueTask<'TResult1>,
                continuation: 'TResult1 -> EffectCode<'Env, 'TOverall, 'TResult2, 'Err>
            ) : EffectCode<'Env, 'TOverall, 'TResult2, 'Err> =
            EffectCode<'Env, 'TOverall, _, 'Err>(fun sm ->
                if __useResumableCode then
                    let mutable awaiter = task.GetAwaiter()

                    let mutable __stack_fin = true

                    if not awaiter.IsCompleted then
                        let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                        __stack_fin <- __stack_yield_fin

                    if __stack_fin then
                        let result = awaiter.GetResult()
                        (continuation result).Invoke(&sm)

                    else
                        sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                        false
                else
                    EffBuilder.BindDynamic(&sm, task, continuation))

        member inline this.Bind<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'Err>
            (
                t: Task<'TResult1>,
                continuation: 'TResult1 -> EffectCode<'Env, 'TOverall, 'TResult2, 'Err>
            ) : EffectCode<'Env, 'TOverall, 'TResult2, 'Err> =
            this.Bind(ValueTask<_>(task = t), continuation)

        member inline _.Bind(result: Result<'T, 'Err>, [<InlineIfLambda>] f: 'T -> EffectCode<_,'TOverall,'T2,_>) =            
            EffectCode<'Env, 'TOverall, 'T2, 'Err>(fun sm ->
                match result with
                | Ok ok -> (f ok).Invoke(&sm)
                | Error e ->
                    sm.Data.Result <- Error e
                    true
                )

        member inline this.ReturnFrom(task: Effect<'Env, 'T, 'Err>) : EffectCode<'Env, 'T, 'T, 'Err> =
            this.Bind(task, (fun (t: 'T) -> this.Return t))

        member inline this.ReturnFrom(result: Result<'T, 'Err>) : EffectCode<'Env, 'T, 'T, 'Err> =
            EffectCode<'Env, 'T, _, 'Err>(fun sm ->
                sm.Data.Result <- result
                true)

        member inline _.Using<'Resource, 'TOverall, 'T, 'Env, 'Err when 'Resource :> IDisposable>
            (
                resource: 'Resource,
                [<InlineIfLambda>]body: 'Resource -> EffectCode<'Env, 'TOverall, 'T, 'Err>
            ) =
            ResumableCode.Using(resource, body)

        //-------------------------------------------------------------------------------
        // Extra for and while
        //-------------------------------------------------------------------------------
        member inline this.For(s: IAsyncEnumerable<'a>, [<InlineIfLambda>] f1) =
            this.Delay(fun () ->
                this.Using(
                    s.GetAsyncEnumerator(),
                    fun enumerator ->
                        let mutable continue' = false

                        this.Bind(
                            enumerator.MoveNextAsync(),
                            fun b ->
                                continue' <- b

                                this.While(
                                    (fun () -> continue'),
                                    let current = enumerator.Current

                                    this.Combine(
                                        (f1 current),
                                        this.Bind(
                                            enumerator.MoveNextAsync(),
                                            fun b ->
                                                continue' <- b
                                                this.Zero()
                                        )
                                    )
                                )
                        )
                ))

        member inline this.While
            (
                [<InlineIfLambda>] condition: unit -> Effect<'Env, bool, 'Err>,
                body: EffectCode<'Env, 'TOverall, unit, 'Err>
            ) : EffectCode<'Env, 'TOverall, unit, 'Err> =
            this.Delay(fun () ->
                let mutable continue' = false

                this.Bind(
                    condition (),
                    fun b ->
                        let y = b
                        continue' <- y

                        this.While(
                            (fun () -> continue'),
                            this.Delay(fun () ->
                                this.Combine(
                                    body,
                                    this.Bind(
                                        condition (),
                                        fun b ->
                                            let y = b
                                            continue' <- y
                                            this.Zero()
                                    )
                                ))
                        )
                ))

        member inline this.While
            (
                [<InlineIfLambda>] condition: unit -> ValueTask<bool>,
                body: EffectCode<'Env, 'TOverall, unit, 'Err>
            ) : EffectCode<'Env, 'TOverall, unit, 'Err> =
            this.While(
                (fun () ->
                    Effect(
                        EffectDelegate(fun _ -> vtask {
                            let! b = condition ()
                            return Ok b
                        })
                    )),
                body
            )

        member inline this.For(s: IEnumerable<'a>, [<InlineIfLambda>]f1: 'a -> EffectCode<'Env, unit, unit, 'Err>) =
            this.Delay(fun () ->
                this.Using(
                    s.GetEnumerator(),
                    fun enumerator -> this.While((fun () -> enumerator.MoveNext()), (f1 enumerator.Current))
                ))

        member inline this.For
            (
                (a, b): struct ('a * 'a),
                [<InlineIfLambda>] f: 'a -> EffectCode<'Env, unit, unit, 'Err>
            ) =
            this.Delay(fun () -> this.Combine(f a, f b))

        member inline this.For
            (
                (a, b, c): struct ('a * 'a * 'a),
                [<InlineIfLambda>] f: 'a -> EffectCode<'Env, unit, unit, 'Err>
            ) =
            this.Delay(fun () -> this.Combine(this.Combine(f a, f b), f c))

        member inline this.For
            (
                (a, b, c, d): struct ('a * 'a * 'a * 'a),
                [<InlineIfLambda>] f: 'a -> EffectCode<'Env, unit, unit, 'Err>
            ) =
            this.Delay(fun () -> this.Combine(this.Combine(this.Combine(f a, f b), f c), f d))

        member inline this.For
            (
                (a, b, c, d, e): struct ('a * 'a * 'a * 'a * 'a),
                [<InlineIfLambda>] f: 'a -> EffectCode<'Env, unit, unit, 'Err>
            ) =
            this.Delay(fun () -> this.Combine(this.Combine(this.Combine(this.Combine(f a, f b), f c), f d), f e))

        member inline this.For
            (
                (a, b, c, d, e, f): struct ('a * 'a * 'a * 'a * 'a * 'a),
                [<InlineIfLambda>] fn: 'a -> EffectCode<'Env, unit, unit, 'Err>
            ) =
            this.Delay(fun () ->
                this.Combine(
                    this.Combine(this.Combine(this.Combine(this.Combine(fn a, fn b), fn c), fn d), fn e),
                    fn f
                ))

        member inline this.For
            (
                (a, b, c, d, e, f, g): struct ('a * 'a * 'a * 'a * 'a * 'a * 'a),
                [<InlineIfLambda>] fn: 'a -> EffectCode<'Env, unit, unit, 'Err>
            ) =
            this.Delay(fun () ->
                this.Combine(
                    this.Combine(
                        this.Combine(this.Combine(this.Combine(this.Combine(fn a, fn b), fn c), fn d), fn e),
                        fn f
                    ),
                    fn g
                ))

//Fsharp plus
type Effect<'R, 'T, 'E> with
    (* Functor *)
    ///Implements Map on the effect, making it a Functor
    static member inline Map(h: Effect<'r, 'a, 'e>, f: 'a -> 'b) = eff {
        let! e = h
        return f e
    }

    ///Implements Return on the effect
    static member inline Return(a) = eff { return a }

    (* Monad *)
    ///Implements Bind on the effect, which together with the functor Return makes it a Monad
    static member inline (>>=)(h: Effect<'r, 'a, 'e>, f: 'a -> Effect<'r, 'b, 'e>) = eff {
        let! e = h
        return! f e
    }

    //helper, seem type inference get wonky with operator
    /// <exclude/>
    static member inline ap<'r, 'a, 'b, 'e>
        (applicative: Effect<'r, 'b -> 'a, 'e>)
        (e: Effect<'r, 'b, 'e>)
        : Effect<'r, 'a, 'e> =
        eff {
            let! fn = applicative
            let! a = e
            return fn a
        }

    (* applicative *)
    ///Implements Apply on the effect, making it an applicative
    static member inline (<*>)(f, e) = Effect.ap<_,_,_,_> f e
