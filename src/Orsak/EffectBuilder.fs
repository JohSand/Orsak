namespace Orsak

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

    member inline _.Delay
        (generator: unit -> EffectCode<'Env, 'TOverall, 'T, 'Err>)
        : EffectCode<'Env, 'TOverall, 'T, 'Err> =
        EffectCode<'Env, 'TOverall, 'T, 'Err>(fun sm -> (generator ()).Invoke(&sm))


    [<DefaultValue>]
    member inline _.Zero() : EffectCode<'Env, 'TOverall, unit, 'Err> = ResumableCode.Zero()

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
                //todo stock CombineDynamic does not propagate error on first task1
                ResumableCode.CombineDynamic(&sm, task1, task2))

    member inline _.While
        (
            [<InlineIfLambda>] condition: unit -> bool,
            body: EffectCode<'Env, 'TOverall, unit, 'Err>
        ) : EffectCode<'Env, 'TOverall, unit, 'Err> =
        EffectCode<'Env, 'TOverall, unit, 'Err>(fun sm ->
            if __useResumableCode then
                //-- RESUMABLE CODE START
                let mutable __stack_go = true
                let mutable cc = true

                while __stack_go && condition () && cc do
                    // NOTE: The body of the state machine code for 'while' may contain await points, so resuming
                    // the code will branch directly into the expanded 'body', branching directly into the while loop
                    let __stack_body_fin = body.Invoke(&sm)

                    if __stack_body_fin then
                        // If the body completed, we go back around the loop (__stack_go = true)
                        // If the body yielded, we yield (__stack_go = false)
                        __stack_go <- __stack_body_fin

                        match sm.Data.Result with
                        | Ok _ -> ()
                        | Error _ -> cc <- false
                    else
                        __stack_go <- false


                __stack_go
            //-- RESUMABLE CODE END
            else
                EffBuilderBase.WhileDynamic(&sm, condition, body))


    /// Wraps a step in a try/with. This catches exceptions both in the evaluation of the function
    /// to retrieve the step, and in the continuation of the step (if any).
    member inline _.TryWith
        (
            body: EffectCode<_, 'TOverall, 'T, _>,
            catch: exn -> EffectCode<_, 'TOverall, 'T, _>
        ) : EffectCode<_, 'TOverall, 'T, _> =
        ResumableCode.TryWith(body, catch)

    /// Wraps a step in a try/finally. This catches exceptions both in the evaluation of the function
    /// to retrieve the step, and in the continuation of the step (if any).
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

    static member inline RecoverDynamic<'Env, 'T, 'TOverall, 'TResult, 'Err>
        (
            sm: byref<ResumableStateMachine<EffectStateMachineData<'Env,'TResult,'Err>>>,
            task: ValueTask<_>,
            continuation: ('Err -> 'TResult)
        ) : bool =
        let mutable awaiter = task.GetAwaiter()

        let cont =
            (EffectResumptionFunc<'Env,'TResult,'Err>(fun sm ->
                    let result = awaiter.GetResult()
                    match result with
                    | Ok result ->
                        sm.Data.Result <- Ok result
                        true
                    | Error error ->
                        sm.Data.Result <- Ok(continuation error)
                        true      
                )
            )

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
            sm: byref<ResumableStateMachine<EffectStateMachineData<'Env,'TResult,'Err2>>>,
            task: ValueTask<_>,
            f: ('Err1 -> 'Err2)
        ) : bool =
        let mutable awaiter = task.GetAwaiter()

        let cont =
            (EffectResumptionFunc<'Env,'TResult,'Err2>(fun sm ->
                    let result = awaiter.GetResult()
                    match result with
                    | Ok result ->
                        sm.Data.Result <- Result<_, 'Err2>.Ok result
                        true
                    | Error(error: 'Err1) ->
                        sm.Data.Result <- Result<_, 'Err2>.Error(f error)
                        true   
                )
            )

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

    static member inline ZipDynamic<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'Err when 'Err: (static member (+): 'Err -> 'Err -> 'Err)>
        (
            sm: byref<ResumableStateMachine<EffectStateMachineData<'Env,'TOverall,'Err>>>,
            task1: ValueTask<Result<'TResult1,'Err>>,
            task2: ValueTask<Result<'TResult2,'Err>>,
            f
        ) : bool =
        let mutable awaiter1 = task1.GetAwaiter()
        let mutable awaiter2 = task2.GetAwaiter()
        let cont =
            (EffectResumptionFunc<'Env,'TOverall,'Err>(fun sm ->
                    let cont =  
                        (EffectResumptionFunc<'Env,'TOverall,'Err>(fun sm -> 
                            let result = awaiter1.GetResult()
                            let result2 = awaiter2.GetResult()

                            match result, result2 with
                            | Ok result, Ok result2 ->
                                sm.Data.Result <- Ok(f struct (result, result2))
                                true
                            | Error error, Ok _ ->
                                sm.Data.Result <- Error error
                                true
                            | Ok _, Error result ->
                                sm.Data.Result <- Error result
                                true
                            | Error error, Error result ->
                                sm.Data.Result <- Error(error + result)
                                true                   
                                ))

                    if awaiter2.IsCompleted then
                        cont.Invoke(&sm)
                     else
                        sm.ResumptionDynamicInfo.ResumptionData <- (awaiter2 :> ICriticalNotifyCompletion)
                        sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                        false  
                )
            )

        if awaiter1.IsCompleted then
            cont.Invoke(&sm)
        else
            sm.ResumptionDynamicInfo.ResumptionData <- (awaiter1 :> ICriticalNotifyCompletion)
            sm.ResumptionDynamicInfo.ResumptionFunc <- cont
            false


    member inline _.Zip<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'Err
        when 'Err: (static member (+): 'Err -> 'Err -> 'Err)>
        (
            eff: Effect<'Env, 'TResult1, 'Err>,
            eff2: Effect<'Env, 'TResult2, 'Err>,
            f
        ) : EffectCode<'Env, _, 'T, 'Err> =
        EffectCode<'Env, 'TOverall, 'T, 'Err>(fun sm ->
            let task = eff.Run sm.Data.Environment
            let task2 = eff2.Run sm.Data.Environment

            if __useResumableCode then
                //-- RESUMABLE CODE START
                // Get an awaiter from the task
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
                            sm.Data.Result <- Ok(f struct (result, result2))
                            true
                        | Error error, Ok _ ->
                            sm.Data.Result <- Error error
                            true
                        | Ok _, Error result ->
                            sm.Data.Result <- Error result
                            true
                        | Error error, Error result ->
                            sm.Data.Result <- Error(error + result)
                            true
                    else
                        sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter2, &sm)
                        false
                else
                    sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                    false
            else
                EffBuilderBase.ZipDynamic(&sm, task, task2, f)
            )


    static member inline TryRecoverDynamic<'Env, 'T, 'TOverall, 'TResult, 'Err>
        (
            sm: byref<ResumableStateMachine<EffectStateMachineData<'Env,'TResult,'Err>>>,
            task: ValueTask<_>,
            continuation: ('Err -> Result<'TResult, 'Err>)
        ) : bool =
        let mutable awaiter = task.GetAwaiter()

        let cont =
            (EffectResumptionFunc<'Env,'TResult,'Err>(fun sm ->
                    let result = awaiter.GetResult()
                    match result with
                    | Ok result ->
                        sm.Data.Result <- Ok result
                        true
                    | Error error ->
                        sm.Data.Result <- continuation error
                        true      
                )
            )

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
                //-- RESUMABLE CODE START
                // Get an awaiter from the task
                let mutable awaiter = task.GetAwaiter()

                let mutable __stack_fin = true

                if not awaiter.IsCompleted then
                    // This will yield with __stack_yield_fin = false
                    // This will resume with __stack_yield_fin = true
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
            sm: byref<ResumableStateMachine<EffectStateMachineData<'Env,'TResult,'Err>>>,
            task: ValueTask<_>,
            continuation: ('Err -> Effect<'Env, 'TResult, 'Err>)
        ) : bool =
        let mutable awaiter = task.GetAwaiter()

        let cont =
            (EffectResumptionFunc<'Env,'TResult,'Err>(fun sm ->
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
                            (EffectResumptionFunc<'Env,'TResult,'Err>(fun sm ->
                                    sm.Data.Result <- awaiter.GetResult()
                                    true
                                    ))

                        if awaiter.IsCompleted then
                            cont.Invoke(&sm)
                        else
                            sm.ResumptionDynamicInfo.ResumptionData <- (awaiter :> ICriticalNotifyCompletion)
                            sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                            false    
                )
            )

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
                //-- RESUMABLE CODE START
                // Get an awaiter from the task
                let mutable awaiter = task1.GetAwaiter()

                let mutable __stack_fin = true

                if not awaiter.IsCompleted then
                    // This will yield with __stack_yield_fin = false
                    // This will resume with __stack_yield_fin = true
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


    member inline this.Bind2Return(m1: Effect<'Env, 'a, 'Err>, m2: Effect<'Env, 'b, 'Err>, [<InlineIfLambda>] f) =
        this.Zip(m1, m2, f)

    member inline this.MergeSources(m1, m2: Effect<_, _, _>) = this.Bind2Return(m1, m2, id)

/// <exclude/>
type EffBuilder() =
    inherit EffBuilderBase()

    // This is the dynamic implementation - this is not used
    // for statically compiled tasks.  An executor (resumptionFuncExecutor) is
    // registered with the state machine, plus the initial resumption.
    // The executor stays constant throughout the execution, it wraps each step
    // of the execution in a try/with.  The resumption is changed at each step
    // to represent the continuation of the computation.
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
                    //-- RESUMABLE CODE START
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

        (*  Not sure I want *)
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
                (EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->
                    let result = (^Awaiter: (member GetResult: unit -> 'TResult1) awaiter)
                    (continuation result).Invoke(&sm)))

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
                    //-- RESUMABLE CODE START
                    // Get an awaiter from the awaitable
                    let mutable awaiter = (^TaskLike: (member GetAwaiter: unit -> ^Awaiter) task)

                    let mutable __stack_fin = true

                    if not (^Awaiter: (member get_IsCompleted: unit -> bool) awaiter) then
                        // This will yield with __stack_yield_fin = false
                        // This will resume with __stack_yield_fin = true
                        let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                        __stack_fin <- __stack_yield_fin

                    if __stack_fin then
                        let result = (^Awaiter: (member GetResult: unit -> 'TResult1) awaiter)
                        (continuation result).Invoke(&sm)
                    else
                        sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                        false
                else
                    EffBuilder.BindDynamic(&sm, task, continuation)
            //-- RESUMABLE CODE END
            )

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
                (EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->
                    let result = awaiter.GetResult()
                    (continuation result).Invoke(&sm)))

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
                (EffectResumptionFunc<'Env, 'TOverall, 'Err>(fun sm ->
                    let result = awaiter.GetResult()

                    match result with
                    | Ok result -> (continuation result).Invoke(&sm)
                    | Error error ->
                        sm.Data.Result <- Error error
                        true

                ))

            // shortcut to continue immediately
            if awaiter.IsCompleted then
                cont.Invoke(&sm)
            else
                sm.ResumptionDynamicInfo.ResumptionData <- (awaiter :> ICriticalNotifyCompletion)
                sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                false

        member inline _.Bind<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'Err>
            (
                task: ValueTask<Result<'TResult1, 'Err>>,
                continuation: 'TResult1 -> EffectCode<'Env, 'TOverall, 'TResult2, 'Err>
            ) : EffectCode<'Env, 'TOverall, 'TResult2, 'Err> =

            EffectCode<'Env, 'TOverall, _, 'Err>(fun sm ->
                if __useResumableCode then
                    //-- RESUMABLE CODE START
                    // Get an awaiter from the task
                    let mutable awaiter = task.GetAwaiter()

                    let mutable __stack_fin = true

                    if not awaiter.IsCompleted then
                        // This will yield with __stack_yield_fin = false
                        // This will resume with __stack_yield_fin = true
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
                    EffBuilder.BindDynamic(&sm, task, continuation)
            //-- RESUMABLE CODE END
            )



        [<NoEagerConstraintApplication>]
        member inline _.Bind<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'Err>
            (
                eff: Effect<'Env, 'TResult1, 'Err>,
                continuation: 'TResult1 -> EffectCode<'Env, 'TOverall, 'TResult2, 'Err>

            ) : EffectCode<'Env, 'TOverall, 'TResult2, 'Err> =

            EffectCode<'Env, 'TOverall, _, 'Err>(fun sm ->
                let task = eff.Run sm.Data.Environment

                if __useResumableCode then
                    //-- RESUMABLE CODE START
                    // Get an awaiter from the task
                    let mutable awaiter = task.GetAwaiter()

                    let mutable __stack_fin = true

                    if not awaiter.IsCompleted then
                        // This will yield with __stack_yield_fin = false
                        // This will resume with __stack_yield_fin = true
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
                    EffBuilder.BindDynamic(&sm, task, continuation)
            //-- RESUMABLE CODE END
            )

        member inline this.Bind<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'Err>
            (
                task: ValueTask<'TResult1>,
                continuation: 'TResult1 -> EffectCode<'Env, 'TOverall, 'TResult2, 'Err>
            ) : EffectCode<'Env, 'TOverall, 'TResult2, 'Err> =
            EffectCode<'Env, 'TOverall, _, 'Err>(fun sm ->
                if __useResumableCode then
                    //-- RESUMABLE CODE START
                    // Get an awaiter from the task
                    let mutable awaiter = task.GetAwaiter()

                    let mutable __stack_fin = true

                    if not awaiter.IsCompleted then
                        // This will yield with __stack_yield_fin = false
                        // This will resume with __stack_yield_fin = true
                        let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                        __stack_fin <- __stack_yield_fin

                    if __stack_fin then
                        let result = awaiter.GetResult()
                        (continuation result).Invoke(&sm)

                    else
                        sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                        false
                else
                    EffBuilder.BindDynamic(&sm, task, continuation)
            //-- RESUMABLE CODE END
            )

        member inline this.Bind<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'Err>
            (
                task: Task<Result<'TResult1, 'Err>>,
                continuation: 'TResult1 -> EffectCode<'Env, 'TOverall, 'TResult2, 'Err>
            ) : EffectCode<'Env, 'TOverall, 'TResult2, 'Err> =
            this.Bind(ValueTask<_>(task = task), continuation)

        member inline this.Bind<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'Err>
            (
                t: Task<'TResult1>,
                continuation: 'TResult1 -> EffectCode<'Env, 'TOverall, 'TResult2, 'Err>
            ) : EffectCode<'Env, 'TOverall, 'TResult2, 'Err> =
            this.Bind(
                vtask {
                    let! result = t
                    return Ok result
                },
                continuation
            )

        member inline this.Bind(result: Result<_, _>, f) =
            this.Bind(ValueTask<_>(result = result), f)

        member inline _.Using<'Resource, 'TOverall, 'T, 'Env, 'Err when 'Resource :> IDisposable>
            (
                resource: 'Resource,
                body: 'Resource -> EffectCode<'Env, 'TOverall, 'T, 'Err>
            ) =
            ResumableCode.Using(resource, body)

        member inline this.For
            (
                s: IAsyncEnumerable<'a>,
                [<InlineIfLambda>] f1: 'a -> EffectCode<'Env, unit, unit, 'Err>
            ) =
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

        member inline this.For(s: IEnumerable<'a>, f1: 'a -> EffectCode<'Env, unit, unit, 'Err>) =
            this.Delay(fun () ->
                this.Using(
                    s.GetEnumerator(),
                    fun enumerator ->
                        let mutable continue' = false
                        continue' <- enumerator.MoveNext()

                        this.While(
                            (fun () -> continue'),
                            this.Combine(
                                (f1 enumerator.Current),
                                this.Delay(fun () ->
                                    continue' <- enumerator.MoveNext()
                                    this.Zero())
                            )
                        )
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

        member inline this.ReturnFrom(task: Effect<'Env, 'T, 'Err>) : EffectCode<'Env, 'T, 'T, 'Err> =
            this.Bind(task, (fun (t: 'T) -> this.Return t))

        member inline this.ReturnFrom(result: Result<'T, 'Err>) : EffectCode<'Env, 'T, 'T, 'Err> =
            this.Bind(ValueTask<_>(result), this.Return)


        //meh
        static member inline ZipDynamic<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'Err when 'Err: (static member (+): 'Err -> 'Err -> 'Err)>
            (
                sm: byref<ResumableStateMachine<EffectStateMachineData<'Env,'TOverall,'Err>>>,
                task1: ValueTask<Result<'TResult1,'Err>>,
                task2: ValueTask<Result<'TResult2,'Err>>,
                task3: ValueTask<Result<'TResult2,'Err>>,
                _f
            ) : bool = 
            let mutable awaiter1 = task1.GetAwaiter()      

            let cont =
                EffectResumptionFunc<_,_,_>(fun sm ->
                    let mutable awaiter2 = task2.GetAwaiter()
                    let cont =  
                        EffectResumptionFunc<'Env,'TOverall,'Err>(fun sm -> 
                            let mutable awaiter3 = task3.GetAwaiter()
                            let cont = EffectResumptionFunc<'Env,'TOverall,'Err>(fun sm -> failwith "")                        
                            if awaiter3.IsCompleted then
                                cont.Invoke(&sm)
                            else
                                sm.ResumptionDynamicInfo.ResumptionData <- (awaiter3 :> ICriticalNotifyCompletion)
                                sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                                false                   
                        )
                        
                    if awaiter2.IsCompleted then
                        cont.Invoke(&sm)
                    else
                        sm.ResumptionDynamicInfo.ResumptionData <- (awaiter2 :> ICriticalNotifyCompletion)
                        sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                        false  
                    )
                            
            if awaiter1.IsCompleted then
                cont.Invoke(&sm)
            else
                sm.ResumptionDynamicInfo.ResumptionData <- (awaiter1 :> ICriticalNotifyCompletion)
                sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                false





//Fsharp plus
type Effect<'R, 'T, 'E> with
    (* Functor *)
    ///Implements Map on the effect, making it a Functor
    static member inline Map(h: Effect<'r, 'a, 'e>, f: 'a -> 'b) =
        eff {
            let! e = h
            return f e
        }

    ///Implements Return on the effect
    static member inline Return(a) = eff { return a }

    (* Monad *)
    ///Implements Bind on the effect, which together with the functor Return makes it a Monad
    static member inline (>>=)(h: Effect<'r, 'a, 'e>, f: 'a -> Effect<'r, 'b, 'e>) =
        eff {
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
    static member inline (<*>)(f, e) = Effect.ap f e
