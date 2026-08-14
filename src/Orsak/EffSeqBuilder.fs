namespace Orsak

open System
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open System.Runtime.CompilerServices
open System.Threading.Tasks.Sources

open FSharp.Core.CompilerServices
open FSharp.Core.CompilerServices.StateMachineHelpers
open FSharp.Control

type EffectSeqDelegate<'r, 'a, 'e> = delegate of 'r -> IAsyncEnumerable<Result<'a, 'e>>

[<AbstractClass; NoComparison; NoEquality>]
type ResumableAsyncEnumerator<'T>() =
    abstract member MoveNext: unit -> unit

    [<DefaultValue(false)>]
    val mutable Token: CancellationToken

    //handles our implementation of IValueTaskSource<bool>
    [<DefaultValue(false)>]
    val mutable ValueTaskSource: ManualResetValueTaskSourceCore<bool>

    //allows MoveNextAsync()
    interface IValueTaskSource<bool> with
        member this.GetStatus token =
            let mutable s = &this.ValueTaskSource
            s.GetStatus token

        member this.GetResult token =
            let mutable s = &this.ValueTaskSource
            s.GetResult token

        member this.OnCompleted(continuation, state, token, flags) =
            let mutable s = &this.ValueTaskSource
            s.OnCompleted(continuation, state, token, flags)

    //allows external callers do drive the state-machine

    [<DefaultValue(false)>]
    val mutable InProgress: int

    /// Used by the AsyncEnumerator interface to return the Current value when
    /// IAsyncEnumerator.Current is called
    [<DefaultValue(false)>]
    val mutable Current: ValueOption<'T>


    interface IAsyncEnumerator<'T> with
        member this.Current =
            match this.Current with
            | ValueSome x -> x
            | ValueNone -> Unchecked.defaultof<_>

        member this.MoveNextAsync() =
            let mutable src = &this.ValueTaskSource
            src.Reset()

            if this.Token.IsCancellationRequested || this.InProgress = 0 then
                ValueTask<_> false
            else
                this.MoveNext()
                let version = src.Version

                match src.GetStatus(version) with
                | ValueTaskSourceStatus.Succeeded ->
                    let result = src.GetResult(version)
                    src.Reset()
                    ValueTask.FromResult result

                | ValueTaskSourceStatus.Faulted
                | ValueTaskSourceStatus.Canceled
                | ValueTaskSourceStatus.Pending
                | _ -> ValueTask<bool>(this, version)

        member this.DisposeAsync() = vtask {
            this.InProgress <- -1
            return ()
        }


[<Struct; NoComparison; NoEquality>]
type EffSeq<'r, 'a, 'e> =
    | Effect of EffectSeqDelegate<'r, 'a, 'e>

    member this.Invoke(e: 'r) = let (Effect f) = this in f.Invoke e

    member this.WithCancellation(outerToken) =
        let that = this
        //is this javascript?
        //not the most efficient impl, allocates more than we want
        EffSeq.Effect(
            EffectSeqDelegate(fun env ->
                { new IAsyncEnumerable<Result<'a, 'e>> with
                    member _.GetAsyncEnumerator(innerToken) =
                        let source = CancellationTokenSource.CreateLinkedTokenSource(innerToken, outerToken)
                        let enumerator = that.Invoke(env).GetAsyncEnumerator(source.Token)

                        { new IAsyncEnumerator<Result<'a, 'e>> with
                            member this.Current = enumerator.Current

                            member this.DisposeAsync() =
                                source.Dispose()
                                enumerator.DisposeAsync()

                            member this.MoveNextAsync() = enumerator.MoveNextAsync()
                        }
                })
        )

[<Struct; NoComparison; NoEquality>]
type EffSeqStateMachineData<'Env, 'T, 'Err> =
    //Environment of the effect
    [<DefaultValue(false)>]
    val mutable Env: 'Env

    [<DefaultValue(false)>]
    val mutable Awaiter: ICriticalNotifyCompletion

    [<DefaultValue(false)>]
    val mutable Enumerator: ResumableAsyncEnumerator<Result<'T, 'Err>>

and [<NoComparison; NoEquality>] EffectEnumerable<'Env, 'Machine, 'T, 'Err
    when 'Machine :> IAsyncStateMachine
    and 'Machine :> IResumableStateMachine<EffSeqStateMachineData<'Env, 'T, 'Err>>
    and 'Machine: struct>() =
    inherit ResumableAsyncEnumerator<Result<'T, 'Err>>()

    [<DefaultValue(false)>]
    val mutable InitialMachine: 'Machine

    member inline this.MoveNext(sm: byref<#IAsyncStateMachine>) = sm.MoveNext()
    //allows external callers to move the enumerator state
    override this.MoveNext() : unit = this.MoveNext(&this.StateMachine)

    [<DefaultValue(false)>]
    val mutable StateMachine: 'Machine

    interface IAsyncEnumerable<Result<'T, 'Err>> with
        member this.GetAsyncEnumerator(ct) =
            let enumerator =
                if Interlocked.CompareExchange(&this.InProgress, 1, -1) = -1 then
                    this
                else
                    EffectEnumerable<'Env, 'Machine, 'T, 'Err>(InProgress = 1)

            enumerator.Token <- ct

            //this gives the clone a COPY of our IResumableStateMachine, which at this point should be the initial machine.
            enumerator.StateMachine <- this.InitialMachine
            let mutable copy = &enumerator.StateMachine
            //the machine now has its own data, and can progress independent of the initial machine
            copy.Data <- EffSeqStateMachineData(Enumerator = enumerator, Env = copy.Data.Env)

            enumerator

and EffSeqCode<'Env, 'T, 'Err> = ResumableCode<EffSeqStateMachineData<'Env, 'T, 'Err>, unit>
and EffSeqStateMachine<'Env, 'T, 'Err> = ResumableStateMachine<EffSeqStateMachineData<'Env, 'T, 'Err>>
and EffSeqResumptionFunc<'Env, 'T, 'Err> = ResumptionFunc<EffSeqStateMachineData<'Env, 'T, 'Err>>
and EffSeqResumptionDynamicInfo<'Env, 'T, 'Err> = ResumptionDynamicInfo<EffSeqStateMachineData<'Env, 'T, 'Err>>

// <exclude/>
// Dynamic-path counterpart to EffectEnumerable. EffectEnumerable's GetAsyncEnumerator clones a
// fresh enumerator per call by struct-copying InitialMachine - correct for a compiler-specialized
// struct machine, whose entire resumption state (ResumptionPoint + captured locals) lives in the
// struct's own fields. It is NOT correct for the dynamic (ResumableStateMachine<'Data>-as-'Machine)
// case: there, "where to resume from" lives in ResumptionDynamicInfo.ResumptionFunc, a mutable field
// on a *reference-typed* object - struct-copying the machine copies the reference, not the object,
// so every clone (and every re-enumeration of the same EffSeq value) would share and stomp on the
// same ResumptionFunc. This type builds a completely fresh ResumptionDynamicInfo (and state machine)
// per GetAsyncEnumerator call instead of cloning anything.
[<NoComparison; NoEquality>]
type internal DynamicEffSeqEnumerator<'Env, 'T, 'Err>() =
    inherit ResumableAsyncEnumerator<Result<'T, 'Err>>()

    [<DefaultValue(false)>]
    val mutable StateMachine: EffSeqStateMachine<'Env, 'T, 'Err>

    // sm.MoveNext() isn't directly callable on a ResumableStateMachine<'Data> value - MoveNext is
    // an explicit IAsyncStateMachine interface implementation, and casting a byref struct to the
    // interface would box it (losing in-place mutation). Calling it through a generic parameter
    // constrained to #IAsyncStateMachine dispatches via `constrained.callvirt` instead, which works
    // on the byref in place - same trick EffectEnumerable's own MoveNext helper uses above.
    member inline private this.MoveNextOn(sm: byref<#IAsyncStateMachine>) = sm.MoveNext()

    override this.MoveNext() : unit = this.MoveNextOn(&this.StateMachine)

[<NoComparison; NoEquality>]
type internal DynamicEffSeqEnumerable<'Env, 'T, 'Err>(code: EffSeqCode<'Env, 'T, 'Err>, env: 'Env) =
    interface IAsyncEnumerable<Result<'T, 'Err>> with
        member _.GetAsyncEnumerator(ct) =
            let enumerator = DynamicEffSeqEnumerator<'Env, 'T, 'Err>(InProgress = 1)
            enumerator.Token <- ct

#nowarn "3513"
            let initialResumptionFunc =
                EffSeqResumptionFunc<'Env, 'T, 'Err>(fun sm -> code.Invoke(&sm))
#warnon "3513"

            let resumptionInfo =
                { new EffSeqResumptionDynamicInfo<'Env, 'T, 'Err>(initialResumptionFunc) with
                    member info.MoveNext(sm) =
                        let mutable savedExn = null

                        try
                            sm.ResumptionDynamicInfo.ResumptionData <- Unchecked.defaultof<obj>
                            let step = info.ResumptionFunc.Invoke(&sm)
                            sm.Data.Enumerator.InProgress <- Convert.ToInt32(not step)

                            if step || sm.Data.Enumerator.Current.IsSome then
                                sm.Data.Enumerator.ValueTaskSource.SetResult(not step)
                            else
                                let mutable awaiter =
                                    sm.ResumptionDynamicInfo.ResumptionData :?> ICriticalNotifyCompletion

                                assert not (isNull awaiter)
                                let enumerator = sm.Data.Enumerator
                                awaiter.UnsafeOnCompleted(fun () -> enumerator.MoveNext())
                        with exn ->
                            savedExn <- exn

                        match savedExn with
                        | null -> ()
                        | exn ->
                            sm.Data.Enumerator.ValueTaskSource.SetException exn
                            sm.Data.Enumerator.InProgress <- 0

                    member _.SetStateMachine(_, _) = ()
                }

            enumerator.StateMachine <- EffSeqStateMachine<'Env, 'T, 'Err>()
            let mutable sm = &enumerator.StateMachine
            sm.ResumptionDynamicInfo <- resumptionInfo
            sm.Data <- EffSeqStateMachineData(Enumerator = enumerator, Env = env)

            enumerator :> IAsyncEnumerator<Result<'T, 'Err>>

type EffSeqBuilder() =

    /// Dynamic (non-statically-reduced) implementation of Run, used automatically whenever the
    /// F# compiler cannot statically reduce an `effSeq { }` block into a compiled state machine.
    /// Drives a DynamicEffSeqEnumerable via a hand-written ResumptionDynamicInfo instead of a
    /// compiler-specialized struct machine; every suspend-capable EffSeqCode member (Bind,
    /// WhileAsync, TryFinallyAsync/2, ...) has its own dynamic branch to support this.
    static member RunDynamic(code: EffSeqCode<'Env, 'T, 'Err>) : EffSeq<'Env, 'T, 'Err> =
        EffSeq.Effect(
            EffectSeqDelegate(fun env ->
                DynamicEffSeqEnumerable<'Env, 'T, 'Err>(code, env) :> IAsyncEnumerable<Result<'T, 'Err>>)
        )
    member inline _.Delay(f: unit -> EffSeqCode<'Env, 'T, 'Err>) =
        EffSeqCode<'Env, 'T, 'Err>(fun sm -> f().Invoke(&sm))

    member inline _.Run(code: EffSeqCode<'Env, 'T, 'Err>) : EffSeq<'Env, 'T, 'Err> =
        if __useResumableCode<obj> then
            __stateMachine<EffSeqStateMachineData<'Env, 'T, 'Err>, EffSeq<'Env, 'T, 'Err>>
                (MoveNextMethodImpl<_>(fun sm ->
                    __resumeAt sm.ResumptionPoint
                    let mutable __stack_exn = Unchecked.defaultof<_>

                    try
                        let __stack_code_fin = code.Invoke(&sm)
                        sm.Data.Enumerator.InProgress <- Convert.ToInt32(not __stack_code_fin)
                        //always finish current iteration step if we complete the iteration, else finish it if we have gotten some data
                        if __stack_code_fin || sm.Data.Enumerator.Current.IsSome then
                            sm.Data.Enumerator.ValueTaskSource.SetResult(not __stack_code_fin)
                        else
                            let enumerator = sm.Data.Enumerator
                            sm.Data.Awaiter.UnsafeOnCompleted(fun () -> enumerator.MoveNext())
                    with exn ->
                        __stack_exn <- exn

                    match __stack_exn with
                    | null -> ()
                    | exn ->
                        sm.Data.Enumerator.ValueTaskSource.SetException exn
                        sm.Data.Enumerator.InProgress <- 0))
                (SetStateMachineMethodImpl<_>(fun _ _ -> ()))
                (AfterCode<_, _>(fun sm ->
                    let ts = EffectEnumerable<'Env, _, 'T, 'Err>(InitialMachine = sm, InProgress = -1)

                    EffSeq.Effect(
                        EffectSeqDelegate(fun env ->
                            ts.InitialMachine.Data.Env <- env
                            ts :> IAsyncEnumerable<Result<'T, 'Err>>)
                    )))
        else
            EffSeqBuilder.RunDynamic(code)


    member inline _.Zero() : EffSeqCode<'Env, 'T, 'Err> = ResumableCode.Zero()

    member inline _.Combine(task1: EffSeqCode<'Env, 'T, 'Err>, task2: EffSeqCode<'Env, 'T, 'Err>) =
        ResumableCode.Combine(task1, task2)

    member inline _.Yield(value: 'T) : EffSeqCode<'Env, 'T, 'Err> =
        EffSeqCode<'Env, 'T, 'Err>(fun sm ->
            let __stack_fin = ResumableCode.Yield().Invoke(&sm)
            sm.Data.Enumerator.Current <- ValueSome(Ok value)
            __stack_fin)

    // WhileAsync lives in the LowPrioritySeq module below (after Bind is defined) since
    // it needs this.Bind, which is added there as an EffSeqBuilder extension - extension
    // members aren't visible yet from inside this primary type definition.

    member inline _.While([<InlineIfLambda>] condition: unit -> bool, body: EffSeqCode<'Env, 'T, 'Err>) =
        ResumableCode.While(condition, body)

    member inline _.TryWith(body: EffSeqCode<'Env, 'T, 'Err>, catch: exn -> EffSeqCode<'Env, 'T, 'Err>) =
        ResumableCode.TryWith(body, catch)

    member inline _.TryFinallyAsync(body: EffSeqCode<'Env, 'T, 'Err>, compensationAction: unit -> Task) =
        ResumableCode.TryFinallyAsync(
            EffSeqCode<'Env, 'T, 'Err>(fun sm -> body.Invoke(&sm)),
            EffSeqCode<'Env, 'T, 'Err>(fun sm ->
                let task = compensationAction ()

                if __useResumableCode<obj> then
                    let mutable __stack_condition_fin = true

                    if not task.IsCompleted then
                        let mutable awaiter = task.GetAwaiter()
                        let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                        __stack_condition_fin <- __stack_yield_fin

                        if not __stack_condition_fin then
                            sm.Data.Awaiter <- awaiter
                            sm.Data.Enumerator.Current <- ValueNone

                    __stack_condition_fin
                else
                    let mutable awaiter = task.GetAwaiter()

                    let cont =
                        EffSeqResumptionFunc<'Env, 'T, 'Err>(fun sm ->
                            awaiter.GetResult()
                            true)

                    if awaiter.IsCompleted then
                        cont.Invoke(&sm)
                    else
                        sm.ResumptionDynamicInfo.ResumptionData <- (awaiter :> ICriticalNotifyCompletion)
                        sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                        sm.Data.Enumerator.Current <- ValueNone
                        false)
        )

    member inline _.TryFinally(body: EffSeqCode<'Env, 'T, 'Err>, compensationAction: unit -> unit) =
        ResumableCode.TryFinally(
            EffSeqCode<'Env, 'T, 'Err>(fun sm -> body.Invoke(&sm)),
            EffSeqCode<'Env, 'T, 'Err>(fun _ ->
                compensationAction ()
                true)
        )

    member inline this.Using(disp: #IAsyncDisposable, body: #IAsyncDisposable -> EffSeqCode<'Env, 'T, 'Err>) =
        this.TryFinallyAsync(
            (fun sm -> (body disp).Invoke(&sm)),
            (fun () ->
                if not (isNull (box disp)) then
                    disp.DisposeAsync().AsTask()
                else
                    Task.CompletedTask)
        )


[<AutoOpen>]
module LowPrioritySeq =
    type EffSeqBuilder with

#nowarn "3513"
        [<NoEagerConstraintApplication>]
        static member inline BindDynamic< ^TaskLike, 'Env, 'T, 'U, ^Awaiter, 'Err
            when ^TaskLike: (member GetAwaiter: unit -> ^Awaiter)
            and ^Awaiter :> ICriticalNotifyCompletion
            and ^Awaiter: (member get_IsCompleted: unit -> bool)
            and ^Awaiter: (member GetResult: unit -> 'T)>
            (
                sm: byref<EffSeqStateMachine<'Env, 'U, 'Err>>,
                task: ^TaskLike,
                continuation: 'T -> EffSeqCode<'Env, 'U, 'Err>
            ) : bool =

            let mutable awaiter = (^TaskLike: (member GetAwaiter: unit -> ^Awaiter) task)

            let cont =
                EffSeqResumptionFunc<'Env, 'U, 'Err>(fun sm ->
                    let result = (^Awaiter: (member GetResult: unit -> 'T) awaiter)
                    (continuation result).Invoke(&sm))

            if (^Awaiter: (member get_IsCompleted: unit -> bool) awaiter) then
                cont.Invoke(&sm)
            else
                sm.ResumptionDynamicInfo.ResumptionData <- (awaiter :> ICriticalNotifyCompletion)
                sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                sm.Data.Enumerator.Current <- ValueNone
                false
#warnon "3513"

        [<NoEagerConstraintApplication>]
        member inline _.Bind< ^TaskLike, 'Env, 'T, 'U, ^Awaiter, 'Err
            when ^TaskLike: (member GetAwaiter: unit -> ^Awaiter)
            and ^Awaiter :> ICriticalNotifyCompletion
            and ^Awaiter: (member get_IsCompleted: unit -> bool)
            and ^Awaiter: (member GetResult: unit -> 'T)>
            (
                task: ^TaskLike,
                [<InlineIfLambda>] continuation: 'T -> EffSeqCode<'Env, 'U, 'Err>
            ) =

            EffSeqCode<'Env, 'U, 'Err>(fun sm ->
                if __useResumableCode<obj> then
                    let mutable awaiter = (^TaskLike: (member GetAwaiter: unit -> ^Awaiter) task)
                    let mutable __stack_fin = true

                    if not (^Awaiter: (member get_IsCompleted: unit -> bool) awaiter) then
                        let __stack_fin2 = ResumableCode.Yield().Invoke(&sm)
                        __stack_fin <- __stack_fin2


                    if __stack_fin then
                        let result = (^Awaiter: (member GetResult: unit -> 'T) awaiter)
                        (continuation result).Invoke(&sm)

                    else
                        sm.Data.Awaiter <- awaiter
                        sm.Data.Enumerator.Current <- ValueNone
                        false
                else
                    EffSeqBuilder.BindDynamic(&sm, task, continuation))

        // Reuses this.Bind (above, which has its own static/dynamic split) for the
        // "await condition()" step instead of hand-rolling a suspend here - the
        // previous hand-rolled version only worked statically: its suspend branch wrote
        // sm.Data.Awaiter directly, which nothing in the dynamic driver (EffSeqBuilder.
        // RunDynamic) ever reads, so it deadlocked/asserted whenever condition() genuinely
        // suspended under the dynamic path. Reusing Bind also drops the .AsTask() the old
        // code forced on every suspend - ValueTaskAwaiter<'T> already satisfies Bind's
        // GetAwaiter/IsCompleted/GetResult constraints directly.
        member inline this.WhileAsync
            (
                [<InlineIfLambda>] condition: unit -> ValueTask<bool>,
                body: EffSeqCode<'Env, 'T, 'Err>
            ) : EffSeqCode<'Env, 'T, 'Err> =
            let mutable condition_res = true

            ResumableCode.While(
                (fun () -> condition_res),
                // While invokes this body value once per iteration, so condition() must be
                // called fresh from inside that per-invocation closure - calling this.Bind
                // (and therefore condition()) directly as an eager argument to While would
                // evaluate it once at construction time and reuse that one ValueTask/result
                // on every iteration (the same "hoisted out of the loop" bug already found
                // and fixed for For loops in commit 161475b).
                EffSeqCode<'Env, 'T, 'Err>(fun sm ->
                    (this.Bind(
                        condition (),
                        fun (result: bool) ->
                            condition_res <- result
                            EffSeqCode<'Env, 'T, 'Err>(fun sm -> if condition_res then body.Invoke(&sm) else true)
                    ))
                        .Invoke(&sm))
            )

        [<NoEagerConstraintApplication>]
        member inline _.TryFinallyAsync2< ^TaskLike, 'Env, 'T, ^Awaiter, 'Err
            when ^TaskLike: (member GetAwaiter: unit -> ^Awaiter)
            and ^Awaiter :> ICriticalNotifyCompletion
            and ^Awaiter: (member get_IsCompleted: unit -> bool)
            and ^Awaiter: (member GetResult: unit -> unit)>
            (
                body: EffSeqCode<'Env, 'T, 'Err>,
                [<InlineIfLambda>] compensationAction: unit -> ^TaskLike
            ) =
            ResumableCode.TryFinallyAsync(
                EffSeqCode<'Env, 'T, 'Err>(fun sm -> body.Invoke(&sm)),
                EffSeqCode<'Env, 'T, 'Err>(fun sm ->
                    let taskLike = compensationAction ()

                    if isNull (box taskLike) then
                        true
                    elif __useResumableCode<obj> then
                        let mutable __stack_condition_fin = true
                        let mutable awaiter = (^TaskLike: (member GetAwaiter: unit -> ^Awaiter) taskLike)

                        if not (^Awaiter: (member get_IsCompleted: unit -> bool) awaiter) then
                            let __stack_fin2 = ResumableCode.Yield().Invoke(&sm)
                            __stack_condition_fin <- __stack_fin2

                            if not __stack_condition_fin then
                                sm.Data.Awaiter <- awaiter
                                sm.Data.Enumerator.Current <- ValueNone

                        __stack_condition_fin
                    else
                        let mutable awaiter = (^TaskLike: (member GetAwaiter: unit -> ^Awaiter) taskLike)

                        let cont =
                            EffSeqResumptionFunc<'Env, 'T, 'Err>(fun sm ->
                                (^Awaiter: (member GetResult: unit -> unit) awaiter)
                                true)

                        if (^Awaiter: (member get_IsCompleted: unit -> bool) awaiter) then
                            cont.Invoke(&sm)
                        else
                            sm.ResumptionDynamicInfo.ResumptionData <- (awaiter :> ICriticalNotifyCompletion)
                            sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                            sm.Data.Enumerator.Current <- ValueNone
                            false)

            )

        //support for disposable-like, not sure if like
        [<NoEagerConstraintApplication>]
        member inline this.Using< ^DisposableLike, ^TaskLike, 'Env, 'T, ^Awaiter, 'Err
            when ^DisposableLike: (member DisposeAsync: unit -> ^TaskLike)
            and ^TaskLike: (member GetAwaiter: unit -> ^Awaiter)
            and ^Awaiter :> ICriticalNotifyCompletion
            and ^Awaiter: (member get_IsCompleted: unit -> bool)
            and ^Awaiter: (member GetResult: unit -> unit)>
            (
                disp: ^DisposableLike,
                [<InlineIfLambda>] body: ^DisposableLike -> EffSeqCode<'Env, 'T, 'Err>
            ) : EffSeqCode<'Env, 'T, 'Err> =
            this.TryFinallyAsync2(
                (fun sm -> (body disp).Invoke(&sm)),
                (fun () ->
                    if not (isNull (box disp)) then
                        (^DisposableLike: (member DisposeAsync: unit -> ^TaskLike) disp)
                    else
                        Unchecked.defaultof< ^TaskLike>)
            )

        //I guess we could support AsyncEnumerable-like, but it would be truly hideous
        //the signature for MoveNextAsync is also task-like, and so it would require extra support, and so on
        //with while!, doing it manually might be a viable alt for other like-types.
        [<NoEagerConstraintApplication>]
        member inline this.For
            (
                source: ConfiguredCancelableAsyncEnumerable<'TElement>,
                [<InlineIfLambda>] body: 'TElement -> EffSeqCode<'Env, 'T, 'Err>
            ) =
            EffSeqCode<'Env, 'T, 'Err>(fun sm ->
                this
                    .Using(
                        source.ConfigureAwait(false).GetAsyncEnumerator(),
                        fun e ->
                            let mutable e = e
                            let moveNext () = vtask { return! e.MoveNextAsync() }
                            this.WhileAsync(moveNext, (fun sm -> (body e.Current).Invoke(&sm)))
                    )
                    .Invoke(&sm))


[<AutoOpen>]
module MediumPriority =
    type EffSeqBuilder with

        member inline this.Using(dispensation: #IDisposable, body: #IDisposable -> EffSeqCode<'Env, 'T, 'Err>) =
            this.TryFinally(
                (fun sm -> (body dispensation).Invoke(&sm)),
                (fun () ->
                    if not (isNull (box dispensation)) then
                        dispensation.Dispose())
            )

        member inline this.For(sequence: seq<'TElement>, body: 'TElement -> EffSeqCode<'Env, 'T, 'Err>) =
            this.Using(
                sequence.GetEnumerator(),
                fun e -> this.While(e.MoveNext, (fun sm -> (body e.Current).Invoke(&sm)))
            )

        member inline this.YieldFrom(source: seq<'T>) : EffSeqCode<'Env, 'T, 'Err> = this.For(source, this.Yield)

        member inline this.For(source: #IAsyncEnumerable<'TElement>, body: 'TElement -> EffSeqCode<'Env, 'T, 'Err>) =
            EffSeqCode<'Env, 'T, 'Err>(fun sm ->
                this
                    .Using(
                        source.GetAsyncEnumerator(),
                        fun e -> this.WhileAsync(e.MoveNextAsync, (fun sm -> (body e.Current).Invoke(&sm)))
                    )
                    .Invoke(&sm))

        member inline this.For(source: EffSeq<'Env, 'TElement, 'Err>, body: 'TElement -> EffSeqCode<'Env, 'T, 'Err>) =
            EffSeqCode<'Env, 'T, 'Err>(fun sm ->
                this
                    .Using(
                        source.Invoke(sm.Data.Env).GetAsyncEnumerator(),
                        fun e ->
                            this.WhileAsync(
                                e.MoveNextAsync,
                                (fun sm ->
                                    match e.Current with
                                    | Ok current -> (body current).Invoke(&sm)
                                    | Error err ->
                                        sm.Data.Enumerator.Current <- ValueSome(Error err)
                                        false)
                            )
                    )
                    .Invoke(&sm))


        member inline this.YieldFrom(source: IAsyncEnumerable<'T>) =
            this.For(source, (fun v -> this.Yield(v)))

        [<NoEagerConstraintApplication>]
#nowarn "3513"
        static member BindDynamic
            (
                sm: byref<EffSeqStateMachine<'Env, 'TResult2, 'Err>>,
                task: ValueTask<Result<'TResult1, 'Err>>,
                continuation: 'TResult1 -> EffSeqCode<'Env, 'TResult2, 'Err>
            ) : bool =
            let mutable awaiter = task.GetAwaiter()

            let cont =
                EffSeqResumptionFunc<'Env, 'TResult2, 'Err>(fun sm ->
                    match awaiter.GetResult() with
                    | Ok result -> (continuation result).Invoke(&sm)
                    | Error err ->
                        sm.Data.Enumerator.Current <- ValueSome(Error err)
                        false)

            if awaiter.IsCompleted then
                cont.Invoke(&sm)
            else
                sm.ResumptionDynamicInfo.ResumptionData <- (awaiter :> ICriticalNotifyCompletion)
                sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                sm.Data.Enumerator.Current <- ValueNone
                false
#warnon "3513"

        member inline _.Bind<'Env, 'TResult1, 'TResult2, 'Err>
            (
                eff: Effect<'Env, 'TResult1, 'Err>,
                continuation: 'TResult1 -> EffSeqCode<'Env, 'TResult2, 'Err>
            ) =
            EffSeqCode<'Env, 'TResult2, 'Err>(fun sm ->
                let task = eff.Run sm.Data.Env

                if __useResumableCode<obj> then
                    let mutable awaiter = task.GetAwaiter()
                    let mutable __stack_fin = true

                    if not awaiter.IsCompleted then
                        let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                        __stack_fin <- __stack_yield_fin

                    if __stack_fin then

                        match awaiter.GetResult() with
                        | Ok result -> (continuation result).Invoke(&sm)
                        | Error err ->
                            sm.Data.Enumerator.Current <- ValueSome(Error err)
                            false

                    else
                        sm.Data.Awaiter <- awaiter
                        sm.Data.Enumerator.Current <- ValueNone
                        false
                else
                    EffSeqBuilder.BindDynamic(&sm, task, continuation))

        member inline this.YieldFrom(eff: Effect<'Env, 'T, 'Err>) : EffSeqCode<'Env, 'T, 'Err> =
            this.Bind<'Env, 'T, 'T, 'Err>(eff, this.Yield)

[<AutoOpen>]
module HighPrioritySeq =
    type EffSeqBuilder with

#nowarn "3513"
        static member BindDynamic
            (
                sm: byref<EffSeqStateMachine<'Env, 'U, 'Err>>,
                task: Task<'T>,
                continuation: 'T -> EffSeqCode<'Env, 'U, 'Err>
            ) : bool =
            let mutable awaiter = task.GetAwaiter()

            let cont =
                EffSeqResumptionFunc<'Env, 'U, 'Err>(fun sm ->
                    let result = awaiter.GetResult()
                    (continuation result).Invoke(&sm))

            if awaiter.IsCompleted then
                cont.Invoke(&sm)
            else
                sm.ResumptionDynamicInfo.ResumptionData <- (awaiter :> ICriticalNotifyCompletion)
                sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                sm.Data.Enumerator.Current <- ValueNone
                false
#warnon "3513"

        member inline _.Bind(task: Task<'T>, [<InlineIfLambda>] continuation: 'T -> EffSeqCode<'Env, 'U, 'Err>) =
            EffSeqCode<'Env, 'U, 'Err>(fun sm ->
                if __useResumableCode<obj> then
                    let mutable awaiter = task.GetAwaiter()
                    let mutable __stack_fin = true

                    if not awaiter.IsCompleted then
                        let __stack_fin2 = ResumableCode.Yield().Invoke(&sm)
                        __stack_fin <- __stack_fin2

                    if __stack_fin then
                        let result = awaiter.GetResult()
                        (continuation result).Invoke(&sm)
                    else
                        sm.Data.Awaiter <- awaiter
                        sm.Data.Enumerator.Current <- ValueNone
                        false
                else
                    EffSeqBuilder.BindDynamic(&sm, task, continuation))

        member inline this.Bind
            (
                computation: Async<'T>,
                [<InlineIfLambda>] continuation: 'T -> EffSeqCode<'Env, 'U, 'Err>
            ) =
            this.Bind(Async.StartAsTask(computation), continuation)

        member inline this.Bind
            (
                result: Result<'T, 'Err>,
                [<InlineIfLambda>] continuation: 'T -> EffSeqCode<'Env, 'U, 'Err>
            ) =
            EffSeqCode<'Env, 'U, 'Err>(fun sm ->
                match result with
                | Ok ok -> (continuation ok).Invoke(&sm)
                | Error err ->
                    sm.Data.Enumerator.Current <- ValueSome(Error err)
                    ResumableCode.Yield().Invoke(&sm))

    type EffBuilderBase with

        member inline this.For
            (
                s: EffSeq<'Env, 'a, 'Err>,
                [<InlineIfLambda>] f1: 'a -> EffectCode<'Env, 'T, unit, 'Err>
            ) =
            EffectCode<'Env, 'T, unit, 'Err>(fun sm ->
                this
                    .Using(
                        s.Invoke(sm.Data.Environment).GetAsyncEnumerator(),
                        fun enumerator ->
                            // Current must be read inside the loop body, same as For(IEnumerable, ...)
                            // above - While invokes the body value once per iteration, so on the dynamic
                            // path anything evaluated while constructing the body (as the bare match
                            // used to be) is hoisted out of the loop, reading Current before the first
                            // MoveNextAsync.
                            this.While(
                                enumerator.MoveNextAsync,
                                EffectCode<'Env, 'T, unit, 'Err>(fun sm ->
                                    (match enumerator.Current with
                                     | Ok ok -> f1 ok
                                     | Error err ->
                                         EffectCode<'Env, 'T, _, 'Err>(fun x ->
                                             x.Data.Result <- Error err
                                             true))
                                        .Invoke(&sm))
                            )
                    )
                    .Invoke(&sm))

[<AutoOpen>]
module EffSeqBuilder =
    let effSeq = EffSeqBuilder()
