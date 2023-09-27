namespace Orsak

#nowarn "57" // note: this is *not* an experimental feature, but they forgot to switch off the flag

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

    [<DefaultValue(false)>]
    val mutable Registration: CancellationTokenRegistration

    //handles our implementation of IValueTaskSource<bool>
    [<DefaultValue(false)>]
    val mutable ValueTaskSource: ManualResetValueTaskSourceCore<bool>

    //allows MoveNextAsync()
    interface IValueTaskSource<bool> with
        member this.GetStatus token = this.ValueTaskSource.GetStatus token

        member this.GetResult token = this.ValueTaskSource.GetResult token

        member this.OnCompleted(continuation, state, token, flags) =
            this.ValueTaskSource.OnCompleted(continuation, state, token, flags)

    //allows external callers do drive the state-machine

    /// allows MoveNextAsync know that we are fully done, and will never move again.
    [<DefaultValue(false)>]
    val mutable IsComplete: bool

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

            if this.Token.IsCancellationRequested || this.IsComplete then
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

        member this.DisposeAsync() = this.Registration.DisposeAsync()

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
                if this.IsComplete then
                    EffectEnumerable<'Env, 'Machine, 'T, 'Err>()
                else
                    this

            enumerator.Token <- ct

            enumerator.Registration <-
                ct.Register(fun () ->
                    let mutable src = &enumerator.ValueTaskSource
                    src.SetResult(false))

            //this gives the clone a COPY of our IResumableStateMachine, which at this point should be the initial machine.
            enumerator.StateMachine <- this.InitialMachine
            let mutable copy = &enumerator.StateMachine
            //the machine now has it's own data, and can progress independent of the initial machine
            copy.Data <- EffSeqStateMachineData(Enumerator = enumerator, Env = copy.Data.Env)

            enumerator

and EffSeqCode<'Env, 'T, 'Err> = ResumableCode<EffSeqStateMachineData<'Env, 'T, 'Err>, unit>
and EffSeqStateMachine<'Env, 'T, 'Err> = ResumableStateMachine<EffSeqStateMachineData<'Env, 'T, 'Err>>

type EffSeqBuilder() =
    member inline _.Delay(f: unit -> EffSeqCode<'Env, 'T, 'Err>) =
        EffSeqCode<'Env, 'T, 'Err>(fun sm -> f().Invoke(&sm))

    member inline _.Run(code: EffSeqCode<'Env, 'T, 'Err>) : EffSeq<'Env, 'T, 'Err> =
        if __useResumableCode then
            __stateMachine<EffSeqStateMachineData<'Env, 'T, 'Err>, EffSeq<'Env, 'T, 'Err>>
                (MoveNextMethodImpl<_>(fun sm ->
                    __resumeAt sm.ResumptionPoint
                    let mutable __stack_exn = Unchecked.defaultof<_>

                    try
                        let __stack_code_fin = code.Invoke(&sm)
                        sm.Data.Enumerator.IsComplete <- __stack_code_fin
                        //always finish current itteration step if we complete the itteration, else finish it if we have gotten some data
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
                        sm.Data.Enumerator.IsComplete <- true))
                (SetStateMachineMethodImpl<_>(fun _ _ -> ()))
                (AfterCode<_, _>(fun sm ->
                    let ts = EffectEnumerable<'Env, _, 'T, 'Err>(InitialMachine = sm)

                    EffSeq.Effect(
                        EffectSeqDelegate(fun env ->
                            ts.InitialMachine.Data.Env <- env
                            ts :> IAsyncEnumerable<Result<'T, 'Err>>)
                    )))
        else
            NotImplementedException "No dynamic implementation yet." |> raise


    member inline _.Zero() : EffSeqCode<'Env, 'T, 'Err> = ResumableCode.Zero()

    member inline _.Combine(task1: EffSeqCode<'Env, 'T, 'Err>, task2: EffSeqCode<'Env, 'T, 'Err>) =
        ResumableCode.Combine(task1, task2)

    member inline _.Yield(value: 'T) : EffSeqCode<'Env, 'T, 'Err> =
        EffSeqCode<'Env, 'T, 'Err>(fun sm ->
            let __stack_fin = ResumableCode.Yield().Invoke(&sm)
            sm.Data.Enumerator.Current <- ValueSome(Ok value)
            __stack_fin)

    member inline _.WhileAsync
        (
            [<InlineIfLambda>] condition: unit -> ValueTask<bool>,
            body: EffSeqCode<'Env, 'T, 'Err>
        ) : EffSeqCode<'Env, 'T, 'Err> =
        let mutable condition_res = true

        ResumableCode.While(
            (fun () -> condition_res),
            EffSeqCode<'Env, 'T, 'Err>(fun sm ->
                let mutable __stack_condition_fin = true
                let __stack_vtask = condition ()

                if __stack_vtask.IsCompleted then
                    __stack_condition_fin <- true
                    condition_res <- __stack_vtask.Result
                else
                    let task = __stack_vtask.AsTask()
                    let mutable awaiter = task.GetAwaiter()
                    let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                    __stack_condition_fin <- __stack_yield_fin

                    if __stack_condition_fin then
                        condition_res <- task.Result
                    else
                        sm.Data.Awaiter <- awaiter
                        sm.Data.Enumerator.Current <- ValueNone


                if __stack_condition_fin then
                    if condition_res then body.Invoke(&sm) else true
                else
                    false)
        )

    member inline _.While([<InlineIfLambda>] condition: unit -> bool, body: EffSeqCode<'Env, 'T, 'Err>) =
        ResumableCode.While(condition, body)

    member inline _.TryWith(body: EffSeqCode<'Env, 'T, 'Err>, catch: exn -> EffSeqCode<'Env, 'T, 'Err>) =
        ResumableCode.TryWith(body, catch)

    member inline _.TryFinallyAsync(body: EffSeqCode<'Env, 'T, 'Err>, compensationAction: unit -> Task) =
        ResumableCode.TryFinallyAsync(
            EffSeqCode<'Env, 'T, 'Err>(fun sm -> body.Invoke(&sm)),
            EffSeqCode<'Env, 'T, 'Err>(fun sm ->
                let mutable __stack_condition_fin = true
                let task = compensationAction ()

                if not task.IsCompleted then
                    let mutable awaiter = task.GetAwaiter()
                    let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                    __stack_condition_fin <- __stack_yield_fin

                    if not __stack_condition_fin then
                        sm.Data.Awaiter <- awaiter
                        sm.Data.Enumerator.Current <- ValueNone

                __stack_condition_fin)
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

        [<NoEagerConstraintApplication>]
        member inline _.Bind< ^TaskLike, 'Env, 'T, 'U, ^Awaiter, 'TOverall, 'Err
            when ^TaskLike: (member GetAwaiter: unit -> ^Awaiter)
            and ^Awaiter :> ICriticalNotifyCompletion
            and ^Awaiter: (member get_IsCompleted: unit -> bool)
            and ^Awaiter: (member GetResult: unit -> 'T)>
            (
                task: ^TaskLike,
                [<InlineIfLambda>] continuation: ('T -> EffSeqCode<'Env, 'U, 'Err>)
            ) =

            EffSeqCode<'Env, 'U, 'Err>(fun sm ->
                let mutable awaiter = (^TaskLike: (member GetAwaiter: unit -> ^Awaiter) (task))
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
                    false)

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

                    if not (isNull (box taskLike)) then
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
                        true)

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
                            this.WhileAsync((moveNext), (fun sm -> (body e.Current).Invoke(&sm)))
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
        member inline _.Bind<'Env, 'TResult1, 'TResult2, 'Err>
            (
                eff: Effect<'Env, 'TResult1, 'Err>,
                continuation: ('TResult1 -> EffSeqCode<'Env, 'TResult2, 'Err>)
            ) =
            EffSeqCode<'Env, 'TResult2, 'Err>(fun sm ->
                let task = eff.Run sm.Data.Env

                if __useResumableCode then
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
                    failwith "lazy")

        member inline this.YieldFrom(eff: Effect<'Env, 'T, 'Err>) : EffSeqCode<'Env, 'T, 'Err> =
            this.Bind<'Env, 'T, 'T, 'Err>(eff, this.Yield)

[<AutoOpen>]
module HighPrioritySeq =
    type EffSeqBuilder with

        member inline _.Bind(task: Task<'T>, [<InlineIfLambda>] continuation: ('T -> EffSeqCode<'Env, 'U, 'Err>)) =
            EffSeqCode<'Env, 'U, 'Err>(fun sm ->
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
                    false)

        member inline this.Bind
            (
                computation: Async<'T>,
                [<InlineIfLambda>] continuation: ('T -> EffSeqCode<'Env, 'U, 'Err>)
            ) =
            this.Bind(Async.StartAsTask(computation), continuation)

        member inline this.Bind
            (
                result: Result<'T, 'Err>,
                [<InlineIfLambda>] continuation: ('T -> EffSeqCode<'Env, 'U, 'Err>)
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
            EffectCode<'Env, 'T, unit, 'Err>(
                (fun sm ->
                    this
                        .Using(
                            s.Invoke(sm.Data.Environment).GetAsyncEnumerator(),
                            fun enumerator ->
                                this.While(
                                    (enumerator.MoveNextAsync),
                                    (match enumerator.Current with
                                     | Ok ok -> f1 ok
                                     | Error err ->
                                         EffectCode<'Env, 'T, _, 'Err>(fun x ->
                                             x.Data.Result <- Error err
                                             true))
                                )
                        )
                        .Invoke(&sm))
            )

[<AutoOpen>]
module EffSeqBuilder =
    let effSeq = EffSeqBuilder()
