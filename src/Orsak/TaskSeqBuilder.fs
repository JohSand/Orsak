namespace Orsak.Control

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Threading
open System.Threading.Tasks
open FSharp.Control
open FSharp.Core.CompilerServices
open FSharp.Core.CompilerServices.StateMachineHelpers
open Orsak

[<Struct; NoComparison; NoEquality>]
type TaskSeqStateMachineData<'T> =

    [<DefaultValue(false)>]
    val mutable Awaiter: ICriticalNotifyCompletion

    [<DefaultValue(false)>]
    val mutable Enumerator: ResumableAsyncEnumerator<'T>

and [<NoComparison; NoEquality>] TaskSeqEnumerable<'Machine, 'T
    when 'Machine :> IAsyncStateMachine
    and 'Machine :> IResumableStateMachine<TaskSeqStateMachineData<'T>>
    and 'Machine: struct>() =
    inherit ResumableAsyncEnumerator<'T>()

    [<DefaultValue(false)>]
    val mutable InitialMachine: 'Machine

    member inline this.MoveNext(sm: byref<#IAsyncStateMachine>) = sm.MoveNext()
    //allows external callers to move the enumerator state
    override this.MoveNext() : unit = this.MoveNext(&this.StateMachine)

    [<DefaultValue(false)>]
    val mutable StateMachine: 'Machine

    interface IAsyncEnumerable<'T> with
        member this.GetAsyncEnumerator(ct) =
            let enumerator =
                if Interlocked.CompareExchange(&this.InProgress, 1, -1) = -1 then
                    this
                else
                    TaskSeqEnumerable<'Machine, 'T>(InProgress = 1)

            enumerator.Token <- ct

            //this gives the clone a COPY of our IResumableStateMachine, which at this point should be the initial machine.
            enumerator.StateMachine <- this.InitialMachine
            let mutable copy = &enumerator.StateMachine
            //the machine now has its own data, and can progress independent of the initial machine
            copy.Data <- TaskSeqStateMachineData(Enumerator = enumerator)

            enumerator

and TaskSeqCode<'T> = ResumableCode<TaskSeqStateMachineData<'T>, unit>
and TaskSeqStateMachine<'T> = ResumableStateMachine<TaskSeqStateMachineData<'T>>

type TaskSeqBuilder() =

    [<NoCompilerInlining>]
    static member notImplemented() : IAsyncEnumerable<'T> =
        raise (NotImplementedException "No dynamic implementation yet.")

    member inline _.Delay(f: unit -> TaskSeqCode<'T>) =
        TaskSeqCode<'T>(fun sm -> f().Invoke(&sm))

    member inline _.Run(code: TaskSeqCode<'T>) : IAsyncEnumerable<'T> =
        if __useResumableCode<obj> then
            __stateMachine<TaskSeqStateMachineData<'T>, IAsyncEnumerable<'T>>
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
                    TaskSeqEnumerable<_, 'T>(InitialMachine = sm, InProgress = -1) :> IAsyncEnumerable<'T>))
        else
            TaskSeqBuilder.notImplemented ()


    member inline _.Zero() : TaskSeqCode<'T> = ResumableCode.Zero()

    member inline _.Combine(task1: TaskSeqCode<'T>, task2: TaskSeqCode<'T>) = ResumableCode.Combine(task1, task2)

    member inline _.Yield(value: 'T) : TaskSeqCode<'T> =
        TaskSeqCode<'T>(fun sm ->
            let __stack_fin = ResumableCode.Yield().Invoke(&sm)
            sm.Data.Enumerator.Current <- ValueSome(value)
            __stack_fin)

    member inline _.WhileAsync
        ([<InlineIfLambda>] condition: unit -> ValueTask<bool>, body: TaskSeqCode<'T>)
        : TaskSeqCode<'T> =
        let mutable condition_res = true

        ResumableCode.While(
            (fun () -> condition_res),
            TaskSeqCode<'T>(fun sm ->
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

    member inline _.While([<InlineIfLambda>] condition: unit -> bool, body: TaskSeqCode<'T>) =
        ResumableCode.While(condition, body)

    member inline _.TryWith(body: TaskSeqCode<'T>, catch: exn -> TaskSeqCode<'T>) = ResumableCode.TryWith(body, catch)

    member inline _.TryFinallyAsync(body: TaskSeqCode<'T>, compensationAction: unit -> Task) =
        ResumableCode.TryFinallyAsync(
            TaskSeqCode<'T>(fun sm -> body.Invoke(&sm)),
            TaskSeqCode<'T>(fun sm ->
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

    member inline _.TryFinally(body: TaskSeqCode<'T>, compensationAction: unit -> unit) =
        ResumableCode.TryFinally(
            TaskSeqCode<'T>(fun sm -> body.Invoke(&sm)),
            TaskSeqCode<'T>(fun _ -> let () = compensationAction () in true)
        )

    member inline this.Using(disp: #IAsyncDisposable, body: #IAsyncDisposable -> TaskSeqCode<'T>) =
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
    type TaskSeqBuilder with

        [<NoEagerConstraintApplication>]
        member inline _.Bind< ^TaskLike, 'T, 'U, ^Awaiter, 'TOverall
            when ^TaskLike: (member GetAwaiter: unit -> ^Awaiter)
            and ^Awaiter :> ICriticalNotifyCompletion
            and ^Awaiter: (member get_IsCompleted: unit -> bool)
            and ^Awaiter: (member GetResult: unit -> 'T)>
            (task: ^TaskLike, [<InlineIfLambda>] continuation: 'T -> TaskSeqCode<'U>)
            =

            TaskSeqCode<'U>(fun sm ->
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
                    false)

        [<NoEagerConstraintApplication>]
        member inline _.TryFinallyAsync2< ^TaskLike, 'T, ^Awaiter
            when ^TaskLike: (member GetAwaiter: unit -> ^Awaiter)
            and ^Awaiter :> ICriticalNotifyCompletion
            and ^Awaiter: (member get_IsCompleted: unit -> bool)
            and ^Awaiter: (member GetResult: unit -> unit)>
            (body: TaskSeqCode<'T>, [<InlineIfLambda>] compensationAction: unit -> ^TaskLike)
            =
            ResumableCode.TryFinallyAsync(
                TaskSeqCode<'T>(fun sm -> body.Invoke(&sm)),
                TaskSeqCode<'T>(fun sm ->
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
        member inline this.Using< ^DisposableLike, ^TaskLike, 'T, ^Awaiter
            when ^DisposableLike: (member DisposeAsync: unit -> ^TaskLike)
            and ^TaskLike: (member GetAwaiter: unit -> ^Awaiter)
            and ^Awaiter :> ICriticalNotifyCompletion
            and ^Awaiter: (member get_IsCompleted: unit -> bool)
            and ^Awaiter: (member GetResult: unit -> unit)>
            (disp: ^DisposableLike, [<InlineIfLambda>] body: ^DisposableLike -> TaskSeqCode<'T>)
            : TaskSeqCode<'T> =
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
                [<InlineIfLambda>] body: 'TElement -> TaskSeqCode<'T>
            ) =
            TaskSeqCode<'T>(fun sm ->
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
    type TaskSeqBuilder with

        member inline this.Using(dispensation: #IDisposable, body: #IDisposable -> TaskSeqCode<'T>) =
            this.TryFinally(
                (fun sm -> (body dispensation).Invoke(&sm)),
                (fun () ->
                    if not (isNull (box dispensation)) then
                        dispensation.Dispose())
            )

        member inline this.For(sequence: seq<'TElement>, body: 'TElement -> TaskSeqCode<'T>) =
            this.Using(
                sequence.GetEnumerator(),
                fun e -> this.While(e.MoveNext, (fun sm -> (body e.Current).Invoke(&sm)))
            )

        member inline this.YieldFrom(source: seq<'T>) : TaskSeqCode<'T> = this.For(source, this.Yield)

        member inline this.For(source: #IAsyncEnumerable<'TElement>, body: 'TElement -> TaskSeqCode<'T>) =
            this.Using(
                source.GetAsyncEnumerator(),
                fun e -> this.WhileAsync(e.MoveNextAsync, (fun sm -> (body e.Current).Invoke(&sm)))
            )


        member inline this.YieldFrom(source: IAsyncEnumerable<'T>) =
            this.For(source, (fun v -> this.Yield(v)))

[<AutoOpen>]
module HighPrioritySeq =
    type TaskSeqBuilder with

        member inline _.Bind(task: Task<'T>, [<InlineIfLambda>] continuation: 'T -> TaskSeqCode<'U>) =
            TaskSeqCode<'U>(fun sm ->
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

        member inline this.Bind(computation: Async<'T>, [<InlineIfLambda>] continuation: 'T -> TaskSeqCode<'U>) =
            this.Bind(Async.StartImmediateAsTask(computation), continuation)

[<AutoOpen>]
module TaskSeqBuilder =
    let taskSeq = TaskSeqBuilder()

    type TaskBuilder with

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
