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
type ResumableAsyncEnumerator<'T> =
    new: unit -> ResumableAsyncEnumerator<'T>
    abstract member MoveNext: unit -> unit

    [<DefaultValue(false)>]
    val mutable Token: CancellationToken

    [<DefaultValue(false)>]
    val mutable Registration: CancellationTokenRegistration

    //handles our implementation of IValueTaskSource<bool>
    [<DefaultValue(false)>]
    val mutable ValueTaskSource: ManualResetValueTaskSourceCore<bool>

    interface IValueTaskSource<bool>

    //allows external callers do drive the state-machine

    [<DefaultValue(false)>]
    val mutable InProgress: int

    /// Used by the AsyncEnumerator interface to return the Current value when
    /// IAsyncEnumerator.Current is called
    [<DefaultValue(false)>]
    val mutable Current: ValueOption<'T>

    interface IAsyncEnumerator<'T>

[<Struct; NoComparison; NoEquality>]
type EffSeq<'r, 'a, 'e> =
    | Effect of EffectSeqDelegate<'r, 'a, 'e>

    member Invoke: e: 'r -> IAsyncEnumerable<Result<'a, 'e>>
    member WithCancellation: outerToken: CancellationToken -> EffSeq<'r, 'a, 'e>

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
    and 'Machine: struct> =
    new: unit -> EffectEnumerable<'Env, 'Machine, 'T, 'Err>
    inherit ResumableAsyncEnumerator<Result<'T, 'Err>>

    [<DefaultValue(false)>]
    val mutable InitialMachine: 'Machine

    member inline MoveNext: sm: byref<#IAsyncStateMachine> -> unit
    override MoveNext: unit -> unit

    [<DefaultValue(false)>]
    val mutable StateMachine: 'Machine

    interface IAsyncEnumerable<Result<'T, 'Err>>

and EffSeqCode<'Env, 'T, 'Err> = ResumableCode<EffSeqStateMachineData<'Env, 'T, 'Err>, unit>
and EffSeqStateMachine<'Env, 'T, 'Err> = ResumableStateMachine<EffSeqStateMachineData<'Env, 'T, 'Err>>

type EffSeqBuilder =
    new: unit -> EffSeqBuilder
    member inline Delay: f: (unit -> EffSeqCode<'Env, 'T, 'Err>) -> EffSeqCode<'Env, 'T, 'Err>
    member inline Run: code: EffSeqCode<'Env, 'T, 'Err> -> EffSeq<'Env, 'T, 'Err>
    member inline Zero: unit -> EffSeqCode<'Env, 'T, 'Err>

    member inline Combine:
        task1: EffSeqCode<'Env, 'T, 'Err> * task2: EffSeqCode<'Env, 'T, 'Err> ->
            ResumableCode<EffSeqStateMachineData<'Env, 'T, 'Err>, unit>

    member inline Yield: value: 'T -> EffSeqCode<'Env, 'T, 'Err>

    member inline WhileAsync:
        [<InlineIfLambda>] condition: (unit -> ValueTask<bool>) * body: EffSeqCode<'Env, 'T, 'Err> ->
            EffSeqCode<'Env, 'T, 'Err>

    member inline While:
        [<InlineIfLambda>] condition: (unit -> bool) * body: EffSeqCode<'Env, 'T, 'Err> ->
            ResumableCode<EffSeqStateMachineData<'Env, 'T, 'Err>, unit>

    member inline TryWith:
        body: EffSeqCode<'Env, 'T, 'Err> * catch: (exn -> EffSeqCode<'Env, 'T, 'Err>) ->
            ResumableCode<EffSeqStateMachineData<'Env, 'T, 'Err>, unit>

    member inline TryFinallyAsync:
        body: EffSeqCode<'Env, 'T, 'Err> * compensationAction: (unit -> Task) ->
            ResumableCode<EffSeqStateMachineData<'Env, 'T, 'Err>, unit>

    member inline TryFinally:
        body: EffSeqCode<'Env, 'T, 'Err> * compensationAction: (unit -> unit) ->
            ResumableCode<EffSeqStateMachineData<'Env, 'T, 'Err>, unit>

    member inline Using:
        disp: 'a * body: ('a -> EffSeqCode<'Env, 'T, 'Err>) ->
            ResumableCode<EffSeqStateMachineData<'Env, 'T, 'Err>, unit>
            when 'a :> IAsyncDisposable

[<AutoOpen>]
module LowPrioritySeq =
    type EffSeqBuilder with
        [<NoEagerConstraintApplication>]
        member inline Bind:
            task: ^TaskLike * [<InlineIfLambda>] continuation: ('T -> EffSeqCode<'Env, 'U, 'Err>) ->
                EffSeqCode<'Env, 'U, 'Err>
                when ^TaskLike: (member GetAwaiter: unit -> ^Awaiter)
                and ^Awaiter :> ICriticalNotifyCompletion
                and ^Awaiter: (member get_IsCompleted: unit -> bool)
                and ^Awaiter: (member GetResult: unit -> 'T)

        [<NoEagerConstraintApplication>]
        member inline TryFinallyAsync2:
            body: EffSeqCode<'Env, 'T, 'Err> * [<InlineIfLambda>] compensationAction: (unit -> ^TaskLike) ->
                ResumableCode<EffSeqStateMachineData<'Env, 'T, 'Err>, unit>
                when ^TaskLike: (member GetAwaiter: unit -> ^Awaiter)
                and ^Awaiter :> ICriticalNotifyCompletion
                and ^Awaiter: (member get_IsCompleted: unit -> bool)
                and ^Awaiter: (member GetResult: unit -> unit)

        [<NoEagerConstraintApplication>]
        member inline Using:
            disp: ^DisposableLike * [<InlineIfLambda>] body: (^DisposableLike -> EffSeqCode<'Env, 'T, 'Err>) ->
                EffSeqCode<'Env, 'T, 'Err>
                when ^DisposableLike: (member DisposeAsync: unit -> ^TaskLike)
                and ^TaskLike: (member GetAwaiter: unit -> ^Awaiter)
                and ^Awaiter :> ICriticalNotifyCompletion
                and ^Awaiter: (member get_IsCompleted: unit -> bool)
                and ^Awaiter: (member GetResult: unit -> unit)

        [<NoEagerConstraintApplication>]
        member inline For:
            source: ConfiguredCancelableAsyncEnumerable<'TElement> *
            [<InlineIfLambda>] body: ('TElement -> EffSeqCode<'Env, 'T, 'Err>) ->
                EffSeqCode<'Env, 'T, 'Err>

[<AutoOpen>]
module MediumPriority =
    type EffSeqBuilder with
        member inline Using:
            dispensation: 'a * body: ('a -> EffSeqCode<'Env, 'T, 'Err>) ->
                ResumableCode<EffSeqStateMachineData<'Env, 'T, 'Err>, unit>
                when 'a :> IDisposable

        member inline For:
            sequence: 'TElement seq * body: ('TElement -> EffSeqCode<'Env, 'T, 'Err>) ->
                ResumableCode<EffSeqStateMachineData<'Env, 'T, 'Err>, unit>

        member inline YieldFrom: source: 'T seq -> EffSeqCode<'Env, 'T, 'Err>

        member inline For:
            source: #IAsyncEnumerable<'TElement> * body: ('TElement -> EffSeqCode<'Env, 'T, 'Err>) ->
                EffSeqCode<'Env, 'T, 'Err>

        member inline For:
            source: EffSeq<'Env, 'TElement, 'Err> * body: ('TElement -> EffSeqCode<'Env, 'T, 'Err>) ->
                EffSeqCode<'Env, 'T, 'Err>

        member inline YieldFrom: source: IAsyncEnumerable<'T> -> EffSeqCode<'a, 'T, 'b>

        [<NoEagerConstraintApplication>]
        member inline Bind<'Env, 'TResult1, 'TResult2, 'Err> :
            eff: Effect<'Env, 'TResult1, 'Err> * continuation: ('TResult1 -> EffSeqCode<'Env, 'TResult2, 'Err>) ->
                EffSeqCode<'Env, 'TResult2, 'Err>

        member inline YieldFrom: eff: Effect<'Env, 'T, 'Err> -> EffSeqCode<'Env, 'T, 'Err>

[<AutoOpen>]
module HighPrioritySeq =
    type EffSeqBuilder with
        member inline Bind:
            task: Task<'T> * [<InlineIfLambda>] continuation: ('T -> EffSeqCode<'Env, 'U, 'Err>) ->
                EffSeqCode<'Env, 'U, 'Err>

        member inline Bind:
            computation: Async<'T> * [<InlineIfLambda>] continuation: ('T -> EffSeqCode<'Env, 'U, 'Err>) ->
                EffSeqCode<'Env, 'U, 'Err>

        member inline Bind:
            result: Result<'T, 'Err> * [<InlineIfLambda>] continuation: ('T -> EffSeqCode<'Env, 'U, 'Err>) ->
                EffSeqCode<'Env, 'U, 'Err>

    type EffBuilderBase with
        member inline For:
            s: EffSeq<'Env, 'a, 'Err> * [<InlineIfLambda>] f1: ('a -> EffectCode<'Env, 'T, unit, 'Err>) ->
                EffectCode<'Env, 'T, unit, 'Err>

[<AutoOpen>]
module EffSeqBuilder =
    val effSeq: EffSeqBuilder
