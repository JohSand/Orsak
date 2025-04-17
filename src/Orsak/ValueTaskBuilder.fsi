namespace FSharp.Control

#nowarn "57"
#nowarn "3511"
#nowarn "3513"

open System
open System.Runtime.CompilerServices
open System.Threading.Tasks
open Microsoft.FSharp.Core
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Core.CompilerServices.StateMachineHelpers
open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators
open Microsoft.FSharp.Control
open Microsoft.FSharp.Collections

/// <exclude/>
/// The extra data stored in ResumableStateMachine for tasks
[<Struct; NoComparison; NoEquality>]
type ValueValueTaskStateMachineData<'T> =

    [<DefaultValue(false)>]
    val mutable Result: 'T

    [<DefaultValue(false)>]
    val mutable MethodBuilder: PoolingAsyncValueTaskMethodBuilder<'T>

/// <exclude/>
and ValueTaskStateMachine<'TOverall> = ResumableStateMachine<ValueValueTaskStateMachineData<'TOverall>>
/// <exclude/>
and ValueTaskResumptionFunc<'TOverall> = ResumptionFunc<ValueValueTaskStateMachineData<'TOverall>>
/// <exclude/>
and ValueTaskResumptionDynamicInfo<'TOverall> = ResumptionDynamicInfo<ValueValueTaskStateMachineData<'TOverall>>
/// <exclude/>
and ValueTaskCode<'TOverall, 'T> = ResumableCode<ValueValueTaskStateMachineData<'TOverall>, 'T>

/// <exclude/>
type ValueTaskBuilder =
    new: unit -> ValueTaskBuilder
    member inline Delay: generator: (unit -> ValueTaskCode<'TOverall, 'T>) -> ValueTaskCode<'TOverall, 'T>

    /// Used to represent no-ops like the implicit empty "else" branch of an "if" expression.
    [<DefaultValue>]
    member inline Zero: unit -> ValueTaskCode<'TOverall, unit>

    member inline Return: value: 'T -> ValueTaskCode<'T, 'T>

    /// Chains together a step with its following step.
    /// Note that this requires that the first step has no result.
    /// This prevents constructs like `task { return 1; return 2; }`.
    member inline Combine:
        task1: ValueTaskCode<'TOverall, unit> * task2: ValueTaskCode<'TOverall, 'T> -> ValueTaskCode<'TOverall, 'T>

    /// Builds a step that executes the body while the condition predicate is true.
    member inline While:
        [<InlineIfLambda>] condition: (unit -> bool) * body: ValueTaskCode<'TOverall, unit> ->
            ValueTaskCode<'TOverall, unit>

    /// Wraps a step in a try/with. This catches exceptions both in the evaluation of the function
    /// to retrieve the step, and in the continuation of the step (if any).
    member inline TryWith:
        body: ValueTaskCode<'TOverall, 'T> * catch: (exn -> ValueTaskCode<'TOverall, 'T>) ->
            ValueTaskCode<'TOverall, 'T>

    /// Wraps a step in a try/finally. This catches exceptions both in the evaluation of the function
    /// to retrieve the step, and in the continuation of the step (if any).
    member inline TryFinally:
        body: ValueTaskCode<'TOverall, 'T> * [<InlineIfLambda>] compensation: (unit -> unit) ->
            ValueTaskCode<'TOverall, 'T>

    member inline For: sequence: 'T seq * body: ('T -> ValueTaskCode<'TOverall, unit>) -> ValueTaskCode<'TOverall, unit>

    member inline internal TryFinallyAsync:
        body: ValueTaskCode<'TOverall, 'T> * compensation: (unit -> ValueTask) -> ValueTaskCode<'TOverall, 'T>

    member inline Using:
        resource: 'Resource * body: ('Resource -> ValueTaskCode<'TOverall, 'T>) -> ValueTaskCode<'TOverall, 'T>
            when 'Resource :> IAsyncDisposable

    static member RunDynamic: code: ValueTaskCode<'T, 'T> -> ValueTask<'T>
    member inline Run: code: ValueTaskCode<'T, 'T> -> ValueTask<'T>
    member inline Run: code: ValueTaskCode<unit, unit> -> ValueTask

/// <exclude/>
[<AutoOpen>]
module ValueTaskBuilder =
    val vtask: ValueTaskBuilder

/// <exclude/>
[<AutoOpen>]
module LowPriority =
    type ValueTaskBuilder with
        [<NoEagerConstraintApplication>]
        static member inline BindDynamic:
            sm: byref<ResumableStateMachine<ValueValueTaskStateMachineData<'TOverall>>> *
            task: ^TaskLike *
            continuation: ('TResult1 -> ValueTaskCode<'TOverall, 'TResult2>) ->
                bool
                when ^TaskLike: (member GetAwaiter: unit -> ^Awaiter)
                and ^Awaiter :> ICriticalNotifyCompletion
                and ^Awaiter: (member get_IsCompleted: unit -> bool)
                and ^Awaiter: (member GetResult: unit -> 'TResult1)

        [<NoEagerConstraintApplication>]
        member inline Bind:
            task: ^TaskLike * continuation: ('TResult1 -> ValueTaskCode<'TOverall, 'TResult2>) ->
                ValueTaskCode<'TOverall, 'TResult2>
                when ^TaskLike: (member GetAwaiter: unit -> ^Awaiter)
                and ^Awaiter :> ICriticalNotifyCompletion
                and ^Awaiter: (member get_IsCompleted: unit -> bool)
                and ^Awaiter: (member GetResult: unit -> 'TResult1)

        member inline For:
            sequence: Collections.Generic.IAsyncEnumerable<'T> * body: ('T -> ValueTaskCode<'TOverall, unit>) ->
                ValueTaskCode<'TOverall, unit>

        [<NoEagerConstraintApplication>]
        member inline ReturnFrom:
            task: ^TaskLike -> ValueTaskCode<'T, 'T>
                when ^TaskLike: (member GetAwaiter: unit -> ^Awaiter)
                and ^Awaiter :> ICriticalNotifyCompletion
                and ^Awaiter: (member get_IsCompleted: unit -> bool)
                and ^Awaiter: (member GetResult: unit -> 'T)

        member inline Using:
            resource: 'Resource * body: ('Resource -> ValueTaskCode<'TOverall, 'T>) ->
                ResumableCode<ValueValueTaskStateMachineData<'TOverall>, 'T>
                when 'Resource :> IDisposable

/// <exclude/>
[<AutoOpen>]
module HighPriority =
    type ValueTaskBuilder with
        static member BindDynamic:
            sm: byref<ResumableStateMachine<ValueValueTaskStateMachineData<'TOverall>>> *
            task: Task<'TResult1> *
            continuation: ('TResult1 -> ValueTaskCode<'TOverall, 'TResult2>) ->
                bool

        member inline Bind:
            task: Task<'TResult1> * continuation: ('TResult1 -> ValueTaskCode<'TOverall, 'TResult2>) ->
                ValueTaskCode<'TOverall, 'TResult2>

        member inline ReturnFrom: task: Task<'T> -> ValueTaskCode<'T, 'T>

/// <exclude/>
[<AutoOpen>]
module MediumPriority =
    open HighPriority

    type ValueTaskBuilder with
        member inline While:
            [<InlineIfLambda>] condition: (unit -> Task<bool>) * body: ValueTaskCode<'TOverall, unit> ->
                ValueTaskCode<'TOverall, unit>

        member inline Bind:
            computation: Async<'TResult1> * continuation: ('TResult1 -> ValueTaskCode<'TOverall, 'TResult2>) ->
                ValueTaskCode<'TOverall, 'TResult2>

        member inline ReturnFrom: computation: Async<'T> -> ValueTaskCode<'T, 'T>
