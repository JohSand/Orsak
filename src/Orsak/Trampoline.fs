namespace Orsak

open System
open System.Threading
open System.Runtime.CompilerServices

/// <exclude/>
/// Thread-local trampoline that bounds stack growth in tail-recursive effects.
/// Implements ICriticalNotifyCompletion so state machines can register their
/// continuation via AwaitUnsafeOnCompleted, yielding to an iterative dispatch loop
/// rather than recursing. Stack depth is bounded to O(MaxDepth) regardless of n.
type Trampoline private () =

    static let holder = new ThreadLocal<Trampoline>(fun () -> Trampoline())

    let mutable depth = 0

    [<Literal>]
    let MaxDepth = 50

    let mutable pending: Action voption = ValueNone
    let mutable running = false

    let start () =
        try
            running <- true

            while pending.IsSome do
                let next = pending.Value
                pending <- ValueNone
                next.Invoke()
        finally
            running <- false

    let set (action: Action) =
        pending <- ValueSome action

        if not running then
            start ()

    interface ICriticalNotifyCompletion with
        member _.OnCompleted(continuation) = set continuation
        member _.UnsafeOnCompleted(continuation) = set continuation

    static member Current: Trampoline = holder.Value

    /// Returns true when the current synchronous call chain should yield to the
    /// trampoline's iterative loop. True when not inside the loop at all, or every
    /// MaxDepth calls while inside the loop (to unwind bounded stack segments).
    member _.ShouldBounce =
        not running
        || pending.IsNone
           && (depth <- depth + 1
               depth % MaxDepth = 0)
