# Trampoline Research: IcedTasks Branch Analysis

## Source

Repository: https://github.com/majocha/IcedTasks/tree/trampoline  
Key commit: `39cd1ca` ("update", 2025-12-01)  
Primary files changed: `Trampoline.fs`, `ColdTask.fs`, `CancellableTask.fs`,
`CancellableValueTask.fs`, `CancellablePoolingValueTask.fs`, `CancellableTaskBuilderBase.fs`

---

## The Core Data Structure

`Trampoline.fs` defines a thread-local singleton:

```fsharp
type Trampoline private () =
    static let holder = new ThreadLocal<_>(fun () -> Trampoline())

    let mutable depth = 0
    [<Literal>] let MaxDepth = 50
    let mutable pending: Action voption = ValueNone
    let mutable running = false
    let mutable primed = true
```

The trampoline implements `ICriticalNotifyCompletion`, meaning it can be used as a
fake awaiter that the `MethodBuilder` can register continuations against.

### Key methods

| Member | Behaviour |
|---|---|
| `ShouldBounce` | Returns `true` when **not currently running**, OR when the call depth hits a multiple of 50 |
| `set action` | Stores the continuation; if the trampoline is not running, starts its loop |
| `start()` | Iterative loop — drains `pending` without recursing |
| `Prime()` / `IsAwaited()` | One-shot flag used by cold-task `Source` wrappers to signal "this computation is being let!-bound, trampoline use is safe" |

---

## How It Solves Stack Overflow — Not via `ReturnFromFinal`

IcedTasks does **not** use `ReturnFromFinal`. The stack-overflow problem is solved at
two call sites in every generated `MoveNext`:

### 1. `yieldOnBindLimit` — inserted at the entry of `MoveNext`

```fsharp
let inline yieldOnBindLimit () =
    ColdTaskCode(fun sm ->
        if Trampoline.Current.ShouldBounce then
            let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
            if not __stack_yield_fin then
                sm.Data.MethodBuilder.AwaitOnCompleted(Trampoline.Current.Ref, &sm)
            __stack_yield_fin
        else
            true
    )
```

When `ShouldBounce` is true the state machine yields itself to the trampoline
(`AwaitOnCompleted` with the trampoline as the "awaiter"). The trampoline's
`OnCompleted` stores the continuation and its iterative loop dispatches it, so the
original call stack is fully unwound before the continuation runs.

### 2. Static `Run` — `noBounce` guard around both `SetResult` and the main code block

```fsharp
// In ColdTask.Run (static __stateMachine path):
let noBounce = not (Trampoline.Current.IsAwaited())

let __stack_go1 = noBounce || yieldOnBindLimit().Invoke(&sm)

if __stack_go1 then
    let __stack_code_fin = code.Invoke(&sm)
    if __stack_code_fin then
        let __stack_go2 = noBounce || yieldOnBindLimit().Invoke(&sm)
        if __stack_go2 then
            sm.Data.MethodBuilder.SetResult(sm.Data.Result)
```

The `noBounce` short-circuit means: if the cold task is being started at the
top level (not awaited by another computation), skip the trampoline entirely to
avoid introducing allocations where none are needed.

### 3. Dynamic `RunDynamic` — `Bounce` state in the `DynamicState` DU

```fsharp
type DynamicState =
    | Running | SetResult | SetException of ExceptionDispatchInfo
    | Awaiting of ICriticalNotifyCompletion
    | Bounce of DynamicState      // ← new state
    | Immediate of DynamicState   // ← new state
```

`MoveNext` can now return a `Bounce` state. When seen, it registers with the
trampoline via `AwaitOnCompleted` instead of calling itself recursively. `Immediate`
means "re-enter synchronously without yielding" — used when the task is not being
awaited by a trampoline-aware consumer.

---

## The `Prime`/`IsAwaited` Protocol — Preventing Deadlocks

A subtle but critical detail: the trampoline must not be used when a cold task is
started **outside** of a `let!` bind (e.g., directly at a test entry point), because:

1. A continuation is posted to the trampoline.
2. The trampoline is not currently "started" by anyone.
3. The task never completes.

IcedTasks solves this with a one-shot boolean `primed`:

- `Prime()` sets `primed = true` (called by `Trampoline.Allow`, which wraps each
  `Source` method — the CE method called on the right-hand side of a `let!`).
- `IsAwaited()` reads and resets `primed` — returns `true` only if `Prime()` was
  called since the last check.
- `bounceAllowed = Trampoline.Current.IsAwaited()` is captured at `RunDynamic` time.
  If the cold task is not being awaited, `bounceAllowed = false`, so `maybeBounce`
  always returns `Immediate`, and no trampoline posting occurs.

---

## `ReturnFromFinal` — Explicitly Deferred

The comment at `Trampoline.fs:84` is a direct acknowledgement:

```fsharp
// TODO: similar mechanism can be used to communicate that the task is bound as a
// tail-call (ReturnFromFinal).
```

This means:
- `ReturnFromFinal` was considered as an additional optimisation, not as the primary
  mechanism.
- The stack-overflow prevention comes from the **periodic yield at every Bind**
  (every 50 synchronous calls), not from tail-call detection.
- `ReturnFromFinal` could potentially let the trampoline skip the `MaxDepth` counter
  entirely for the last bind in a chain — a minor additional optimisation on top.

---

## Implications for Orsak

### What to adopt

| Component | What it maps to in Orsak |
|---|---|
| `Trampoline` type (thread-local, `ICriticalNotifyCompletion`, iterative loop) | New file, identical concept |
| `yieldOnBindLimit` | A new helper inside `EffBuilderBase`, called at the start of every Bind invocation |
| `noBounce` guard in `Run` | Added to `EffBuilder.Run` and `EffBuilder.RunDynamic` — skip trampoline when the Effect is started at the composition root, not inside another `eff {}` |
| `Prime`/`IsAwaited` | Required to prevent deadlock; called by a `Source`-style wrapper when an `Effect` is `let!`-bound inside `eff {}` |
| `Bounce`/`Immediate` DU states | Added to the dynamic `ResumptionData` — replaces the current plain `ICriticalNotifyCompletion` object |

### What to leave out (for now)

- `ReturnFromFinal` — still a useful minor optimisation (avoids the `Ok`-rewrap) but
  not the mechanism that prevents stack overflows. Can be added later independently.
- `MaxDepth` tuning — 50 may not be the right value for Orsak; should be benchmarked
  once the trampoline is in place.

### Key difference from our initial investigation

Our original hypothesis was that `ReturnFromFinal` with a trampoline could eliminate
the O(n) stack growth by intercepting the tail call before `eff.Run` is called. The
IcedTasks work shows this is **not the right level to intercept**. The stack grows
because each synchronous `Bind` calls `eff.Run` → `Start` → `MoveNext` on the current
thread. The fix is to yield the *current state machine* to an iterative loop before
that recursion can build up — i.e., at the `Bind`/`MoveNext` level, not at the
`ReturnFrom` level.

The `TailCallTests.fs` test we wrote (`tail recursive effect should not grow the call
stack`) will pass once the `yieldOnBindLimit` mechanism is in place, because the stack
is unwound every 50 synchronous binds regardless of whether `ReturnFromFinal` exists.
