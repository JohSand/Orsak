# Fable support: what it would take

The question: can Orsak's effects be used from F# compiled to JavaScript with [Fable](https://fable.io), and in what
form. Tried with Fable 5.27.0, a local tool of the repository (`dotnet tool restore`).

## Orsak's sources as they are (`AsIs/`)

`AsIs.fsproj` links Orsak's own core files (`Marker.fs` to `BaseEffects.fs`), unchanged:

```bash
dotnet fable AsIs --outDir AsIs/out --noCache
```

It fails. Every error comes from three things the core is built on:

* **Resumable code**: `__stateMachine`, `__useResumableCode`, `__resumeAt`, `ResumableCode`, `ResumptionDynamicInfo`.
  Fable can't translate F#'s resumable state machines, so neither the static nor the dynamic path of `eff` compiles.
* **`ValueTask`**: its constructor, `AsTask`, `ValueTaskAwaiter`, and the method builders
  (`AsyncValueTaskMethodBuilder`, `PoolingAsyncValueTaskMethodBuilder`).
* **Blocking on a task**: `GetAwaiter().GetResult()` compiles with warning FABLE0110, but returns the task itself in
  JavaScript, as nothing can block there.

So the core can't be shared with Fable as it is: `eff`, the `Effect` type's delegate returning `ValueTask`, and
everything built on them need a Fable implementation of their own.

## The building blocks (`Probe/`)

`Probe.fs` asks, one definition at a time, what such an implementation could be built on:

```bash
dotnet fable Probe --outDir Probe/out --noCache
```

| Building block | Fable |
|---|---|
| `task { }`, `Task<'T>`, `Task.FromResult`, `Task.Delay` | Compiles, to Fable's task builder over promises |
| `async { }`, `Async<'T>`, `Async.Sleep` | Compiles |
| `CancellationTokenSource`, `Cancel`, `IsCancellationRequested` | Compiles |
| A struct union holding a delegate from an interface environment to `Async<Result<_, _>>` | Compiles |
| A builder (no resumable code) with `Bind` overloads for effects, tasks and results | Compiles |
| `ValueTask<'T>` | **Error**: not supported |
| `:? IClock`, a type test against an interface | **Compiles to `false`, without a warning** |
| `IAsyncEnumerable<'T>.GetAsyncEnumerator` | Compiles to a plain, name-mangled interface call; no JavaScript async iteration |

`Run.fs` runs the probe on Node.js (`npm` isn't needed; `package.json` only makes the output ES modules):

```bash
dotnet fable Probe --outDir Probe/out --noCache
node Probe/out/Run.js
```

```text
workflow: Ok (Thu Nov 12 2026 00:00:00 GMT+0100 (Central European Standard Time))
task: 42
cancelled: true
isClock on a clock: false
```

The workflow binds an effect, a task and a result, and the task and the cancellation work. `ValueTask` is behind
`#if VALUETASK`, so the rest compiles; `--define VALUETASK` shows its error.

The type test is the trap: `isClock` compiles to `return false`, and returns it for a clock. Orsak tests the environment against interfaces at
run time, in `Time.delay` (`:? ICancellationProvider`) and `Cancellation.timeout` (`:? ITimeProvider`). On Fable these
would never find the cancellation source or the clock, silently. A Fable implementation must not rely on such tests.

## Choosing files for Fable (`Conditional/`)

Fable sets the MSBuild property `FABLE_COMPILER` when it reads a project, so one project file can compile different
files for .NET and Fable. `Lib.fsproj` includes `Which.Net.fs` when it isn't set, and `Which.Fable.fs` when it is:

```bash
dotnet run --project Conditional/App                                    # compiled by .NET
dotnet fable Conditional/App --outDir Conditional/out && node Conditional/out/App.js   # compiled by Fable
```

That holds for a project reference. Whether it also holds for a project in a package's `fable/` folder is for the
packaging step to check.

## What follows

A Fable implementation of the core, beside the .NET one:

* `Effect<'r, 'a, 'e>` as a delegate from the environment to `Task<Result<'a, 'e>>` (a JavaScript promise) instead of
  `ValueTask`, so the type and its functions keep their names and shapes.
* `eff` as an ordinary builder, without resumable code, with the same `Bind` overloads where Fable has the types.
* No type tests against interfaces: what is optional in the environment today is either required, or passed
  explicitly.

## Decisions

* **Scope:** the first version has `Effect`, `eff` and the `Effect` module. Base effects, resilience, `effSeq` and
  scopes may follow. Orsak.AspNetCore and Orsak.Myriad are .NET only.
* **Packaging:** inside the `Orsak` package: its Fable sources go in the package's `fable/` folder, where Fable looks
  for them, and are selected with `FABLE_COMPILER`. Users reference `Orsak` from both .NET and Fable projects.
* **Targets:** JavaScript only.
* **Testing:** tests compiled with Fable run on Node.js, locally and in CI.

## Plan

1. **Effect and eff.** `Effect<'r, 'a, 'e>` over `Task<Result<'a, 'e>>`, with `run`, `runOrFail` and `Effect.Create`,
   and `eff` with `Return`, `ReturnFrom`, `Bind` (effects, tasks, `Async`, results), `Zero`, `Combine`, `Delay`,
   `While`, `For` (sequences), `TryWith`, `TryFinally` and `Using`, without resumable code.
2. **The Effect module,** function by function, where it means the same in JavaScript. The ones built on .NET
   concurrency (`par`, `whenAll`, `parLimited`, `fanOut`, `race`, `timeout`) are looked at one by one: JavaScript is
   single-threaded, so some become concurrent rather than parallel, and some may be left out.
3. **Tests,** shared where they can be: the same test written once, run with xunit on .NET and compiled with Fable
   and run on Node.js.
4. **Packaging and CI:** the Fable sources in the package's `fable/` folder, and a CI job that compiles and runs the
   Fable tests.

## Step 1: Effect and eff

Done on `feature/fable`, in `src/Orsak/Fable/`: `EffectBuilder.fs` has `Effect`, `AsyncResult`, `EffectDelegate`,
`eff` and `mkEffect`, and `Effect.fs` has `Effect.run`, `Effect.runOrFail`, `Effect.Create` and `Effect.Error`.
`Orsak.fsproj` compiles them, with `Marker.fs`, instead of the .NET files when `FABLE_COMPILER` is set.

`tests/Orsak.Fable.Tests` tests them with a small runner of its own, as xunit can't run on Fable. The same tests run
on .NET, against the .NET implementation, and on Node.js, against the Fable one, and pass on both (23):

```bash
dotnet run --project tests/Orsak.Fable.Tests -c Release
dotnet fable tests/Orsak.Fable.Tests -o tests/Orsak.Fable.Tests/out && node tests/Orsak.Fable.Tests/out/Program.js
```

Not in Fable's `eff` yet:

* applicative `and!` (`Bind2` to `Bind7`, `MergeSources`), and the `Effect` type's operators (`Map`, `>>=`, `<*>`);
* `for` over `IAsyncEnumerable`, and over the struct tuples the .NET builder unrolls;
* `use` of an `IAsyncDisposable`, and `use!`;
* the stack-safe `return!` of the .NET builder's trampoline: on Fable, a `return!` in a recursive effect chains
  promises instead, which doesn't grow the stack, as a while loop of 100,000 iterations shows, but holds memory.

Found on the way:

* **A bug on .NET:** `eff { return! Task.FromResult 1 }` returns `Ok ()`, not `Ok 1`. In tail position, `return!` of a
  `Task<'T>` resolves to `ReturnFromFinal(task: Task)`, for tasks without a value, and drops the value. Binding it with
  `let!` first returns it. The shared tests leave tasks out of `return!` until it's fixed; the Fable builder returns
  the value.
* `Task.FromException` compiles with Fable, to an import that Fable's library doesn't have, and fails when the module
  loads on Node.js. The tests make a faulted task with `task { }` instead.
