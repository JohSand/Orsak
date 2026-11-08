# Fable support: what it would take

The question: can Orsak's effects be used from F# compiled to JavaScript with [Fable](https://fable.io), and in what
form. Tried with Fable 5.27.0, installed as a local tool in this folder (`dotnet tool restore`).

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

The type test is the trap: `isClock` compiles to `return false`. Orsak tests the environment against interfaces at
run time, in `Time.delay` (`:? ICancellationProvider`) and `Cancellation.timeout` (`:? ITimeProvider`). On Fable these
would never find the cancellation source or the clock, silently. A Fable implementation must not rely on such tests.

## What follows

A Fable implementation of the core, beside the .NET one:

* `Effect<'r, 'a, 'e>` as a delegate from the environment to `Task<Result<'a, 'e>>` (a JavaScript promise) instead of
  `ValueTask`, so the type and its functions keep their names and shapes.
* `eff` as an ordinary builder, without resumable code, with the same `Bind` overloads where Fable has the types.
* No type tests against interfaces: what is optional in the environment today is either required, or passed
  explicitly.

Open questions, for the design:

* Scope: which of the core (`Effect`, `eff`, the `Effect` module, base effects, resilience, `effSeq`, scopes) is
  needed in Fable. Orsak.AspNetCore and Orsak.Myriad are .NET only.
* Packaging: the Fable sources in the `Orsak` package itself (in its `fable/` folder, selected with
  `FABLE_COMPILER`), or a package of their own.
* Targets: JavaScript only, or also Fable's other targets (TypeScript, Python, Rust, Dart, BEAM).
* Testing: running the compiled tests needs Node.js, which this machine doesn't have yet.
