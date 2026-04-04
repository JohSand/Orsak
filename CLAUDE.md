# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Project Is

Orsak is an F# library for functional effects inspired by ZIO. It implements pragmatic, composable modeling of side-effects through interfaces using the type `Effect<'r, 'a, 'e>` — a lazy computation requiring environment `'r`, returning success `'a` or error `'e`.

## Build & Test Commands

```bash
dotnet tool restore
dotnet restore
dotnet build --configuration Release
dotnet test --no-build -c release

# Run a single test project
dotnet test tests/Orsak.Tests/Orsak.Tests.fsproj --no-build -c release

# Run a single test by name filter
dotnet test tests/Orsak.Tests/Orsak.Tests.fsproj --no-build -c release --filter "FullyQualifiedName~TestName"

# Pack NuGet packages
dotnet pack --configuration Release --output ./nupkg
```

The test suite uses **Verify.Xunit** for snapshot/approval testing. When snapshot output changes intentionally, update `.verified.txt` files alongside tests.

## Architecture

### Core Types (`src/Orsak/`)

- **`Effect<'r, 'a, 'e>`** — struct wrapper around `EffectDelegate<'r, 'a, 'e>`, a delegate returning `ValueTask<Result<'a, 'e>>`. Lazy — nothing runs until `run`/`runOrFail` is called with an environment.
- **`eff { }` computation expression** (`EffectBuilder.fs`) — resumable state machine (F# resumable code) for zero-allocation synchronous paths. Supports bind on `Task`, `Async`, `Result`, `ValueTask`, and nested `Effect`.
- **`ScopedEffect`** (`ScopedEffect.fs`) — `AsyncDisposable`-aware scopes for resource management (e.g., DB transactions, progress tracking). Two builders: `ScopeCreatingEffectBuilder` (creates scope) and `ScopeAwareEffectBuilder` (binds within existing scope).
- **`EffectSeq`** (`EffSeqBuilder.fs`) — streaming effects as async enumerables.
- **`BaseEffects.fs`** — pre-built effect interfaces: `IGuidGenerator`, `ITimeProvider`, `IRandomProvider`, `ICacheProvider`, `ICancellationProvider`.

### Environment / Dependency Injection Pattern

Effects express their dependencies via the `'r` type parameter. Multiple constraints compose structurally:

```fsharp
type IButtonPusherProvider =
    abstract member ButtonPusher: IButtonPusher

let pushButton () =
    Effect.Create(fun (provider: #IButtonPusherProvider) -> provider.ButtonPusher.PushButton())

let workflow () = eff {
    do! pushButton ()        // 'r must satisfy IButtonPusherProvider
    let! beans = countBeans() // 'r must also satisfy IBeanCounterProvider
    return beans * 2
}
// Inferred: workflow: unit -> Effect<#IButtonPusherProvider & #IBeanCounterProvider, int, 'e>
```

The real environment type (usually passed at the composition root) implements all required interfaces.

### Myriad Code Generation (`src/Orsak.Myriad/`)

A Myriad plugin that generates effect boilerplate from interface definitions. Annotate an interface with `[<Generator.Effects>]` and the generator emits the `Effect.Create(...)` wrappers automatically. The `Orsak.Myriad.Showcase` sample project demonstrates generated output.

### ASP.NET Core Integration (`src/Orsak.AspNetCore/`)

- **`EndpointRouting.fs`** — maps effects to HTTP endpoints; uses `FastExpressionCompiler` for route parameter parsing.
- **`BackgroundWorker.fs`** — long-running background tasks as effects.

### Multi-targeting

The main library targets `net6.0`, `net8.0`, and `net10.0`. Some APIs (e.g., `ITimeProvider`) are conditional on `net8.0` or later. Tests run on `net8.0` and `net10.0`.

## Key Conventions

- All async work uses `ValueTask` (not `Task`) for allocation efficiency — preserve this when adding new features.
- Errors are modeled as the `'e` type parameter (discriminated unions), not exceptions. Exceptions bubble only for truly unexpected failures.
- The `IProvide<'t>` marker interface is used as a dedicated marker — don't conflate it with ad-hoc provider interfaces.
- Snapshot test files (`*.verified.txt`) live alongside test files; commit them with intentional changes.
