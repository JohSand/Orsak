# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Project Is

Orsak is an F# library for functional effects inspired by ZIO. It implements pragmatic, composable modeling of side-effects through interfaces using the type `Effect<'r, 'a, 'e>` — a lazy computation requiring environment `'r`, returning success `'a` or error `'e`.

## Build & Test Commands

```bash
dotnet tool restore
dotnet restore
dotnet build --configuration Release
dotnet test --no-build -c Release

# Run a single test project
dotnet test --project tests/Orsak.Tests/Orsak.Tests.fsproj --no-build -c Release

# Run tests by name (names may contain spaces; * only at the start or end)
dotnet test --project tests/Orsak.Tests/Orsak.Tests.fsproj --no-build -c Release --filter-method "*TestName*"

# Pack NuGet packages
dotnet pack --configuration Release --output ./nupkg
```

How a change is made is in "Working on a change" in `CONTRIBUTING.md`: branch names, what a pull request includes (tests, XML docs, docs pages, a changelog line), a feature's stages (spike in `experiments/`, internal prototype, `[<Experimental>]`, stable, `[<Obsolete>]`), breaking changes (package validation against the latest release, `CompatibilitySuppressions.xml`), and framework-specific code. Follow it.

Releases are made with `scripts/release.sh <version>`, from `CHANGELOG.md`; see "Releasing" in `CONTRIBUTING.md`. Record user-visible changes under `## [Unreleased]` there. Pushes to `main` publish prereleases to GitHub Packages, and only tags publish to nuget.org.

`dotnet test` runs on Microsoft.Testing.Platform (set in `global.json`), so options go to the test runner directly: `--filter-method`, `--filter-class`, `--filter-query`, not VSTest's `--filter`, which is rejected.

The test suite uses **Verify.Xunit** for snapshot/approval testing. When snapshot output changes intentionally, update `.verified.txt` files alongside tests.

## Architecture

Prefer a functional style, with short pure functions where possible.

Use interfaces primarily to describe side-effects.

### Core Types (`src/Orsak/`)

- **`Effect<'r, 'a, 'e>`** — struct wrapper around `EffectDelegate<'r, 'a, 'e>`, a delegate returning `ValueTask<Result<'a, 'e>>`. Lazy — nothing runs until `run`/`runOrFail` is called with an environment.
- **`eff { }` computation expression** (`EffectBuilder.fs`) — resumable state machine (F# resumable code) for zero-allocation synchronous paths. Supports bind on `Task`, `Async`, `Result`, `ValueTask`, and nested `Effect`.
- **`ScopedEffect`** (`ScopedEffect.fs`) — `AsyncDisposable`-aware scopes for resource management (e.g., DB transactions, progress tracking). An environment implementing `IGrowsInto<'Scope, 'Inner>`, which inherits `ScopeProvider<'Scope>`, begins the scope and grows into the environment of the effects in a scope, which reach the scope through an ordinary provider constraint. `Scope.Within`/`CompletingScope.Within` and the builders `ScopeCreatingEffectBuilder`/`CompletableScopeCreatingEffectBuilder` open the scope and return an effect of the outer environment.
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

`Orsak` and `Orsak.AspNetCore` target `net8.0` and `net10.0`, and tests run on both. `Orsak.AspNetCore.OpenApi` targets only `net10.0`, as Microsoft.AspNetCore.OpenApi 10 does, and its tests run there. The lowest target is the oldest supported LTS, kept for about six months after it goes out of support. A higher target is added only when it unlocks an API or speedup used behind `#if`. Dropping a target goes in a major version.

`Orsak` also compiles with Fable, for JavaScript (experimental): `Effect` and `eff` from `src/Orsak/Fable/`, and the `Effect` module from the shared `Effect.fs`, whose .NET-only parts are behind `#if !FABLE_COMPILER`. When changing `Effect.fs`, `Fable/` or the `Effect` operators, run the shared tests on both, as in "Fable" in `CONTRIBUTING.md`.

`Orsak.Remoting` (client and pluggable wire format, `IWireFormat`, with `JsonFormat`; same sources on .NET and Fable) and `Orsak.Remoting.AspNetCore` (`MapRemote`) let a client call an interface's members on a server. `[<GenRemote>]` (the attribute is in Orsak.Remoting, the generator is `RemoteGen` in Orsak.Myriad, matching it by name) generates the client's proxy. `experiments/Remoting/README.md` has the design and why it isn't built on Fable.Remoting. Its JSON must be the same text on .NET and Fable: `tests/Orsak.Fable.Tests/RemotingTests.fs` checks that, and `tests/Orsak.Remoting.EndToEnd/run.sh` runs a server with both clients.

## Key Conventions

- All async work uses `ValueTask` (not `Task`) for allocation efficiency — preserve this when adding new features.
- Errors are modeled as the `'e` type parameter (discriminated unions), not exceptions. Exceptions bubble only for truly unexpected failures.
- The `IProvide<'t>` marker interface is used as a dedicated marker — don't conflate it with ad-hoc provider interfaces.
- Snapshot test files (`*.verified.txt`) live alongside test files; commit them with intentional changes.
- Name tests as sentences with double backticks and spaces, e.g. ``` ``fanOut completes for an empty source`` ```, keeping function names as written (``` ``addDelay_ adds delay on false`` ```). Filters match names with spaces.
