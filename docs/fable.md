---
title: Fable
category: Guides
categoryindex: 3
index: 9
---

Fable
=====

Effects can be used in F# compiled to JavaScript with [Fable](https://fable.io): `Effect`, `eff`, the `Effect`
module and most base effects work in Fable 5, for JavaScript. Support is experimental: what's there behaves as on .NET, and is tested on both,
but it covers the core of Orsak, not all of it.

## Setup

Reference the `Orsak` package, as on .NET:

```xml
<PackageReference Include="Orsak" Version="..." />
```

The package carries the sources Fable compiles, and Fable chooses them itself. An effect written once, in a project
shared by a .NET server and a Fable client, compiles on both:

```fsharp
type IGreeter =
    abstract Greet: string -> string

let greet name = Effect.Create(fun (g: #IGreeter) -> g.Greet name)

let greetAll names = eff {
    let! greetings = names |> List.map greet |> Effect.par
    return greetings
}
```

```fsharp
// in the Fable client
task {
    match! greetAll [ "Fable"; "Node" ] |> Effect.run (Greeter()) with
    | Ok greetings -> printfn "%A" greetings
    | Error e -> printfn "failed: %s" e
}
|> ignore
```

## What differs from .NET

* **Effects run on `Task`**, which Fable compiles to a JavaScript promise, rather than `ValueTask`, which Fable doesn't
  have: `Effect.run` returns a `Task<Result<'a, 'e>>`. Code that uses `AsyncResult<'a, 'e>`, the type of an effect's
  result, compiles on both.
* **`eff` is an ordinary computation expression,** rather than a compiled state machine, as Fable can't translate
  F#'s resumable code. It binds effects, tasks, `Async` and `Result`, and has `if`, `while`, `for` over sequences,
  `try`, `use` and `and!`.
* **Concurrent, not parallel.** JavaScript runs one thing at a time, so `Effect.par`, `whenAll`, `parLimited` and
  `race` run their effects concurrently, interleaved where they wait, but never in parallel. What they return is the
  same as on .NET.
* **No type tests against interfaces.** In JavaScript, `:? IClock` compiles to `false`, without a warning, so an
  effect that checks whether its environment implements an interface never finds it. Require the interface instead,
  as `Effect.Create(fun (clock: #IClock) -> ...)` does.

## Base effects

`Time`, `GuidGenerator`, `Random` and `CancellationSource`, with their providers, work as on .NET, but for the clock:
Fable has no `System.TimeProvider`, so on Fable an `ITimeProvider`'s `Clock` is Orsak's own `TimeProvider`. It has
`TimeProvider.System` and `GetUtcNow`, as .NET's does, so a clock written with them compiles on both, as long as
`open Orsak` comes after `open System`:

```fsharp
open System
open Orsak

type ClientEnv() =
    interface ITimeProvider with
        member _.Clock = TimeProvider.System

// a fixed clock, for tests
let fixedClock now =
    { new TimeProvider() with
        member _.GetUtcNow() = now
    }
```

A fake clock on Fable decides when `Time.delay` ends by overriding `Delay`, which .NET's `TimeProvider` doesn't have.
And `Time.delay` isn't ended by the environment's `ICancellationProvider` on Fable: on .NET it finds that with a type
test, which JavaScript doesn't have, as above.

## Generated effects

The effects `[<GenEffects>]` generates, and the proxies `[<GenRemote>]` generates, compile with Fable. Orsak.Myriad
runs when .NET builds the project, so the code must be generated into the shared project's files, with
`MyriadInlineGeneration`, or into a file Fable compiles too, and the project references the Orsak.Myriad package for its
attributes, which Fable compiles to nothing. See [Generating code](myriad.html).

## What is .NET only

* `Effect.fanOut`, effect sequences (`effSeq`, `EffSeq`) and `Effect.Create` of a `ValueTask`;
* `for` over an `IAsyncEnumerable`, and `use` of an `IAsyncDisposable`;
* `Log` and `MemoryCache`, built on Microsoft.Extensions, `Orsak.Resilience`, scopes and `CancellationScope`;
* Orsak.AspNetCore and Orsak.AspNetCore.OpenApi, and running Orsak.Myriad.

Some may follow; [Supported frameworks](supported-frameworks.html) lists what each package targets.
