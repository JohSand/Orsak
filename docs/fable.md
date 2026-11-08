---
title: Fable
category: Guides
categoryindex: 3
index: 9
---

Fable
=====

Effects can be used in F# compiled to JavaScript with [Fable](https://fable.io): `Effect`, `eff` and the `Effect`
module work in Fable 5, for JavaScript. Support is experimental: what's there behaves as on .NET, and is tested on both,
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

## What is .NET only

* `Effect.fanOut`, effect sequences (`effSeq`, `EffSeq`) and `Effect.Create` of a `ValueTask`;
* `for` over an `IAsyncEnumerable`, and `use` of an `IAsyncDisposable`;
* the base effects (`Time`, `Log`, `CancellationSource`, ...), `Orsak.Resilience`, scopes and `CancellationScope`;
* Orsak.AspNetCore, Orsak.AspNetCore.OpenApi and Orsak.Myriad.

Some may follow; [Supported frameworks](supported-frameworks.html) lists what each package targets.
