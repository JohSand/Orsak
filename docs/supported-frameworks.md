---
title: Supported frameworks
category: About
categoryindex: 4
index: 1
---

Supported frameworks
====================

## Packages

| Package | Targets | Requires |
|---|---|---|
| Orsak | `net8.0`, `net10.0` | FSharp.Core 8.0.300 or later |
| Orsak.AspNetCore | `net8.0`, `net10.0` | FSharp.Core 8.0.300 or later |
| Orsak.AspNetCore.OpenApi | `net10.0` | Microsoft.AspNetCore.OpenApi 10 |
| Orsak.Myriad | `net8.0` | FSharp.Core 9, Myriad 1.1 (see [Generating code](myriad.html)) |
| Orsak.Remoting | `net8.0`, `net10.0`, and Fable (JavaScript) | FSharp.Core 8.0.300 or later, Fable.Core 5 |
| Orsak.Remoting.AspNetCore | `net8.0`, `net10.0` | FSharp.Core 8.0.300 or later |

Orsak.Myriad runs at build time, in Myriad, so its target is the runtime the build needs, not the one the application
runs on. Orsak.AspNetCore.OpenApi targets only .NET 10, as Microsoft.AspNetCore.OpenApi 10 does.

The whole API is available on every .NET target. The tests run on .NET 8 and .NET 10.

`Orsak` also works with [Fable](fable.html), for JavaScript, which is experimental: the package carries the sources
Fable compiles, which cover `Effect`, `eff` and the `Effect` module, tested on Node.js.

## Policy

* **The lowest target is the oldest .NET LTS release still in support.** It is kept for about six months after it goes
  out of support, as many applications move to a newer runtime some months later.
* **A higher target is added when it gives Orsak an API or a speedup it uses**, not for every .NET release. A newer
  runtime runs the highest target there is, so a release that isn't targeted, such as an STS release, is still
  supported, and the tests run on it.
* **Dropping a target is a breaking change.** It comes in a major version, or before 1.0 a minor version, and the
  changelog says which release was the last for that framework.
* Targets are reviewed each November, when a new .NET release comes out.

## History

| Framework | Targeted by | Last release |
|---|---|---|
| .NET 6 | 0.1.0 to 0.7.0 | 0.7.x |
| .NET 8 | 0.5.0 onwards | The lowest target. .NET 8 goes out of support on November 10, 2026, and the target is expected to be dropped about six months later. |
| .NET 10 | 0.8.0 onwards | |

Orsak.Myriad targeted `netstandard2.1` in its first release, 0.6.0, and `net8.0` from 0.7.0.

See the [changelog](https://github.com/JohSand/Orsak/blob/main/CHANGELOG.md) for each release.
