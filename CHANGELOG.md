# Changelog

## [Unreleased]

### Added
Fable support for JavaScript: `Effect`, `eff` and the `Effect` module, from the `Orsak` package, which carries the sources Fable compiles. Effects run on `Task`, which Fable compiles to promises, and `fanOut`, effect sequences, the base effects, resilience and scopes are .NET only, see the Fable guide (experimental)
Orsak.Remoting and Orsak.Remoting.AspNetCore: a client, compiled with Fable or on .NET, calls the members of an interface on an ASP.NET Core server, which serves them with `MapRemote`. The wire format is pluggable, `IWireFormat`, and JSON is the first, the same on .NET and Fable; the contract's errors cross as values, and a failed call raises `RemoteCallException`, see the Remoting guide (experimental)
Orsak.Myriad: `[<GenRemote>]` generates the proxy of a remote interface, which makes the codecs of each member once, so a type the format can't write fails when the proxy is made, as `MapRemote` does when it maps the endpoints (experimental)
Orsak.Remoting: a format of a self-describing encoding is `WireFormat(encoding)`, Orsak.Remoting's mapping of F# types to `WireValue`s with an `IWireEncoding` of them to bytes, a few dozen lines over a library such as MessagePack-CSharp or System.Text.Json; `Conformance` checks an encoding, and that a client's and a server's encodings agree. `JsonFormat` is the JSON encoding's, with the content type `application/vnd.orsak+json`, writes byte arrays as base64, and reads non-public types (experimental)
Orsak.Remoting: `RemoteCallException.StatusCode` is the status the server answered, such as 401 or 403, or `None` if it couldn't be reached; `FetchTransport` takes headers, asked for on each request, such as a bearer token, and fetch's credentials mode. Orsak.Remoting.AspNetCore: a member's attributes, such as `[<Authorize(Roles = "editor")>]`, are its endpoint's metadata, and arguments that can't be read are answered 400, not 500. The Remoting guide has a section on authorization (experimental)
Orsak.Remoting: enums, by name, the smaller integer types, `uint32` and `uint64`, maps, sets, `ResizeArray`, `Dictionary`, `DateTime`, `DateTimeOffset`, `DateOnly`, `TimeOnly` and `TimeSpan` cross the wire, the dates and times as ISO 8601 strings written the same on .NET and Fable, and an option of an option keeps `Some None` apart from `None` (experimental)

### Fixed
`return!` of a `Task<'T>` in an `eff` returns the task's value, instead of `unit`, and can be used in a `try` block too
The `Effect` type's `<*>` applies an effect of a function to an effect of its argument, for FSharpPlus too, as up to 0.5.0. From 0.6.0 it expected the function to return the type it was applied to, so it couldn't be used

## [0.8.0] - 2026-11-08

### Changed
Orsak and Orsak.AspNetCore target net8.0 and net10.0. net6.0 is no longer supported, so ITimeProvider, Time, CancellationSource, CancellationScope, Orsak.Resilience and the worker count metric are available on every target. Microsoft.Extensions.*.Abstractions dependencies now start at 8.0.0. 0.7.x is the last release supporting net6.0

### Added
CancellationProvider: `fromSource` provides a source the caller owns, and `create` a new one, disposed with the provider
Time.delay, which waits on the environment's clock, and ends early when the environment's cancellation source is cancelled
Log: effects that log message templates with the environment's `ILogger`, e.g. `Log.Information("Imported {Count} rows", count)`
CancellationScope: a `CancellationTokenSource` linked to a token, begun and disposed around an effect, so each source lives exactly as long as the effects using it. `Cancellation.within` runs an effect in one, nested in the current scope if there is one, and `Cancellation.timeout` cancels it after a duration, stopping the effect, unlike `Effect.timeout`. `cancellableEff` is a computation expression whose effects run in one
Orsak.Myriad: an environment may grow into a `CancellationScope` by adding an `ICancellationProvider`, and an environment providing an `ICancellationProvider` begins its scopes nested in its source, with `CancellationScope.NestedIn`, so a scope begun in a scope is cancelled with it
Orsak.AspNetCore: endpoints carry `EffectType` metadata, the type of the effect their handler creates, also for a handler that is a lambda
Orsak.AspNetCore: a runner implementing `IDescribeResponses` describes the responses of each endpoint from the type of its effect, as `ResponseDescription` metadata, which is an `IProducesResponseTypeMetadata` for ApiExplorer and OpenAPI generators
Orsak.AspNetCore.OpenApi (.NET 10): `options.AddEffectEndpoints()` adds the endpoints mapped with `MapEffectEndpoints` to Microsoft.AspNetCore.OpenApi's documents, described from their routes, handlers, XML docs and metadata, explicit metadata first
Orsak.AspNetCore.OpenApi: `options.AddFSharpSchemas()` describes options, value options, lists, sets and maps as System.Text.Json writes them: an option is its value or null and not required, and a list, set or map is never null

### Fixed
Effects written in a quotation, such as a route handler lambda or a scaling job's work, can use `let!`, `return!`, `for`, `while` and `Effect.Create`: each overload of the builders has a compiled name of its own, so deserializing the quotation can bind it

### Removed
Orsak.AspNetCore: `scalingJob2`, a prototype, is internal again

## [0.7.0] - 2026-10-25

### Changed
Orsak.Myriad updated to Myriad 1.1.0, and now targets net8.0 (previously netstandard2.1) and requires FSharp.Core 9. Consumers must add `<PackageDownload Include="Myriad" Version="[1.1.0]" />`, see README
Scopes are tracked with ordinary constraints, and are no longer experimental: an environment implementing `IGrowsInto<'Scope, 'Inner>` begins the scope and grows into the environment of the effects in it
A completing scope lets exceptions propagate once it is disposed, instead of requiring an `ExceptionHandler`

### Added
Scopes whose effects run in an environment grown from the outer one: `Scope.Within`, `CompletingScope.Within`, `ScopeCreatingEffectBuilder` and `CompletableScopeCreatingEffectBuilder`
Effect.withEnvironment, to run an effect with an environment derived from the current one
Effect.parLimited and Effect.mapParLimited, running at most a given number of effects at a time
Orsak.AspNetCore: ScalingEffectJob, a background service that scales its workers with its workload, and `scalingJob`, a computation expression for defining one
Orsak.AspNetCore: curried route builders checked at compile time (Orsak.AspNetCore.V2)
Orsak.Myriad: `[<GenEnvironment>]` generates a `create` function for an interface that inherits provider interfaces, and `run` and `runOrFail` functions that run an effect in the created environment, see README
Orsak.Myriad: a `[<GenEnvironment>]` may inherit another from the same file, generates `IGrowsInto` when it declares the environment it grows into, and an `Extend` extension member
Orsak.Myriad: with MyriadInlineGeneration, code is generated to be appended to the file itself, e.g. an ad hoc environment in a `rec` test file. Detected automatically from Myriad 1.1; with Myriad 1.0, set `Inline = true` on `[<GenEffects>]` and `[<GenEnvironment>]`, see README
API documentation for the public API, and Source Link that resolves for every file in the packages

### Fixed
Effect.forever and Effect.retryWhile (and so Effect.retryTimes) no longer overflow the stack when the effect fails synchronously many times in a row: they are loops now, instead of nesting a run per failure
Effect.timeout fails with the error, instead of throwing a TimeoutException
fanOut completes once its source is enumerated, and stops everything when a worker fails, instead of hanging
Orsak.Myriad `[<GenEffects>]`: a configured `ProviderName` now names the generated provider, not just the constraint on the effect functions
Orsak.Myriad `[<GenEffects>]`: module names only trim one leading `I` (`IInventory` generates `Inventory`, not `nventory`)
Orsak.Myriad `[<GenEffects>]`: an input declared as `module A.B` generates into `namespace A`, instead of a `namespace A.B` that clashes with the module

## [0.6.0] - 2026-08-15

### Changed
`IProvide<'t>` is a dedicated marker interface

### Added
Orsak.Myriad: `[<GenEffects>]` generates effect functions and their provider from an interface
An effect for the environment's `ILoggerFactory`
Tail-recursive effects no longer overflow the stack, with a trampoline and `ReturnFromFinal`

### Fixed
The dynamic (not statically compiled) builder path under SDK 10.0.400, and for loops on it
A suspended effect no longer tries to propagate cancellation

## [0.5.0] - 2025-04-16

### Changed
Targets net8.0 and net6.0 (previously net6.0)
Reworked retry and resilience API, in Orsak.Resilience
Improved scoped effect API

### Added
Orsak.AspNetCore: mapping effects to endpoints, and background workers running effects
Common effects: IGuidGenerator, ITimeProvider, IRandomProvider, ICacheProvider and ICancellationProvider
ReturnFrom, and binding functions returning effects
`IProvide<'t>`, a base interface for providers
Reproducible builds

## [0.4.2] - 2023-11-01

### Changed
effSeq is truly lazy

### Added
effSeq: `yield!` of effects, WithCancellation, async disposal, and ConfiguredCancelableAsyncEnumerable

### Fixed
A race condition when setting the result
Yielding after binding results in effSeq

## [0.4.1] - 2023-10-05

### Fixed
Too large allocation in WhenAll
A for loop over an IAsyncEnumerable no longer makes the whole expression return unit

## [0.4.0] - 2023-09-19

### Changed
More performant implementation of Effect.par

### Added
Effect.whenAll, high performance alternative to Effect.par that returns an array
Applicative binds (`and!`) of 6 and 7 effects

### Fixed
Issue where bound effects could be not rerun properly
Binding Async in eff
Effect.traverse
Binding Result now warns, instead of failing, with 8 or more binds

## [0.3.0] - 2023-04-15
Extend api

### Added
Add function for creating effect from result
Add function for running effect and continuing with new effects

### Changed
Rename sequence builder to more honest name

## [0.2.0] - 2023-04-06
Cleanup

### Added
Added support for using unit -> ValueTask<bool> and unit -> Effect<_,bool,_> in while loops

### Changed
Cleaning up the stuff left in extensions with proper implementations


## [0.1.0] - 2023-03-27
First release

### Added
- Effect type and its builder
- Effect module with functions to manipulate effects

[Unreleased]: https://github.com/JohSand/Orsak/compare/0.8.0...HEAD
[0.8.0]: https://github.com/JohSand/Orsak/compare/0.7.0...0.8.0
[0.7.0]: https://github.com/JohSand/Orsak/compare/0.6.0...0.7.0
[0.6.0]: https://github.com/JohSand/Orsak/compare/0.5.0...0.6.0
[0.5.0]: https://github.com/JohSand/Orsak/compare/0.4.2...0.5.0
[0.4.2]: https://github.com/JohSand/Orsak/compare/0.4.1...0.4.2
[0.4.1]: https://github.com/JohSand/Orsak/compare/0.4.0...0.4.1
[0.4.0]: https://github.com/JohSand/Orsak/compare/0.3.0...0.4.0
[0.3.0]: https://github.com/JohSand/Orsak/compare/0.2.0...0.3.0
[0.2.0]: https://github.com/JohSand/Orsak/compare/0.1.0...0.2.0
[0.1.0]: https://github.com/JohSand/Orsak/releases/tag/0.1.0
