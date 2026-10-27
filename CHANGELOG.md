# Changelog

## [Unreleased]

### Changed
More performant implementation of Effect.par
Orsak.Myriad updated to Myriad 1.1.0, and now targets net8.0 (previously netstandard2.1) and requires FSharp.Core 9. Consumers must add `<PackageDownload Include="Myriad" Version="[1.1.0]" />`, see README

### Added
Effect.whenAll, high performance alternative to Effect.par that returns an array
CancellationProvider: `fromSource` provides a source the caller owns, and `create` a new one, disposed with the provider
Time.delay, which waits on the environment's clock, and ends early when the environment's cancellation source is cancelled
Log: effects that log message templates with the environment's `ILogger`, e.g. `Log.Information("Imported {Count} rows", count)`
CancellationScope: a `CancellationTokenSource` linked to a token, begun and disposed around an effect, so each source lives exactly as long as the effects using it. `Cancellation.within` runs an effect in one, nested in the current scope if there is one, and `Cancellation.timeout` cancels it after a duration, stopping the effect, unlike `Effect.timeout`
Orsak.Myriad: an environment may grow into a `CancellationScope` by adding an `ICancellationProvider`, and an environment providing an `ICancellationProvider` begins its scopes nested in its source, with `CancellationScope.NestedIn`, so a scope begun in a scope is cancelled with it
Orsak.AspNetCore: endpoints carry `EffectType` metadata, the type of the effect their handler creates, also for a handler that is a lambda
Orsak.AspNetCore: a runner implementing `IDescribeResponses` describes the responses of each endpoint from the type of its effect, as `ResponseDescription` metadata, which is an `IProducesResponseTypeMetadata` for ApiExplorer and OpenAPI generators
Orsak.AspNetCore.OpenApi (.NET 10): `options.AddEffectEndpoints()` adds the endpoints mapped with `MapEffectEndpoints` to Microsoft.AspNetCore.OpenApi's documents, described from their routes, handlers, XML docs and metadata, explicit metadata first
Orsak.AspNetCore.OpenApi: `options.AddFSharpSchemas()` describes options, value options, lists, sets and maps as System.Text.Json writes them: an option is its value or null and not required, and a list, set or map is never null
Orsak.Myriad: `[<GenEnvironment>]` generates a `create` function for an interface that inherits provider interfaces, and `run` and `runOrFail` functions that run an effect in the created environment, see README
Orsak.Myriad: with MyriadInlineGeneration, code is generated to be appended to the file itself, e.g. an ad hoc environment in a `rec` test file. Detected automatically from Myriad 1.1; with Myriad 1.0, set `Inline = true` on `[<GenEffects>]` and `[<GenEnvironment>]`, see README

### Fixed
Issue where bound effects could be not rerun properly
Effect.forever and Effect.retryWhile (and so Effect.retryTimes) no longer overflow the stack when the effect fails synchronously many times in a row: they are loops now, instead of nesting a run per failure
Orsak.Myriad `[<GenEffects>]`: a configured `ProviderName` now names the generated provider, not just the constraint on the effect functions
Orsak.Myriad `[<GenEffects>]`: module names only trim one leading `I` (`IInventory` generates `Inventory`, not `nventory`)
Orsak.Myriad `[<GenEffects>]`: an input declared as `module A.B` generates into `namespace A`, instead of a `namespace A.B` that clashes with the module

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

[0.1.0]: https://github.com/JohSand/Orsak/releases/tag/0.1.0
