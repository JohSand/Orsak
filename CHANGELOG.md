# Changelog

## [Unreleased]

### Changed
More performant implementation of Effect.par
Orsak.Myriad updated to Myriad 1.0.0, and now targets net8.0 (previously netstandard2.1) and requires FSharp.Core 9. Consumers must add `<PackageDownload Include="Myriad" Version="[1.0.0]" />`, see README

### Added
Effect.whenAll, high performance alternative to Effect.par that returns an array
Orsak.Myriad: `[<GenEnvironment>]` generates a `create` function for an interface that inherits provider interfaces, see README
Orsak.Myriad: `Inline = true` on `[<GenEffects>]` and `[<GenEnvironment>]` generates code to append to its own file with MyriadInlineGeneration, e.g. an ad hoc environment in a `rec` test file, see README

### Fixed
Issue where bound effects could be not rerun properly
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
