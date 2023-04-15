# Changelog
All notable changes to this project will be documented in this file.


## [Unreleased]

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
