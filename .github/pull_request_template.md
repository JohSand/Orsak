<!-- What changes for users, and why. The title is listed in the release notes on GitHub, so write it as a sentence.
     See "Working on a change" in CONTRIBUTING.md. -->



<!-- Tick what applies, and strike out what doesn't, e.g. ~~Docs~~ for a change to CI. -->
- [ ] Tests, named as sentences, with a timeout on asynchronous code
- [ ] XML docs on new public API, with an example for what users call directly
- [ ] Docs pages updated, and `dotnet fsdocs build --properties Configuration=Release --strict --clean --output output` passes
- [ ] A line under `## [Unreleased]` in `CHANGELOG.md`
- [ ] A breaking change is marked `**Breaking:**` in the changelog, and recorded in `CompatibilitySuppressions.xml`
- [ ] Code for one target framework follows "Framework-specific code" in `CONTRIBUTING.md`
