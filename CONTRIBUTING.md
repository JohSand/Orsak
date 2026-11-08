Contributing
============

## Building and testing

```bash
dotnet tool restore
dotnet build -c Release
dotnet test --no-build -c Release
```

The tests run on .NET 8 and .NET 10. To run some of them, filter by method or class name, which accepts wildcards at
the start and end, and names with spaces:

```bash
dotnet test --project tests/Orsak.Tests/Orsak.Tests.fsproj --no-build -c Release -f net10.0 --filter-method "*fanOut*"
dotnet test --project tests/Orsak.Tests/Orsak.Tests.fsproj --no-build -c Release --filter-method "*fanOut completes for an empty source"
```

`dotnet test` runs the tests on Microsoft.Testing.Platform, as set in `global.json`, so it takes the test runner's
options, such as `--filter-method`, `--filter-class` and `--filter-query`, rather than VSTest's `--filter`.

The tests of the Myriad generators compare the generated code with approved snapshots, using
[Verify](https://github.com/VerifyTests/Verify), in `tests/Orsak.Tests/TestDataVerified`. When a change to the
generated code is intended, update the `.verified.fsx` files, and commit them with the change.

Name tests as sentences, with double backticks and spaces, as in ``` ``fanOut completes for an empty source`` ```.
Keep the name of the function a test is about as it is written, as in ``` ``addDelay_ adds delay on false`` ```.
Tests of asynchronous code should have a timeout, as in `[<Fact(Timeout = 10_000)>]`, so a hang fails the test
instead of the run.

## Working on a change

### Branches and pull requests

* Work on a branch from an up-to-date `main`, one topic to a branch, named for its kind: `feature/<topic>`,
  `fix/<topic>`, `docs/<topic>`, or `spike/<topic>` for an experiment that isn't meant to be merged as it is.
* Keep a branch up to date by rebasing it on `main`, rather than merging `main` into it. Pull requests are merged with
  "Rebase and merge", so each commit lands on `main` as it is: write its message as a sentence saying what it does,
  with the reason in the body.
* The title of a pull request is listed in the release notes on GitHub, so write it as a sentence too.
* A larger change to the public API starts with an issue, or a spike, to settle the design before the code.
* Once a branch is merged, delete it, on GitHub and locally. Work that is set aside instead is kept as a tag rather
  than a branch: `git tag archive/<branch> <branch>`, then `git branch -D <branch>`.

### What a change includes

A pull request that changes something users notice comes with:

* tests of it;
* XML doc comments on new public API, with an example for what users call directly, and `<exclude/>` on what is public
  only for technical reasons, such as builder members, so the API reference leaves it out;
* the documentation page about it, new or updated, building with `--strict` (see [Documentation](#documentation));
* a line in `CHANGELOG.md` under `## [Unreleased]`, in the section for its kind: `### Added`, `### Changed`,
  `### Deprecated`, `### Removed` or `### Fixed`. A line for a breaking change starts with `**Breaking:**`. Changes
  users don't notice, such as to tests, CI or internals, need no line.

The template for pull requests lists these.

### A feature's stages

1. **Spike.** An experiment that answers a question, such as whether a design works or what it costs, in a folder of
   its own under `experiments/`, outside the packages. Its `README.md` records what was found, so the answer outlives
   the branch.
2. **Prototype.** In the library, but `internal`, and tested through `InternalsVisibleTo`, as `scalingJob2` was. A
   prototype is not in the changelog.
3. **Experimental.** Public, but marked `[<Experimental("...")>]`, so using it warns (FS0057), and `<exclude/>`, so the
   API reference leaves it out. It may change in any release. Its changelog line ends with `(experimental)`.
4. **Stable.** The attribute and the `<exclude/>` are removed, and it is documented. From here, it follows semver,
   which package validation checks.
5. **Deprecated.** Marked `[<Obsolete("Use ... instead")>]`, with a line under `### Deprecated`. It stays at least
   until the next release that may break, a minor one before 1.0 and a major one after, and is then removed, with a
   line under `### Removed`.

### Breaking changes

CI packs the packages for every pull request, and validates each against the latest release: a change to the public
API that breaks code compiled against that release, such as a removed or changed member, fails the build. To check
locally, pack with the latest release as the baseline:

```bash
dotnet pack -c Release -p:PackageValidationBaselineVersion=0.8.0
```

An intended break is recorded, with the change, in the project's `CompatibilitySuppressions.xml`, which this writes:

```bash
dotnet pack src/Orsak -c Release -p:PackageValidationBaselineVersion=0.8.0 -p:ApiCompatGenerateSuppressionFile=true
```

Check that the file lists only the intended breaks, mark the changelog line `**Breaking:**`, and delete the file once
the release with the break is published, as the next release is validated against that one.

Package validation compares compiled signatures, so some breaks are left to review:

* a change in behavior;
* a change to an `inline` function or a builder member, whose code is compiled into the assemblies of its users;
* a change to the code Orsak.Myriad generates, which its snapshot tests show.

A new package has no release to be validated against, so its project sets
`<DisablePackageBaselineValidation>true</DisablePackageBaselineValidation>` until its first release.

### Framework-specific code

[Supported frameworks](https://johsand.github.io/Orsak/supported-frameworks.html) has the policy for which .NET
versions the packages target. Within it:

* Code for a newer target goes in a file of its own, included with
  `Condition="$([MSBuild]::IsTargetFrameworkCompatible('$(TargetFramework)', 'net11.0'))"`. `#if` is for small
  differences inside a function whose signature is the same on every target.
* A newer target only adds API: an API that exists on several targets behaves the same on all of them.
* `inline` functions and builder members are compiled into their users' code, so their implementation is the same on
  every target. A different mechanism, such as runtime async, gets a name of its own, as a new builder beside `eff`.
* The tests target the same frameworks, with the same conditions.

### Design

* Side effects are described with interfaces, provided by the environment, and errors with the `'e` type, not
  exceptions. Asynchronous code uses `ValueTask`.
* Overloads, and SRTP witnesses, cost type-checking time wherever they are used, in users' code too: keep a member
  to about ten overloads, and prefer interfaces to SRTP where both work.
* A change made for performance is measured with the benchmarks in `benchmarks/`, before and after.

## Releasing

`CHANGELOG.md` is the release notes. Add a line under `## [Unreleased]` with each change users will notice, and a
release moves those lines to a section of its own. Versions follow semver, and before 1.0 a breaking change, such as
dropping a target framework, raises the minor version (see [Breaking changes](#breaking-changes)).

To release, from an up-to-date `main` with a clean working tree:

```bash
scripts/release.sh 0.8.0
git push origin main 0.8.0
```

The script moves the Unreleased notes to a `## [0.8.0]` section, updates the compare links, commits that as
`Release 0.8.0`, and tags the commit. It pushes nothing, so the commit can be checked first. Pushing the tag runs the
build and test workflow, which, once both test jobs pass:

1. publishes the packages to nuget.org, with the version's notes as their release notes, and
2. creates the GitHub release, with the version's notes followed by GitHub's list of merged pull requests, and the
   packages attached.

A tag without a section in `CHANGELOG.md` fails the workflow before anything is published.
`scripts/changelog-section.sh <version>` prints a version's notes, as the workflow reads them.

The workflow publishes to nuget.org with [Trusted Publishing](https://learn.microsoft.com/nuget/nuget-org/trusted-publishing),
so no API key is stored. Its policy on nuget.org is for owner `JohSand`, repository `Orsak` and workflow file
`github-actions.yml`, without an environment. Renaming the workflow file needs the policy changed with it, and the
policy must allow publishing new packages for a release that adds one.

### Prereleases

Every push to `main` that changes the code publishes prerelease packages, versioned by GitVersion, such as
`0.8.1-beta0003`, to GitHub Packages rather than nuget.org. To try one, add the source with a GitHub token that can
read packages (a classic token with `read:packages`; GitHub Packages requires one, even for public packages):

```bash
dotnet nuget add source https://nuget.pkg.github.com/JohSand/index.json --name orsak-prerelease \
    --username <github user> --password <token> --store-password-in-clear-text
```

## Documentation

The documentation site, https://johsand.github.io/Orsak/, is built from `docs/` with
[fsdocs](https://fsprojects.github.io/FSharp.Formatting/), and deployed by `.github/workflows/docs.yml` when the docs
or the source change on `main`. The API reference is generated from the XML doc comments of the libraries. The README
is kept short, and links to the documentation for the details.

### Previewing

```bash
dotnet build -c Release
dotnet fsi docs/.tools/aspnetcore-references.fsx
dotnet fsdocs watch --properties Configuration=Release
```

The second step is needed once, and again after changing the .NET SDK. Scripts can't reference the ASP.NET Core shared
framework as a package, so it writes `docs/.generated/aspnetcore.fsx`, referencing the ASP.NET Core installed with
.NET by path, for the web APIs page to load. The file is specific to the machine, and ignored by git.

This serves the site at http://localhost:8901, and rebuilds and reloads a page when it changes. The API reference is
read from the built assemblies, so after changing doc comments, build again, and restart the watch if the change
doesn't show.

### Checking

Before pushing, build the site the way the workflow does:

```bash
dotnet fsdocs build --properties Configuration=Release --strict --clean --output output
```

`--strict` fails on problems the preview only warns about, such as invalid doc comments and scripts that don't
compile. The `output` folder is ignored by git.

### Pages

* A `.fsx` page is a script, whose code is shown and checked when the site is built. It runs on its own with
  `dotnet fsi docs/<page>.fsx`, against the Release build of Orsak. The output shown on a page is copied from such a
  run, so run the script again after changing its code, and update the output.
* A `.md` page is for code that can't run as a script, such as code generated at build time.
* The front matter at the top of a page sets its title, and where it goes in the navigation (`category`, `index`).
* Every `.md` and `.fsx` file under `docs/` becomes a page, except in folders whose names start with `.`.
