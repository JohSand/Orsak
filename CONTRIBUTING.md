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
