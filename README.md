Orsak
======
---
```fsharp
eff {
    let! line = Effects.Console.readLine()
    do! Effect.Console.writeLine $"Hello {line}"
}
```
---

Orsak is a library for function effects in F#, allowing pragmatic and composable modelling of side-effects through interfaces. 
It is inspired by [Zio](https://zio.dev), with F# encoding similar to the approach described by Bartosz Sypytkowski [here](https://www.bartoszsypytkowski.com/dealing-with-complex-dependency-injection-in-f/). 
The library intends to be as complete as is practical, including a computation expression, supporting functions and operators, and should be compatible with
[FSharpPlus](https://fsprojects.github.io/FSharpPlus/).

However, Orsak is not intended to be a framework. Rather, it is encouraged and recommended to make use of the existing .NET ecosystem, and 
Orsak is intended to make that as easy and friction-free as possible. It can be used piecemeal, and introduced to existing codebases in isolated chunks.

---

This implementation makes use of resumable code to implement the computation expression builder for the lowest possible overhead. 
The builder also has various overloads for ``Bind``, allowing for friction free interop with ``TaskLike<'a>``, ``Async<'a>``, ``Result<'T,'E>`` and more.
It supports ``for``, ``while``, ``use``, ``try-with`` and ``try-finally``, and also applicative bind when the error-type supports ``+``.
While the computation expression is the intended way of working with effects, the corresponding module should contain all the expected functions.

[![NuGet Badge](https://img.shields.io/nuget/v/Orsak.svg?style=flat)](https://www.nuget.org/packages/Orsak) 
[![Build & Tests](https://github.com/JohSand/Orsak/actions/workflows/github-actions.yml/badge.svg)](https://github.com/JohSand/Orsak/actions/workflows/github-actions.yml?query=branch%3Amain)


### Show me the code

---
Imagine having the following interfaces, which each describes some none-pure operation, that is, they both have side-effects.
```fsharp
type IButtonPusher =
    abstract member PushButton: unit -> Task<unit>
    
type IBeanCounter =
    abstract member CountBeans: unit -> Task<int>
```
By creating the following helper-interfaces
```fsharp
type IButtonPusherProvider =
    abstract member ButtonPusher: IButtonPusher
    
type IBeanCounterProvider =
    abstract member BeanCounter: IBeanCounter
```
and the following helpers
```fsharp
let pushButton () =
    Effect.Create(fun (provider: #IButtonPusherProvider) -> provider.ButtonPusher.PushButton())
    
let countBeans () =
    Effect.Create(fun (provider: #IBeanCounterProvider) -> provider.BeanCounter.CountBeans())
```
then it is possible to partially apply, or 'inject' these interfaces into your code by simply making functions calls:
```fsharp
let pushAndCount<'a, 'b when 'a :> IButtonPusherProvider and 'a :> IBeanCounterProvider> ()
    : Effect<'a, int, 'b> =
    eff {
        do! pushButton ()
        let! beans = countBeans ()
        return beans * 2
    }
```
In essence, this is a partially applied function, waiting for an instance of `'a` which implements both `IButtonPusherProvider` and `IBeanCounterProvider`.
Now, in practise you do not need to specify any of this, the type inference will keep track of the requirements of your provider.
```fsharp
let pushAndCount () =
    eff {
        do! pushButton()
        let! beans = countBeans()
        return beans * 2
    }
```
And adding further calls to functions with side-effects will apply the correct constraints seamlessly. 


### Generating the boilerplate with Orsak.Myriad

---
The provider interfaces and `Effect.Create` helpers above can be generated with [Myriad](https://github.com/MoiraeSoftware/myriad).
Reference `Orsak.Myriad` together with `Myriad.Sdk`, and point a generated file at the file containing your interfaces:
```xml
<ItemGroup>
    <Compile Include="Library.fs" />
    <Compile Include="Generated.fs">
        <MyriadFile>Library.fs</MyriadFile>
    </Compile>
</ItemGroup>

<ItemGroup>
    <PackageReference Include="Orsak.Myriad" Version="..." />
    <PackageReference Include="Myriad.Sdk" Version="1.0.0" />
    <!-- Required, see below -->
    <PackageDownload Include="Myriad" Version="[1.0.0]" />
</ItemGroup>
```
Then annotate the interfaces in `Library.fs`:
```fsharp
[<GenEffects>]
type IButtonPusher =
    abstract member PushButton: unit -> Task<unit>
```

**Why the `PackageDownload` is needed:** `Myriad.Sdk` runs the Myriad tool from the NuGet cache
(`<nuget packages>/myriad/<version>/tools/net8.0/any/Myriad.dll`), and tries to download it with a `PackageDownload` declared in its own props file.
NuGet never imports props files from packages during restore, so that download never happens, and the build fails with
`MSB3954: Failed to compute hash for file '...\myriad\1.0.0\tools\net8.0\any\Myriad.dll'`.
Declaring the `PackageDownload` in your own project, or in a `Directory.Build.props`, fixes this. Its version must match the `Myriad.Sdk` version exactly.
A global `dotnet tool install -g Myriad` does **not** help, since global tools are not installed into the NuGet cache.

#### Inline generation

With `<MyriadInlineGeneration>true</MyriadInlineGeneration>`, Myriad appends the generated code to the end of the input file
instead of writing a separate file. Myriad does not tell generators about that setting, so set `Inline = true` on the attributes
in such a file. The code is then generated without `namespace` or `open` declarations, so it continues the file's last module or namespace:
```fsharp
module rec MyTests

open Orsak
open Orsak.Myriad

// An ad hoc environment for the tests in this file
[<GenEnvironment(Inline = true)>]
type ITestEnvironment =
    inherit IButtonPusherProvider
    inherit IGuidGenProvider

let ``pushes the button`` () =
    let env = TestEnvironment.create {| ButtonPusher = fakePusher; GuidGenerator = GuidGenerator.defaultGen () |}
    ...

// TestEnvironment.create is appended here
```
- The generated code comes last, so declare the module or namespace `rec` to use it above. Generated types and
  `TestEnvironment.create` can then be used anywhere in the file. Functions generated by `[<GenEffects>]` have inferred
  types, so F# cannot use them above their declaration without type annotations; use them from later files instead.
- The file must `open Orsak` itself, since some `Effect.Create` overloads come from it and a `rec` module does not allow
  `open` after other declarations.
- `Inline = true` can only be used in the last namespace or module of a file.

#### Environments

An interface that only inherits provider interfaces can be marked with `[<GenEnvironment>]`, which generates a function
to create it from an anonymous record of effects, without writing a concrete type:
```fsharp
[<GenEnvironment>]
type IAppEnvironment =
    inherit IButtonPusherProvider
    inherit IGuidGenProvider

// generated
module AppEnvironment =
    let create (effects: {| ButtonPusher: IButtonPusher; GuidGenerator: Orsak.IGuidGenerator |}) : IAppEnvironment = {
        new IAppEnvironment
        interface IButtonPusherProvider with
            member _.Effect = effects.ButtonPusher
        interface IGuidGenProvider with
            member _.GuidGenerator = effects.GuidGenerator
    }
```
The generator only sees syntax, so it works by convention: `inherit IFooProvider` is taken to expose `Effect: IFoo`.
Orsak's own providers (`IGuidGenProvider`, `ITimeProvider`, `ICacheProvider`, `ICancellationProvider`, `IRandomProvider`) are known.
Other providers can be described in `myriad.toml`, in a section named after the provider:
```toml
[IBeanCounting]
EffectType = "Beans.IBeanCounter"
ProviderPropertyName = "Counter"
```
The generator fails the build with a message if the interface declares members of its own, inherits a generic interface such as `IProvide<'t>`,
inherits a provider whose effect type it cannot infer, or inherits two providers whose effects have the same name.
Only providers inherited directly are considered, so environments cannot be nested. A provider generated by `[<GenEffects>]` must be
generated from an earlier file than the environment that inherits it.

Myriad 1.0.0 requires FSharp.Core 9 or later in any project that loads `Myriad.Core` directly.
See [samples/Orsak.Myriad.Showcase](samples/Orsak.Myriad.Showcase) for a complete example.


---

Main documentation is available at https://johsand.github.io/Orsak/