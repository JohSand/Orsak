Orsak
======
---
```fsharp
eff {
    let! line = Console.readLine ()
    do! Console.writeLine $"Hello {line}"
}
```
---

Orsak is a library for functional effects in F#, allowing pragmatic and composable modelling of side-effects through interfaces. 
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

### Generating the boilerplate

The provider interfaces and `Effect.Create` helpers above can be generated at build time by
[Orsak.Myriad](https://johsand.github.io/Orsak/myriad.html), a [Myriad](https://github.com/MoiraeSoftware/myriad) plugin:
```fsharp
[<GenEffects>]
type IButtonPusher =
    abstract member PushButton: unit -> Task<unit>
```
See [Generating code](https://johsand.github.io/Orsak/myriad.html) for setting it up, and
[the showcase sample](https://github.com/JohSand/Orsak/tree/main/samples/Orsak.Myriad.Showcase) for a complete example.

### Documentation

The documentation is at https://johsand.github.io/Orsak/, with guides to
[errors](https://johsand.github.io/Orsak/errors.html),
[resilience](https://johsand.github.io/Orsak/resilience.html),
[effect sequences](https://johsand.github.io/Orsak/effect-sequences.html),
[web APIs](https://johsand.github.io/Orsak/web-apis.html),
[OpenAPI](https://johsand.github.io/Orsak/openapi.html),
[testing](https://johsand.github.io/Orsak/testing.html) and
[generating code](https://johsand.github.io/Orsak/myriad.html),
and the [API reference](https://johsand.github.io/Orsak/reference/index.html).

To build, test, or work on the documentation, see [CONTRIBUTING.md](https://github.com/JohSand/Orsak/blob/main/CONTRIBUTING.md).
