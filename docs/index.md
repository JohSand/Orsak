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

However, Orsak is not intended to be a framework. Rather, it is encouraged and recommended to make use of the existing .NET ecosystem, and 
Orsak is intended to make that as easy and friction-free as possible. It can be used piecemeal, and introduced to existing codebases in isolated chunks.

---

This implementation makes use of resumable code to implement the computation expression builder for the lowest possible overhead. 
The builder also has various overloads for ``Bind``, allowing for friction free interop with ``TaskLike<'a>``, ``Async<'a>``, ``Result<'T,'E>`` and more.
It supports ``for``, ``while``, ``use``, ``try-with`` and ``try-finally``, and also applicative bind when the error-type supports ``+``.
While the computation expression is the intended way of working with effects, the corresponding module should contain all the expected functions.

---

Where to go next:

* [Introduction](1_Motivation.html) and [Composition](2_Motivation.html): creating effects from interfaces, and why they compose.
* [Errors](errors.html): modelling errors as unions, combining and handling them, and where exceptions fit in.
* [Resilience](resilience.html): retrying, backing off, timing out and keeping background work alive.
* [Effect sequences](effect-sequences.html): streaming results, such as pages from an API, as effects.
* [Testing](testing.html): running effects with environments of fakes, including the clock.
* [Generating code](myriad.html): generating effects and environments from interfaces with Orsak.Myriad.
* The [API reference](reference/index.html).
