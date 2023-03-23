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

However, Orsak is not intended to be a framework. Rather, it is encouraged and recommended to make use of the existing .NET ecosystem, and 
Orsak is intended to make that as easy and friction-free as possible. It can be used piecemeal, and introduced to existing codebases in isolated chunks.

---

This implementation makes use of resumable code to implement the computation expression builder for the lowest possible overhead. 
The builder also has various overloads for ``Bind``, allowing for friction free interop with ``TaskLike<'a>``, ``Async<'a>``, ``Result<'T,'E>`` and more.
It supports ``for``, ``while``, ``use``, ``try-with`` and ``try-finally``, and also applicative bind when the error-type supports ``+``.
While the computation expression is the intended way of working with effects, the corresponding module should contain all the expected functions.
