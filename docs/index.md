Orsak
======

Orsak is a library for function effects in F#, allowing pragmatic and composable modelling of side-effects through interfaces. It is inspired by [Zio](https://zio.dev), with the F# encoding similar to the approach described by Bartosz Sypytkowski [here](https://www.bartoszsypytkowski.com/dealing-with-complex-dependency-injection-in-f/).

---

This implementation makes use of resumable code to implement the computation expression builder for the lowest possible overhead. 
The builder also has various overloads for ``Bind``, allowing for friction free interop with TaskLike, Async, Result<_,_> and more.
It supports for, while, use, try-with and try-finally, and also applicative bind when the error-type supports ``+``.
While the computation expression is the intended way of working with effects, the corresponding module should contain all the expected functions.