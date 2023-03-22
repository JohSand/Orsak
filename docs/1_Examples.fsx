(**
---
title: Scoped Effect Provider
category: Examples
categoryindex: 2
index: 1
---
*)
(*** condition: prepare ***)
#I "../src/Orsak/bin/Release/net6.0"
#r "Orsak.dll"

open Orsak
open System.Threading.Tasks
(**

###Scopes

The point of creating an additional indirection with the ``IEffectProvider`` both to decouple the effect implementation, but also to allow for various factory patterns. After all, a factory is just a [function](https://vimeo.com/113588389).
So for example, some effects might require singleton implementations, other per-request implementations, and creating an effect-provider per request, partially applied with singleton dependencies is a natural solution. That also means that
the provider can be made to hold state that is local to that effect. Also transactions
*)
