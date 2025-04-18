namespace Orsak.Myriad.Gen

open Orsak

type EffectContext<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'l>
    (a: 'a, b: 'b, c: 'c, d: 'd, e: 'e, f: 'f, g: 'g, h: 'h, i: 'i, j: 'j, k: 'k, l: 'l) =
    member _.A = a
    member _.B = b
    member _.C = c
    member _.D = d
    member _.E = e
    member _.F = f
    member _.G = g
    member _.H = h
    member _.I = i
    member _.J = j
    member _.K = k
    member _.L = l

    member this.Create _ = this

type EffectContext<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k>
    (a: 'a, b: 'b, c: 'c, d: 'd, e: 'e, f: 'f, g: 'g, h: 'h, i: 'i, j: 'j, k: 'k) =
    member _.A = a
    member _.B = b
    member _.C = c
    member _.D = d
    member _.E = e
    member _.F = f
    member _.G = g
    member _.H = h
    member _.I = i
    member _.J = j
    member _.K = k

    member this.Create<'l>(l) =
        EffectContext<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'l>(a, b, c, d, e, f, g, h, i, j, k, l)

type EffectContext<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j>
    (a: 'a, b: 'b, c: 'c, d: 'd, e: 'e, f: 'f, g: 'g, h: 'h, i: 'i, j: 'j) =
    member _.A = a
    member _.B = b
    member _.C = c
    member _.D = d
    member _.E = e
    member _.F = f
    member _.G = g
    member _.H = h
    member _.I = i
    member _.J = j

    member this.Create<'k>(k) =
        EffectContext<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k>(a, b, c, d, e, f, g, h, i, j, k)

type EffectContext<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i>(a: 'a, b: 'b, c: 'c, d: 'd, e: 'e, f: 'f, g: 'g, h: 'h, i: 'i) =
    member _.A = a
    member _.B = b
    member _.C = c
    member _.D = d
    member _.E = e
    member _.F = f
    member _.G = g
    member _.H = h
    member _.I = i

    member this.Create<'j>(j) =
        EffectContext<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j>(a, b, c, d, e, f, g, h, i, j)

type EffectContext<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h>(a: 'a, b: 'b, c: 'c, d: 'd, e: 'e, f: 'f, g: 'g, h: 'h) =
    member _.A = a
    member _.B = b
    member _.C = c
    member _.D = d
    member _.E = e
    member _.F = f
    member _.G = g
    member _.H = h

    member this.Create<'i>(i) =
        EffectContext<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i>(a, b, c, d, e, f, g, h, i)

type EffectContext<'a, 'b, 'c, 'd, 'e, 'f, 'g>(a: 'a, b: 'b, c: 'c, d: 'd, e: 'e, f: 'f, g: 'g) =
    member _.A = a
    member _.B = b
    member _.C = c
    member _.D = d
    member _.E = e
    member _.F = f
    member _.G = g

    member this.Create<'h>(h) =
        EffectContext<'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h>(a, b, c, d, e, f, g, h)

type EffectContext<'a, 'b, 'c, 'd, 'e, 'f>(a: 'a, b: 'b, c: 'c, d: 'd, e: 'e, f: 'f) =
    member _.A = a
    member _.B = b
    member _.C = c
    member _.D = d
    member _.E = e
    member _.F = f

    member this.Create<'g>(g) =
        EffectContext<'a, 'b, 'c, 'd, 'e, 'f, 'g>(a, b, c, d, e, f, g)

type EffectContext<'a, 'b, 'c, 'd, 'e>(a: 'a, b: 'b, c: 'c, d: 'd, e: 'e) =
    member _.A = a
    member _.B = b
    member _.C = c
    member _.D = d
    member _.E = e

    member this.Create<'f>(f) =
        EffectContext<'a, 'b, 'c, 'd, 'e, 'f>(a, b, c, d, e, f)

type EffectContext<'a, 'b, 'c, 'd>(a: 'a, b: 'b, c: 'c, d: 'd) =
    member _.A = a
    member _.B = b
    member _.C = c
    member _.D = d

    member this.Create<'e>(e) =
        EffectContext<'a, 'b, 'c, 'd, 'e>(a, b, c, d, e)

type EffectContext<'a, 'b, 'c>(a: 'a, b: 'b, c: 'c) =
    member _.A = a
    member _.B = b
    member _.C = c

    member this.Create<'d>(d) =
        EffectContext<'a, 'b, 'c, 'd>(a, b, c, d)

type EffectContext<'a, 'b>(a: 'a, b: 'b) =
    member _.A = a
    member _.B = b
    member this.Create<'c>(c) = EffectContext<'a, 'b, 'c>(a, b, c)

type EffectContext<'a>(a: 'a) =
    member _.A = a
    member this.Create<'b>(b) = EffectContext<'a, 'b>(a, b)

[<Struct>]
type EffectContext =
    member this.Create<'b>(b) = EffectContext<'b>(b)

type GenContext<'a, 'b, 'c when 'a: (member Create: 'b -> 'c)> = 'a

module Runner =
    let createFrom (a: 't) =
        { new IProvide<'t> with
            member _.Effect = a
        }
