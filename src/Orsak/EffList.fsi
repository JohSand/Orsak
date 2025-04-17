namespace Orsak

type EffectList<'r, 'a, 'e> = Effect<'r, 'a list, 'e>

type EffectListBuilder =
    new: unit -> EffectListBuilder

    member inline Bind:
        res: Result<'l, 'm> * [<InlineIfLambda>] f: ('l -> Effect<'n, 'o list, 'm>) -> EffectList<'n, 'o, 'm>

    member inline Bind:
        eff: Effect<'h, 'i, 'j> * [<InlineIfLambda>] f: ('i -> Effect<'h, 'k list, 'j>) -> EffectList<'h, 'k, 'j>

    member inline Bind:
        eff: EffectList<'d, 'e, 'f> * [<InlineIfLambda>] f: ('e list -> Effect<'d, 'g, 'f>) -> Effect<'d, 'g, 'f>

    member Zero: unit -> Effect<'a, 'b list, 'c>
    member Yield: value: 'a -> EffectList<'r, 'a, 'e>
    member YieldFrom: value: EffectList<'r, 'a, 'e> -> EffectList<'r, 'a, 'e>
    member YieldFrom: value: 'a list -> EffectList<'r, 'a, 'e>
    member Delay: f: (unit -> EffectList<'r, 'a, 'e>) -> EffectList<'r, 'a, 'e>
    member Combine: eff1: EffectList<'r, 'a, 'e> * eff2: EffectList<'r, 'a, 'e> -> EffectList<'r, 'a, 'e>
    member For: s: 'a seq * f: ('a -> EffectList<'r, 'b, 'e>) -> EffectList<'r, 'b, 'e>

[<AutoOpen>]
module EffectListBuilder =
    val effList: EffectListBuilder
