namespace Orsak

//todo

type EffectList<'r, 'a, 'e> = Effect<'r, 'a list, 'e>

type EffectListBuilder() =
    member inline _.Bind(res: Result<_, _>, [<InlineIfLambda>] f) : EffectList<_, _, _> =
        Result.map f res |> Effect.resultJoin

    member inline _.Bind(eff: Effect<_, _, _>, [<InlineIfLambda>] f) : EffectList<_, _, _> = Effect.bind f eff
    member inline _.Bind(eff: EffectList<_, _, _>, [<InlineIfLambda>] f) = Effect.bind f eff
    member _.Zero() = Effect.ret []
    member _.Yield(value) : EffectList<'r, 'a, 'e> = Effect.ret [ value ]
    member _.YieldFrom(value: EffectList<'r, 'a, 'e>) = value
    member _.YieldFrom(value: 'a list) : EffectList<'r, 'a, 'e> = Effect.ret value
    member _.Delay(f: unit -> EffectList<'r, 'a, 'e>) = f ()

    member _.Combine(eff1: EffectList<'r, 'a, 'e>, eff2: EffectList<'r, 'a, 'e>) : EffectList<'r, 'a, 'e> =
        eff1 |> Effect.bind (fun l1 -> eff2 |> Effect.map (List.append l1))

    member this.For(s: seq<'a>, f: 'a -> EffectList<'r, 'b, 'e>) : EffectList<'r, 'b, 'e> =
        s |> Seq.map f |> Seq.fold (fun a b -> this.Combine(a, b)) (Effect.ret [])

[<AutoOpen>]
module EffectListBuilder =
    let effList = EffectListBuilder()

open System.Runtime.CompilerServices



