namespace Orsak

//todo

type EffectSeq<'r, 'a, 'e> = Effect<'r, 'a list, 'e>

type EffectSeqBuilder() =
    member inline _.Bind(res: Result<_, _>, [<InlineIfLambda>] f) : EffectSeq<_, _, _> =
        Result.map f res |> Effect.resultJoin

    member inline _.Bind(eff: Effect<_, _, _>, [<InlineIfLambda>] f) : EffectSeq<_, _, _> = Effect.bind f eff
    member inline _.Bind(eff: EffectSeq<_, _, _>, [<InlineIfLambda>] f) = Effect.bind f eff
    member _.Zero() = Effect.ret []
    member _.Yield(value) : EffectSeq<'r, 'a, 'e> = Effect.ret [ value ]
    member _.YieldFrom(value: EffectSeq<'r, 'a, 'e>) = value
    member _.YieldFrom(value: 'a list) : EffectSeq<'r, 'a, 'e> = Effect.ret value
    member _.Delay(f: unit -> EffectSeq<'r, 'a, 'e>) = f ()

    member _.Combine(eff1: EffectSeq<'r, 'a, 'e>, eff2: EffectSeq<'r, 'a, 'e>) : EffectSeq<'r, 'a, 'e> =
        eff1 |> Effect.bind (fun l1 -> eff2 |> Effect.map (List.append l1))

    member this.For(s: seq<'a>, f: 'a -> EffectSeq<'r, 'b, 'e>) : EffectSeq<'r, 'b, 'e> =
        s |> Seq.map f |> Seq.fold (fun a b -> this.Combine(a, b)) (Effect.ret [])

[<AutoOpen>]
module EffectSeqBuilder =
    let effSeq = EffectSeqBuilder()
