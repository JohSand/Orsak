namespace Orsak

//todo

/// <summary>
/// An effect that produces a list, as built by the <c>effList</c> computation expression.
/// </summary>
type EffectList<'r, 'a, 'e> = Effect<'r, 'a list, 'e>

/// <exclude/>
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

/// <summary>
/// The <c>effList</c> computation expression.
/// </summary>
[<AutoOpen>]
module EffectListBuilder =
    /// <summary>
    /// A computation expression that builds a list with effects: <c>yield</c> adds an item, <c>yield!</c> adds a
    /// list, or the list produced by another <c>effList</c>, and <c>let!</c> binds effects and results.
    /// </summary>
    /// <example>
    /// <code lang="fsharp">
    /// let recipients (order: Order) = effList {
    ///     let! customer = Customers.load order.CustomerId
    ///     yield customer.Email
    ///     for contact in customer.Contacts do
    ///         yield contact.Email
    /// }
    /// </code>
    /// </example>
    let effList = EffectListBuilder()
