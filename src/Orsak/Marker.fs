namespace Orsak

/// <summary>
/// A marker for an environment that provides an effect of type <typeparamref name="'t"/>. An interface marked with
/// <c>[&lt;GenRunner&gt;]</c> from Orsak.Myriad lists the effects its environment provides by inheriting this.
/// </summary>
/// <typeparam name="'t">The provided effect</typeparam>
type IProvide<'t> =
    /// <summary>The provided effect.</summary>
    abstract member Effect: 't
