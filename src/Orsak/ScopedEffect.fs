namespace Orsak.Scoped

open Orsak
open FSharp.Control
open System.Threading.Tasks
open System

#nowarn "3511"

/// <summary>
/// A scope: something effects run in for a while, which is disposed when they are done, such as a transaction or a
/// unit of work.
/// </summary>
type Scoped = IAsyncDisposable

/// <summary>
/// Implemented by an environment that can begin scopes of type <typeparamref name="'Scope"/>.
/// </summary>
type ScopeProvider<'Scope when 'Scope :> Scoped> =
    /// <summary>Begins a new scope, which is disposed by whoever began it.</summary>
    abstract member BeginScope: unit -> ValueTask<'Scope>

/// <summary>
/// A scope that is completed when the effects running in it succeed, such as a transaction that is committed. A
/// scope disposed without being completed abandons its work.
/// </summary>
type CompletableScope =
    /// <summary>Completes the scope, once the effects running in it have succeeded.</summary>
    abstract member Complete: unit -> ValueTask
    inherit Scoped

/// <summary>
/// Implemented by an environment that can begin completable scopes of type <typeparamref name="'Scope"/>.
/// </summary>
type CompletableScopeProvider<'Scope when 'Scope :> CompletableScope> = ScopeProvider<'Scope>

/// <summary>
/// Implemented by an environment that can grow into <typeparamref name="'Inner"/>, the environment of effects running
/// in a scope of type <typeparamref name="'Scope"/>. Such an environment also begins the scope, as a
/// <see cref="ScopeProvider{Scope}"/>. The inner environment usually inherits the outer one, adding a provider of
/// the scope.
/// </summary>
type IGrowsInto<'Scope, 'Inner when 'Scope :> Scoped> =
    inherit ScopeProvider<'Scope>
    /// <summary>Creates the environment of effects running in <paramref name="scope"/>.</summary>
    abstract member Grow: scope: 'Scope -> 'Inner

/// <summary>
/// Opens scopes of type <typeparamref name="'Scope"/>, for effects needing an environment grown from the one the
/// scoped effect is run with.
/// </summary>
[<AbstractClass; Sealed>]
type Scope<'Scope when 'Scope :> Scoped> =
    /// <summary>
    /// Runs <paramref name="body"/> in a new scope: the scope is begun by the environment, which grows into the
    /// environment of <paramref name="body"/>, and disposed when <paramref name="body"/> ends.
    /// </summary>
    /// <param name="body">The effect to run in the scope, an effect of the grown environment.</param>
    /// <returns>An effect of the outer environment, which no longer needs what the scope provides.</returns>
    static member Within<'Outer, 'Inner, 'a, 'err
        when 'Outer :> IGrowsInto<'Scope, 'Inner>>
        (body: Effect<'Inner, 'a, 'err>)
        : Effect<'Outer, 'a, 'err> =
        mkEffect (fun (outer: 'Outer) -> vtask {
            use! scope = outer.BeginScope()
            let inner = outer.Grow scope

            match! body.Run inner with
            | Ok a -> return Ok a
            | Error e -> return Error e
        })

/// <summary>
/// Opens scopes of type <typeparamref name="'Scope"/> that are completed when their effect succeeds, such as
/// transactions, for effects needing an environment grown from the one the scoped effect is run with.
/// </summary>
[<AbstractClass; Sealed>]
type CompletingScope<'Scope when 'Scope :> CompletableScope> =
    /// <summary>
    /// Runs <paramref name="body"/> in a new scope, and completes the scope if <paramref name="body"/> succeeds. The
    /// scope is begun by the environment, which grows into the environment of <paramref name="body"/>, and disposed
    /// when <paramref name="body"/> ends, without being completed if it failed or threw. An exception is not caught:
    /// it propagates once the scope is disposed.
    /// </summary>
    /// <param name="body">The effect to run in the scope, an effect of the grown environment.</param>
    /// <returns>An effect of the outer environment, which no longer needs what the scope provides.</returns>
    static member Within<'Outer, 'Inner, 'a, 'err
        when 'Outer :> IGrowsInto<'Scope, 'Inner>>
        (body: Effect<'Inner, 'a, 'err>)
        : Effect<'Outer, 'a, 'err> =
        mkEffect (fun (outer: 'Outer) -> vtask {
            use! scope = outer.BeginScope()
            let inner = outer.Grow scope

            match! body.Run inner with
            | Ok a ->
                do! scope.Complete()
                return Ok a
            | Error e -> return Error e
        })

/// <summary>
/// A computation expression for effects running in a scope of type <typeparamref name="'Scope"/>. The effects bound
/// in it are ordinary effects, of the environment the outer one grows into: running the expression begins the scope
/// and grows the environment, so the result is an effect of the outer environment, without the scope.
/// </summary>
/// <example>
/// <code>
/// let progress = ScopeCreatingEffectBuilder&lt;ProgressScope&gt;()
///
/// let importAll files = progress {
///     for file in files do
///         do! import file
///         do! Progress.report file
/// }
/// </code>
/// </example>
type ScopeCreatingEffectBuilder<'Scope when 'Scope :> Scoped>() =
    inherit EffBuilderBase()

    /// <summary>Runs the effects of the expression in a new scope, as <c>Scope.Within</c> does.</summary>
    member inline _.Run<'Outer, 'Inner, 'a, 'err
        when 'Outer :> IGrowsInto<'Scope, 'Inner>>
        (code: EffectCode<'Inner, 'a, 'a, 'err>)
        : Effect<'Outer, 'a, 'err> =
        Scope<'Scope>.Within(eff.Run code)

/// <summary>
/// A computation expression for effects running in a completable scope of type <typeparamref name="'Scope"/>, such
/// as a transaction. The effects bound in it are ordinary effects, of the environment the outer one grows into:
/// running the expression begins the scope and grows the environment, and completes the scope if the effect
/// succeeds, so the result is an effect of the outer environment, without the scope.
/// </summary>
/// <example>
/// <code>
/// let transaction = CompletableScopeCreatingEffectBuilder&lt;Transaction&gt;()
///
/// let transfer source target amount = transaction {
///     do! Accounts.withdraw source amount
///     do! Accounts.deposit target amount
/// }
/// </code>
/// </example>
type CompletableScopeCreatingEffectBuilder<'Scope when 'Scope :> CompletableScope>() =
    inherit EffBuilderBase()

    /// <summary>
    /// Runs the effects of the expression in a new scope, completing it if they succeed, as
    /// <c>CompletingScope.Within</c> does.
    /// </summary>
    member inline _.Run<'Outer, 'Inner, 'a, 'err
        when 'Outer :> IGrowsInto<'Scope, 'Inner>>
        (code: EffectCode<'Inner, 'a, 'a, 'err>)
        : Effect<'Outer, 'a, 'err> =
        CompletingScope<'Scope>.Within(eff.Run code)
