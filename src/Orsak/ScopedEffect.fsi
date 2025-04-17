namespace Orsak.Scoped

open Orsak
open FSharp.Control
open System.Threading.Tasks
open System
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Core.CompilerServices.StateMachineHelpers

type ScopedEffectCode<'Scope, 'Env, 'TOverall, 'T, 'Err> = EffectCode<'Env * 'Scope, 'TOverall, 'T, 'Err>

#nowarn "57"
#nowarn "3511"

[<AbstractClass>]
type ScopedEffectBuilder =
    new: unit -> ScopedEffectBuilder
    inherit EffBuilderBase

[<AutoOpen>]
module Extension =
    type ScopedEffectBuilder with
        member inline ReturnFrom:
            eff: Effect<'Env, 'TOverall, 'Err> -> ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TOverall, 'Err>

        [<NoEagerConstraintApplication>]
        member inline Bind<'Scope, 'Env, 'TOverall, 'TResult1, 'TResult2, 'Err> :
            eff: Effect<'Env, 'TResult1, 'Err> *
            continuation: ('TResult1 -> ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err>) ->
                ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err>

type Scoped = IAsyncDisposable

type ScopeProvider<'Scope when 'Scope :> Scoped> =
    abstract member BeginScope: unit -> ValueTask<'Scope>

///A computation expression that knows how to start a scope local to the effect, and bind effects in that scope
[<ExperimentalAttribute("")>]
type ScopeCreatingEffectBuilder<'Scope when 'Scope :> Scoped> =
    new: unit -> ScopeCreatingEffectBuilder<'Scope>
    inherit ScopedEffectBuilder

    member inline Bind:
        ex: Effect<('Env * 'Scope), 'a, 'err> * cont: ('a -> EffectCode<('Env * 'Scope), 'a0, 'b, 'err>) ->
            EffectCode<('Env * 'Scope), 'a0, 'b, 'err>

    [<NoEagerConstraintApplication>]
    member inline Bind:
        eff: Effect<'Scope, 'TResult1, 'Err> *
        continuation: ('TResult1 -> ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err>) ->
            ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err>

    member Run:
        effect: ScopedEffectCode<'Scope, 'r, 'a, 'a, 'err> -> Effect<'r, 'a, 'err> when 'r :> ScopeProvider<'Scope>

type ExceptionHandler<'err> =
    abstract member Handle: exn -> 'err

type CompletableScope =
    abstract member Complete: unit -> ValueTask
    inherit Scoped

type CompletableScopeProvider<'Scope when 'Scope :> CompletableScope> = ScopeProvider<'Scope>

///A computation expression that knows how to start a completable scope local to the effect, and bind effects in that scope
[<ExperimentalAttribute("")>]
type CompletableScopeCreatingEffectBuilder<'Scope when 'Scope :> CompletableScope> =
    new: unit -> CompletableScopeCreatingEffectBuilder<'Scope>
    inherit ScopedEffectBuilder

    member inline Bind:
        ex: Effect<('Env * 'Scope), 'a, 'err> * cont: ('a -> EffectCode<('Env * 'Scope), 'a0, 'b, 'err>) ->
            EffectCode<('Env * 'Scope), 'a0, 'b, 'err>

    member inline ReturnFrom<'Scope, 'Env, 'TOverall, 'Err> :
        eff: Effect<'Scope, 'TOverall, 'Err> -> ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TOverall, 'Err>

    [<NoEagerConstraintApplication>]
    member inline Bind:
        scopedEff: Effect<'Scope, 'TResult1, 'Err> *
        continuation: ('TResult1 -> ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err>) ->
            ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err>

    member Run:
        effect: ScopedEffectCode<'Scope, 'r, 'a, 'a, 'err> -> Effect<'r, 'a, 'err>
            when 'r :> ExceptionHandler<'err> and 'r :> CompletableScopeProvider<'Scope>
namespace Orsak.ScopeAware

open Orsak
open Orsak.Scoped
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Core.CompilerServices.StateMachineHelpers

/// <summary>
/// Allows for binding effects in a scope, but does not allow for starting a new scope.
/// Typically would be run by binding them in a scoped effect. Not expected to be run by themselves, since the
/// Effect.run must run it with an already started scope, and the scopes would generally be started implicitly.
/// </summary>
[<ExperimentalAttribute("")>]
type ScopeAwareEffectBuilder<'Scope> =
    new: unit -> ScopeAwareEffectBuilder<'Scope>
    inherit ScopedEffectBuilder

    member inline ReturnFrom<'Scope, 'Env, 'TOverall, 'Err> :
        eff: Effect<'Scope, 'TOverall, 'Err> -> ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TOverall, 'Err>

    /// <summary>
    /// Binds the effects from the scope
    /// </summary>
    /// <param name="eff"></param>
    /// <param name="continuation"></param>
    [<NoEagerConstraintApplication>]
    member inline Bind:
        eff: Effect<'Scope, 'TResult1, 'Err> *
        continuation: ('TResult1 -> ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err>) ->
            ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err>

    member Run: effect: ScopedEffectCode<'Scope, 'r, 'a, 'a, 'err> -> Effect<('r * 'Scope), 'a, 'err>
