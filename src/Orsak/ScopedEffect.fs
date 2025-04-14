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
type ScopedEffectBuilder() =
    inherit EffBuilderBase()

//shenanigans to make overload resolution work nicely.
[<AutoOpen>]
module Extension =
    type ScopedEffectBuilder with
        member inline this.ReturnFrom<'Scope, 'Env, 'TOverall, 'Err>
            (eff: Effect<'Env, 'TOverall, 'Err>)
            : ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TOverall, 'Err> =
            ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TOverall, 'Err>(fun sm ->
                let env, _ = sm.Data.Environment
                let task = eff.Run env

                if __useResumableCode<obj> then
                    let mutable awaiter = task.GetAwaiter()

                    let mutable __stack_fin = true

                    if not awaiter.IsCompleted then
                        let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                        __stack_fin <- __stack_yield_fin

                    if __stack_fin then
                        let result = awaiter.GetResult()
                        sm.Data.Result <- result
                        true
                    else
                        sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                        false
                else
                    EffBuilder.BindDynamic(&sm, task, this.Return))

        [<NoEagerConstraintApplication>]
        member inline _.Bind<'Scope, 'Env, 'TOverall, 'TResult1, 'TResult2, 'Err>
            (
                eff: Effect<'Env, 'TResult1, 'Err>,
                continuation: 'TResult1 -> ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err>
            ) : ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err> =
            ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err>(fun sm ->
                let env, _ = sm.Data.Environment
                let task = eff.Run env

                if __useResumableCode<obj> then
                    let mutable awaiter = task.GetAwaiter()

                    let mutable __stack_fin = true

                    if not awaiter.IsCompleted then
                        let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                        __stack_fin <- __stack_yield_fin

                    if __stack_fin then
                        let result = awaiter.GetResult()

                        match result with
                        | Ok result -> (continuation result).Invoke(&sm)
                        | Error error ->
                            sm.Data.Result <- Error error
                            true
                    else
                        sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                        false
                else
                    EffBuilder.BindDynamic(&sm, task, continuation))

type Scoped = IAsyncDisposable

type ScopeProvider<'Scope when 'Scope :> Scoped> =
    abstract member BeginScope: unit -> ValueTask<'Scope>

///A computation expression that knows how to start a scope local to the effect, and bind effects in that scope
[<ExperimentalAttribute("")>]
type ScopeCreatingEffectBuilder<'Scope when 'Scope :> Scoped>() =
    inherit ScopedEffectBuilder()

    member inline this.Bind(ex: Effect<'Env * 'Scope, 'a, 'err>, cont) = eff.Bind(ex, cont)

    [<NoEagerConstraintApplication>]
    member inline _.Bind<'Env, 'TOverall, 'TResult1, 'TResult2, 'Err>
        (
            eff: Effect<'Scope, 'TResult1, 'Err>,
            continuation: 'TResult1 -> ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err>

        ) : ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err> =
        ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err>(fun sm ->
            let _, scope = sm.Data.Environment
            let task = eff.Run scope

            if __useResumableCode<obj> then
                let mutable awaiter = task.GetAwaiter()

                let mutable __stack_fin = true

                if not awaiter.IsCompleted then
                    let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                    __stack_fin <- __stack_yield_fin

                if __stack_fin then
                    let result = awaiter.GetResult()

                    match result with
                    | Ok result -> (continuation result).Invoke(&sm)
                    | Error error ->
                        sm.Data.Result <- Error error
                        true
                else
                    sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                    false
            else
                EffBuilder.BindDynamic(&sm, task, continuation))

    member _.Run<'r, 'a, 'err when 'r :> ScopeProvider<'Scope>>
        (effect: ScopedEffectCode<'Scope, 'r, 'a, 'a, 'err>)
        : Effect<'r, 'a, 'err> =
        mkEffect (fun env -> vtask {
            use! scope = (env :> ScopeProvider<'Scope>).BeginScope()

            match! eff.Run(effect).Run((env, scope)) with
            | Ok a -> return Ok a
            | Error e -> return Error e
        })

type ExceptionHandler<'err> =
    abstract member Handle: exn -> 'err

type CompletableScope =
    abstract member Complete: unit -> ValueTask
    inherit Scoped

type CompletableScopeProvider<'Scope when 'Scope :> CompletableScope> = ScopeProvider<'Scope>

///A computation expression that knows how to start a completable scope local to the effect, and bind effects in that scope
[<ExperimentalAttribute("")>]
type CompletableScopeCreatingEffectBuilder<'Scope when 'Scope :> CompletableScope>() =
    inherit ScopedEffectBuilder()

    member inline this.Bind(ex: Effect<'Env * 'Scope, 'a, 'err>, cont) = eff.Bind(ex, cont)

    member inline this.ReturnFrom<'Scope, 'Env, 'TOverall, 'Err>
        (eff: Effect<'Scope, 'TOverall, 'Err>)
        : ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TOverall, 'Err> =
        ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TOverall, 'Err>(fun sm ->
            let _, scope = sm.Data.Environment
            let task = eff.Run scope

            if __useResumableCode<obj> then
                let mutable awaiter = task.GetAwaiter()

                let mutable __stack_fin = true

                if not awaiter.IsCompleted then
                    let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                    __stack_fin <- __stack_yield_fin

                if __stack_fin then
                    let result = awaiter.GetResult()
                    sm.Data.Result <- result
                    true

                else
                    sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                    false
            else
                EffBuilder.BindDynamic(&sm, task, this.Return))

    [<NoEagerConstraintApplication>]
    member inline _.Bind<'Env, 'TOverall, 'TResult1, 'TResult2, 'Err>
        (
            scopedEff: Effect<'Scope, 'TResult1, 'Err>,
            continuation: 'TResult1 -> ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err>

        ) : ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err> =
        ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err>(fun sm ->
            let _, scope = sm.Data.Environment
            let task = scopedEff.Run scope

            if __useResumableCode<obj> then
                let mutable awaiter = task.GetAwaiter()

                let mutable __stack_fin = true

                if not awaiter.IsCompleted then
                    let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                    __stack_fin <- __stack_yield_fin

                if __stack_fin then
                    let result = awaiter.GetResult()

                    match result with
                    | Ok result -> (continuation result).Invoke(&sm)
                    | Error error ->
                        sm.Data.Result <- Error error
                        true
                else
                    sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                    false
            else
                EffBuilder.BindDynamic(&sm, task, continuation))

    member _.Run<'r, 'a, 'err when 'r :> ExceptionHandler<'err> and 'r :> CompletableScopeProvider<'Scope>>
        (effect: ScopedEffectCode<'Scope, 'r, 'a, 'a, 'err>)
        : Effect<'r, 'a, 'err> =
        mkEffect (fun env -> vtask {
                use! scope = env.BeginScope()
                match! eff.Run(effect).Run((env, scope)) with
                | Ok a ->
                    do! scope.Complete()
                    return Ok a
                | Error e -> return Error e

        })

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
type ScopeAwareEffectBuilder<'Scope>() =
    inherit ScopedEffectBuilder()

    member inline this.ReturnFrom<'Scope, 'Env, 'TOverall, 'Err>
        (eff: Effect<'Scope, 'TOverall, 'Err>)
        : ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TOverall, 'Err> =
        ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TOverall, 'Err>(fun sm ->
            let _, scope = sm.Data.Environment
            let task = eff.Run scope

            if __useResumableCode<obj> then
                let mutable awaiter = task.GetAwaiter()

                let mutable __stack_fin = true

                if not awaiter.IsCompleted then
                    let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                    __stack_fin <- __stack_yield_fin

                if __stack_fin then
                    let result = awaiter.GetResult()
                    sm.Data.Result <- result
                    true

                else
                    sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                    false
            else
                EffBuilder.BindDynamic(&sm, task, this.Return))

    /// <summary>
    /// Binds the effects from the scope
    /// </summary>
    /// <param name="eff"></param>
    /// <param name="continuation"></param>
    [<NoEagerConstraintApplication>]
    member inline _.Bind<'Env, 'TOverall, 'TResult1, 'TResult2, 'Err>
        (
            eff: Effect<'Scope, 'TResult1, 'Err>,
            continuation: 'TResult1 -> ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err>

        ) : ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err> =
        ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err>(fun sm ->
            let _, scope = sm.Data.Environment
            let task = eff.Run scope

            if __useResumableCode<obj> then
                let mutable awaiter = task.GetAwaiter()

                let mutable __stack_fin = true

                if not awaiter.IsCompleted then
                    let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                    __stack_fin <- __stack_yield_fin

                if __stack_fin then
                    let result = awaiter.GetResult()

                    match result with
                    | Ok result -> (continuation result).Invoke(&sm)
                    | Error error ->
                        sm.Data.Result <- Error error
                        true
                else
                    sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                    false
            else
                EffBuilder.BindDynamic(&sm, task, continuation))

    member _.Run(effect: ScopedEffectCode<'Scope, 'r, 'a, 'a, 'err>) : Effect<'r * 'Scope, 'a, 'err> = eff.Run(effect)
