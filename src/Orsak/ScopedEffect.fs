namespace Orsak.Scoped

open Orsak
open FSharp.Control
open System.Threading.Tasks
open System
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Core.CompilerServices.StateMachineHelpers

/// <summary>
/// The environment of an effect running in a scope: the environment the scoped effect was run with, and the scope
/// it created.
/// </summary>
/// <exclude/>
[<Struct; NoComparison; NoEquality>]
type ScopedEnvironment<'Env, 'Scope> = { Env: 'Env; Scope: 'Scope }

/// <exclude/>
type ScopedEffectCode<'Scope, 'Env, 'TOverall, 'T, 'Err> = EffectCode<ScopedEnvironment<'Env, 'Scope>, 'TOverall, 'T, 'Err>

#nowarn "57"
#nowarn "3511"
#nowarn "3559"

/// <exclude/>
[<AbstractClass>]
type ScopedEffectBuilder() =
    inherit EffBuilderBase()

//shenanigans to make overload resolution work nicely.
/// <exclude/>
[<AutoOpen>]
module Extension =
    type ScopedEffectBuilder with
        member inline this.ReturnFrom<'Scope, 'Env, 'TOverall, 'Err>
            (eff: Effect<'Env, 'TOverall, 'Err>)
            : ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TOverall, 'Err> =
            ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TOverall, 'Err>(fun sm ->
                let env = sm.Data.Environment.Env
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

        member inline this.ReturnFromFinal<'Scope, 'Env, 'TOverall, 'Err>
            (eff: Effect<'Env, 'TOverall, 'Err>)
            : ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TOverall, 'Err> =
            this.ReturnFrom(eff)

        [<NoEagerConstraintApplication>]
        member inline _.Bind<'Scope, 'Env, 'TOverall, 'TResult1, 'TResult2, 'Err>
            (
                eff: Effect<'Env, 'TResult1, 'Err>,
                continuation: 'TResult1 -> ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err>
            ) : ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err> =
            ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err>(fun sm ->
                let env = sm.Data.Environment.Env
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

/// <exclude/>
type Scoped = IAsyncDisposable

/// <exclude/>
type ScopeProvider<'Scope when 'Scope :> Scoped> =
    abstract member BeginScope: unit -> ValueTask<'Scope>

/// <exclude/>
///A computation expression that knows how to start a scope local to the effect, and bind effects in that scope
[<ExperimentalAttribute("")>]
type ScopeCreatingEffectBuilder<'Scope when 'Scope :> Scoped>() =
    inherit ScopedEffectBuilder()

    member inline this.Bind(ex: Effect<ScopedEnvironment<'Env, 'Scope>, 'a, 'err>, cont) = eff.Bind(ex, cont)

    [<NoEagerConstraintApplication>]
    member inline _.Bind<'Env, 'TOverall, 'TResult1, 'TResult2, 'Err>
        (
            eff: Effect<'Scope, 'TResult1, 'Err>,
            continuation: 'TResult1 -> ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err>

        ) : ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err> =
        ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err>(fun sm ->
            let scope = sm.Data.Environment.Scope
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

            match! eff.Run(effect).Run({ Env = env; Scope = scope }) with
            | Ok a -> return Ok a
            | Error e -> return Error e
        })

/// <exclude/>
type ExceptionHandler<'err> =
    abstract member Handle: exn -> 'err

/// <exclude/>
type CompletableScope =
    abstract member Complete: unit -> ValueTask
    inherit Scoped

/// <exclude/>
type CompletableScopeProvider<'Scope when 'Scope :> CompletableScope> = ScopeProvider<'Scope>

/// <exclude/>
///A computation expression that knows how to start a completable scope local to the effect, and bind effects in that scope
[<ExperimentalAttribute("")>]
type CompletableScopeCreatingEffectBuilder<'Scope when 'Scope :> CompletableScope>() =
    inherit ScopedEffectBuilder()

    member inline this.Bind(ex: Effect<ScopedEnvironment<'Env, 'Scope>, 'a, 'err>, cont) = eff.Bind(ex, cont)

    member inline this.ReturnFrom<'Scope, 'Env, 'TOverall, 'Err>
        (eff: Effect<'Scope, 'TOverall, 'Err>)
        : ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TOverall, 'Err> =
        ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TOverall, 'Err>(fun sm ->
            let scope = sm.Data.Environment.Scope
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

    member inline this.ReturnFromFinal<'Scope, 'Env, 'TOverall, 'Err>
        (eff: Effect<'Scope, 'TOverall, 'Err>)
        : ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TOverall, 'Err> =
        this.ReturnFrom(eff)

    [<NoEagerConstraintApplication>]
    member inline _.Bind<'Env, 'TOverall, 'TResult1, 'TResult2, 'Err>
        (
            scopedEff: Effect<'Scope, 'TResult1, 'Err>,
            continuation: 'TResult1 -> ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err>

        ) : ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err> =
        ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TResult2, 'Err>(fun sm ->
            let scope = sm.Data.Environment.Scope
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
            try
                use! scope = (env :> CompletableScopeProvider<'Scope>).BeginScope()

                match! eff.Run(effect).Run({ Env = env; Scope = scope }) with
                | Ok a ->
                    do! scope.Complete()
                    return Ok a
                | Error e -> return Error e
            with e ->
                let (err: 'err) = (env :> ExceptionHandler<'err>).Handle e
                return Error err
        })

/// <summary>
/// Implemented by an environment that can grow into <typeparamref name="'Inner"/>, the environment of effects running
/// in a scope of type <typeparamref name="'Scope"/>. The inner environment usually inherits the outer one, adding a
/// provider of the scope.
/// </summary>
/// <exclude/>
type IGrowsInto<'Scope, 'Inner> =
    /// <summary>Creates the environment of effects running in <paramref name="scope"/>.</summary>
    abstract member Grow: scope: 'Scope -> 'Inner

/// <summary>
/// Opens scopes of type <typeparamref name="'Scope"/>, for effects needing an environment grown from the one the
/// scoped effect is run with.
/// </summary>
/// <exclude/>
[<ExperimentalAttribute(""); AbstractClass; Sealed>]
type Scope<'Scope when 'Scope :> Scoped> =
    /// <summary>
    /// Runs <paramref name="body"/> in a new scope: the scope is begun by the environment, which grows into the
    /// environment of <paramref name="body"/>, and disposed when <paramref name="body"/> ends.
    /// </summary>
    static member Within<'Outer, 'Inner, 'a, 'err
        when 'Outer :> ScopeProvider<'Scope> and 'Outer :> IGrowsInto<'Scope, 'Inner>>
        (body: Effect<'Inner, 'a, 'err>)
        : Effect<'Outer, 'a, 'err> =
        mkEffect (fun (outer: 'Outer) -> vtask {
            use! scope = (outer :> ScopeProvider<'Scope>).BeginScope()
            let inner = (outer :> IGrowsInto<'Scope, 'Inner>).Grow scope

            match! body.Run inner with
            | Ok a -> return Ok a
            | Error e -> return Error e
        })

/// <summary>
/// Opens scopes of type <typeparamref name="'Scope"/> that are completed when their effect succeeds, such as
/// transactions, for effects needing an environment grown from the one the scoped effect is run with.
/// </summary>
/// <exclude/>
[<ExperimentalAttribute(""); AbstractClass; Sealed>]
type CompletingScope<'Scope when 'Scope :> CompletableScope> =
    /// <summary>
    /// Runs <paramref name="body"/> in a new scope, and completes the scope if <paramref name="body"/> succeeds. The
    /// scope is begun by the environment, which grows into the environment of <paramref name="body"/>, and disposed
    /// when <paramref name="body"/> ends, without being completed if it failed or threw. An exception is not caught:
    /// it propagates once the scope is disposed.
    /// </summary>
    static member Within<'Outer, 'Inner, 'a, 'err
        when 'Outer :> CompletableScopeProvider<'Scope> and 'Outer :> IGrowsInto<'Scope, 'Inner>>
        (body: Effect<'Inner, 'a, 'err>)
        : Effect<'Outer, 'a, 'err> =
        mkEffect (fun (outer: 'Outer) -> vtask {
            use! scope = (outer :> CompletableScopeProvider<'Scope>).BeginScope()
            let inner = (outer :> IGrowsInto<'Scope, 'Inner>).Grow scope

            match! body.Run inner with
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


/// <exclude/>
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
            let scope = sm.Data.Environment.Scope
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

    member inline this.ReturnFromFinal<'Scope, 'Env, 'TOverall, 'Err>
        (eff: Effect<'Scope, 'TOverall, 'Err>)
        : ScopedEffectCode<'Scope, 'Env, 'TOverall, 'TOverall, 'Err> =
        this.ReturnFrom(eff)

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
            let scope = sm.Data.Environment.Scope
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

    member _.Run(effect: ScopedEffectCode<'Scope, 'r, 'a, 'a, 'err>) : Effect<ScopedEnvironment<'r, 'Scope>, 'a, 'err> = eff.Run(effect)
