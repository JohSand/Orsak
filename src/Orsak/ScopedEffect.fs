[<Experimental("Experimental feature, API not stable")>]
module Orsak.Scoped

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

        member inline this.ReturnFrom<'Scope, 'Env, 'T, 'TOverall, 'Err>
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
        member inline _.Bind<'Scope, 'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'Err>
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

type ScopeProvider<'Scope when 'Scope :> IAsyncDisposable> =
    abstract member Scope: unit -> ValueTask<'Scope>

type ScopedEffectBuilder<'Scope when 'Scope :> IAsyncDisposable>() =
    inherit ScopedEffectBuilder()

    member _.Run<'r, 'a, 'err when 'r :> ScopeProvider<'Scope>>
        (effect: ScopedEffectCode<'Scope, 'r, 'a, 'a, 'err>)
        : Effect<'r, 'a, 'err> =
        mkEffect (fun env -> vtask {
            use! scope = (env :> ScopeProvider<'Scope>).Scope()

            match! eff.Run(effect).Run((env, scope)) with
            | Ok a -> return Ok a
            | Error e -> return Error e
        })

type ExceptionHandler<'err> =
    abstract member Handle: exn -> 'err

type TransactionScope =
    abstract member CommitAsync: unit -> Task
    inherit IAsyncDisposable

type TransactionScopeProvider<'Scope when 'Scope :> TransactionScope> = ScopeProvider<'Scope>

type TransactionalEffectBuilder<'Scope when 'Scope :> TransactionScope>() =
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

    [<NoEagerConstraintApplication>]
    member inline _.Bind<'Env, 'T, 'TOverall, 'TResult1, 'TResult2, 'Err>
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

    member _.Run<'r, 'a, 'err when 'r :> ExceptionHandler<'err> and 'r :> TransactionScopeProvider<'Scope>>
        (effect: ScopedEffectCode<'Scope, 'r, 'a, 'a, 'err>)
        : Effect<'r, 'a, 'err> =
        mkEffect (fun env -> vtask {
            try
                use! scope = (env :> TransactionScopeProvider<'Scope>).Scope()

                match! eff.Run(effect).Run((env, scope)) with
                | Ok a ->
                    do! scope.CommitAsync()
                    return Ok a
                | Error e -> return Error e
            with e ->
                let (err: 'err) = (env :> ExceptionHandler<'err>).Handle e
                return Error err
        })
