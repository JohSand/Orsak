namespace Orsak.Effects

open System.Threading
open System.Runtime.CompilerServices

#nowarn "57"
//#nowarn "3559"

open System
open System.Data.Common
open System.Threading.Tasks
open Orsak
open Orsak.ScopeAware
open Orsak.Scoped
open FSharp.Control

[<Experimental("Experimental feature, API not stable")>]
type DbTransactional(tran: DbTransaction) =
    member val Connection = tran.Connection

    interface CompletableScope with
        member this.Complete() = vtask { do tran.Commit() }

        member this.DisposeAsync() : ValueTask = vtask {
            do! tran.DisposeAsync()
            do! this.Connection.DisposeAsync()
        }

type Transaction<'a, 'err> = Effect<DbTransactional, 'a, 'err>

type EnlistedTransaction<'r, 'a, 'err> = Effect<'r * DbTransactional, 'a, 'err>

namespace Orsak.Scoped

open Orsak
open Orsak.Effects
open System.Threading.Tasks

[<AutoOpen>]
module Builder =
    let commitEff = Orsak.Scoped.TransactionalEffectBuilder<DbTransactional>()

    let trxAware = Orsak.ScopeAware.ScopeAwareEffectBuilder<DbTransactional>()


//define some kind of provider that should be scoped
[<Interface>]
type IProgressScope =
    abstract member Progress: System.IProgress<float>
    inherit System.IAsyncDisposable


//builder for your scope
//once outside the scope, it is just a regular effect
module Builder2 =
    let pEff = Orsak.Scoped.ScopeCreatingEffectBuilder<IProgressScope>()

    let enlistP = Orsak.ScopeAware.ScopeAwareEffectBuilder<IProgressScope>()

module Progress =
    open System.Threading
    open Orsak
    let report (t: float) =
        Effect.Create(fun (x: #IProgressScope) -> x.Progress.Report(t))

    open Builder2

    let reportWhenDone r (e: Effect<'r,_,_>) = enlistP {
        let! result = e
        do! report r
        return result
    }

    let whenAll (effects: Effect<'r,_,_> array) = pEff {
        let total = float effects.Length

        let! result =
            effects
            |> Array.map(reportWhenDone total)
            |> Effect.whenAll

        return result
    }

    let whenAllBatch size (effects: Effect<'r,_,_> array) = pEff {
        let total = float effects.Length
        use ss = new SemaphoreSlim(size)
        let! _x =
            effects
            |> Array.map(fun e -> eff {
                do! ss.WaitAsync()
                try
                    return! reportWhenDone total e
                finally
                    ss.Release() |> ignore
            })
            |> Effect.whenAll

        return ()
    }

//type alias for convini
type IProgressScopeProvider = ScopeProvider<IProgressScope>

type GuidGenerator =
    abstract member NewGuid: unit -> System.Guid

type GuidProvider =
    abstract member Gen: GuidGenerator

type Runner() =
    //here we do not need a separat entity, so we can combine with runner.
    interface IProgressScope with
        member this.DisposeAsync() = ValueTask()
        member this.Progress = new System.Progress<float>(fun _x -> ())

    interface IProgressScopeProvider with
        member this.BeginScope() = ValueTask<_>(this :> IProgressScope)

    interface GuidProvider with
        member this.Gen = raise (System.NotImplementedException())

module GuidGenerator =
    let newGuid () =
        Effect.Create(fun (p: #GuidProvider) -> p.Gen.NewGuid())

module Test =
    
    open Orsak

    let test2() =
        [| GuidGenerator.newGuid () |]
        |> Progress.whenAllBatch 10
    
    let test() = task {
        match!
            test2()
            |> id
            |> Effect.run (Runner())
        with
        | Ok () -> ()
        | Error (_ :string) -> ()
    }
        
