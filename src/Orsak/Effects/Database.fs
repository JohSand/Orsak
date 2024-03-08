namespace Orsak.Effects

#nowarn "57"
//#nowarn "3559"

open System.Data.Common
open System.Threading.Tasks
open Orsak
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

/// <summary>
/// Pure transaction effect.
/// </summary>
type Transaction<'a, 'err> = Effect<DbTransactional, 'a, 'err>

/// <summary>
/// Effects created by trxAware have this type.
/// </summary>
type EnlistedTransaction<'r, 'a, 'err> = Effect<'r * DbTransactional, 'a, 'err>

namespace Orsak.Scoped

open Orsak
open Orsak.Effects
open System.Threading
open System.Threading.Tasks

[<AutoOpen>]
module Builder =
    let commitEff = Orsak.Scoped.CompletableScopeCreatingEffectBuilder<DbTransactional>()

    let trxAware = Orsak.ScopeAware.ScopeAwareEffectBuilder<DbTransactional>()

(*
    Using scoped effects:
*)

//define some kind of Effect that should be scoped
[<Interface>]
type IProgressScope =
    abstract member Progress: System.IProgress<float>
    inherit Scoped


//builder for your scope
module Builder2 =
    //using this to create an effect, will create an IProgressScope at the start of the effect, and it will be available
    //throught the effect, and disposed at the end of the effect
    //the result will be just a normal effect, that requires a IProgressScopeProvider
    let pEff = Orsak.Scoped.ScopeCreatingEffectBuilder<IProgressScope>()

    //using this allows access to the IProgressScope created by a another effect
    //expects to be bound in pEff
    //if bound in a normal eff, the resulting Effect will be specialized as an enlisted progress effect
    let enlistP = Orsak.ScopeAware.ScopeAwareEffectBuilder<IProgressScope>()

open Builder2

module Progress =
    //Pure progress effect, no other effects allowed.
    type Progress<'err> = Effect<IProgressScope, unit, 'err>

    //Function for calling our scoped effect
    let report (t: float) : Progress<_> =
        //by not making x generic, we restrict how report is called. (No #)
        //it can now only really be called through pEff or enlistP
        Effect.Create(fun (x: IProgressScope) -> x.Progress.Report(t))

    let reportWhenDone total (e: Effect<'r,_,_>) = enlistP {
        let! result = e
        do! report total
        return result
    }

    let reportWhenDone2 total (e: Effect<'r,_,_>) = eff {
        let! a = reportWhenDone total e
        return a
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

//Implementation of the Scoped Effect
type ProgressScope() =
    let mutable Calls = 0.
    //unique progress per scope.
    let p = System.Progress<float>(fun total ->
                Calls <- Calls + 1.
                let pcnt = Calls / total
                System.Console.WriteLine(pcnt)
            )

    interface IProgressScope with
        member this.Progress = p
        member this.DisposeAsync() = ValueTask()

(* Mixing in other effects *)
type GuidGenerator =
    abstract member NewGuid: unit -> System.Guid

type GuidProvider =
    abstract member Gen: GuidGenerator

module GuidGenerator =
    let newGuid () =
        Effect.Create(fun (p: #GuidProvider) -> p.Gen.NewGuid())

//type alias for convini
type IProgressScopeProvider = ScopeProvider<IProgressScope>

type Runner() =
    interface IProgressScopeProvider with
        member this.BeginScope() = ValueTask<IProgressScope>(ProgressScope())

    interface GuidProvider with
        member this.Gen = raise (System.NotImplementedException())

module Test =
    let test2() = eff {
        return!
            [| GuidGenerator.newGuid () |]
            |> Progress.whenAllBatch 10
    }
    
    let test() = task {
        match!
            test2()
            |> id
            |> Effect.run (Runner())
        with
        | Ok () -> ()
        | Error (_ :string) -> ()
    }
        
