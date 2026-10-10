namespace Orsak.Effects

#nowarn "57"
//#nowarn "3559"

open System.Data.Common
open System.Threading.Tasks
open Orsak
open Orsak.Scoped
open FSharp.Control

/// <exclude/>
[<Experimental("Experimental feature, API not stable")>]
type DbTransactional(tran: DbTransaction) =
    member val Connection = tran.Connection

    interface CompletableScope with
        member this.Complete() = vtask { do tran.Commit() }

        member this.DisposeAsync() : ValueTask = vtask {
            do! tran.DisposeAsync()
            do! this.Connection.DisposeAsync()
        }

/// <exclude/>
/// <summary>
/// Provides the transaction of the scope an effect runs in. An effect asks for the transaction by requiring this of
/// its environment, which only the environment grown in a scope of <see cref="DbTransactional"/> implements.
/// </summary>
[<Experimental("Experimental feature, API not stable")>]
type ITransactionProvider =
    abstract member Transaction: DbTransactional

namespace Orsak.Scoped

open Orsak
open Orsak.Effects
open System.Threading
open System.Threading.Tasks

/// <exclude/>
[<AutoOpen>]
module Builder =
    //effects bound in commitEff run in a transaction, and are ordinary effects of the environment the outer one grows into.
    //the result is an effect of the outer environment, which no longer provides the transaction
    [<Experimental("Experimental feature, API not stable")>]
    let commitEff = Orsak.Scoped.CompletableScopeCreatingEffectBuilder<DbTransactional>()

(*
    Using scoped effects:
*)

//define some kind of Effect that should be scoped
/// <exclude/>
[<Interface>]
type IProgressScope =
    abstract member Progress: System.IProgress<float>
    inherit Scoped

//the provider effects use to reach the progress of the scope they run in
/// <exclude/>
type IProgressProvider =
    abstract member Progress: System.IProgress<float>

//builder for your scope
/// <exclude/>
module Builder2 =
    //using this to create an effect, will create an IProgressScope at the start of the effect, and grow the
    //environment into one that provides it, throughout the effect. The scope is disposed at the end of the effect
    //the result will be just a normal effect, that requires a IProgressScopeProvider which grows in an IProgressScope
    let pEff = Orsak.Scoped.ScopeCreatingEffectBuilder<IProgressScope>()

open Builder2

/// <exclude/>
module Progress =
    //Function for calling our scoped effect
    //it is an ordinary effect, which can only be run in an environment grown in an IProgressScope
    let report (t: float) =
        Effect.Create(fun (x: #IProgressProvider) -> x.Progress.Report(t))

    let reportWhenDone total (e: Effect<'r,_,_>) = eff {
        let! result = e
        do! report total
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

//Implementation of the Scoped Effect
/// <exclude/>
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
/// <exclude/>
type GuidGenerator =
    abstract member NewGuid: unit -> System.Guid

/// <exclude/>
type GuidProvider =
    abstract member Gen: GuidGenerator

/// <exclude/>
module GuidGenerator =
    let newGuid () =
        Effect.Create(fun (p: #GuidProvider) -> p.Gen.NewGuid())

//type alias for convini
/// <exclude/>
type IProgressScopeProvider = ScopeProvider<IProgressScope>

/// <exclude/>
type Runner() =
    interface IProgressScopeProvider with
        member this.BeginScope() = ValueTask<IProgressScope>(ProgressScope())

    interface GuidProvider with
        member this.Gen = raise (System.NotImplementedException())

    interface IGrowsInto<IProgressScope, ScopedRunner> with
        member this.Grow scope = ScopedRunner(scope)

//The environment Runner grows into in an IProgressScope: everything Runner has, and the progress of the scope.
/// <exclude/>
and ScopedRunner(scope: IProgressScope) =
    inherit Runner()

    interface IProgressProvider with
        member this.Progress = scope.Progress

/// <exclude/>
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
        
