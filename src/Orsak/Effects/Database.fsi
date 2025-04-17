namespace Orsak.Effects

#nowarn "57"
//#nowarn "3559"

open System.Data.Common
open System.Threading.Tasks
open Orsak
open Orsak.Scoped
open FSharp.Control

[<Experimental("Experimental feature, API not stable")>]
type DbTransactional =
    new: tran: DbTransaction -> DbTransactional
    member Connection: DbConnection
    interface CompletableScope

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
    val commitEff: CompletableScopeCreatingEffectBuilder<DbTransactional>
    val trxAware: ScopeAware.ScopeAwareEffectBuilder<DbTransactional>

[<Interface>]
type IProgressScope =
    abstract member Progress: System.IProgress<float>
    inherit Scoped

module Builder2 =
    val pEff: ScopeCreatingEffectBuilder<IProgressScope>
    val enlistP: ScopeAware.ScopeAwareEffectBuilder<IProgressScope>

open Builder2

module Progress =
    type Progress<'err> = Effect<IProgressScope, unit, 'err>
    val report: t: float -> Progress<'a>
    val reportWhenDone: total: float -> e: Effect<'r, 'a, 'b> -> Effect<('r * IProgressScope), 'a, 'b>
    val reportWhenDone2: total: float -> e: Effect<'r, 'a, 'b> -> Effect<('r * IProgressScope), 'a, 'b>
    val whenAll: effects: Effect<'r, 'a, 'b> array -> Effect<'r, 'a array, 'b> when 'r :> ScopeProvider<IProgressScope>

    val whenAllBatch:
        size: int -> effects: Effect<'r, 'a, 'b> array -> Effect<'r, unit, 'b> when 'r :> ScopeProvider<IProgressScope>

type ProgressScope =
    new: unit -> ProgressScope
    interface IProgressScope

type GuidGenerator =
    abstract member NewGuid: unit -> System.Guid

type GuidProvider =
    abstract member Gen: GuidGenerator

module GuidGenerator =
    val newGuid: unit -> Effect<#GuidProvider, System.Guid, 'b>

type IProgressScopeProvider = ScopeProvider<IProgressScope>

type Runner =
    new: unit -> Runner
    interface IProgressScopeProvider
    interface GuidProvider

module Test =
    val test2: unit -> Effect<'a, unit, 'b> when 'a :> ScopeProvider<IProgressScope> and 'a :> GuidProvider
    val test: unit -> Task<unit>
