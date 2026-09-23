namespace Tmp
open System.Threading.Tasks
open Orsak
open Orsak.Myriad

// Only one leading I is trimmed from the module name: Inventory, not nventory.
[<GenEffects>]
type IInventory =
    abstract Count: unit -> int

// Configured with ProviderName = "IStorageAccess" and ProviderPropertyName = "Storage",
// which name both the generated provider and the constraint on the effect functions.
[<GenEffects>]
type IStorage =
    abstract Save: string -> Task

namespace Tmp

open Orsak

type IInventoryProvider =
    abstract Effect: IInventory

module Inventory =
    let count () =
        Effect.Create(fun (er: #IInventoryProvider) -> er.Effect.Count())

type IStorageAccess =
    abstract Storage: IStorage

module Storage =
    let save a =
        Effect.Create(fun (er: #IStorageAccess) -> er.Storage.Save a)
