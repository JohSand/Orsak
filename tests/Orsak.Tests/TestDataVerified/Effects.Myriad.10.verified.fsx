namespace Tmp
#r "../bin/Debug/net9.0/Orsak.dll"
#r "../bin/Debug/net9.0/Orsak.Myriad.dll"

open System
open System.Threading.Tasks
open Orsak
open Orsak.Myriad

[<GenEffects>]
type IPersistenceService =
    abstract Persistence: a: int -> Task

namespace Tmp
open Orsak
type IPersistenceServiceProvider =
    abstract Effect: IPersistenceService

module PersistenceService =
    let persistence a =
        Effect.Create(fun (er: #IPersistenceServiceProvider) -> er.Effect.Persistence a)
