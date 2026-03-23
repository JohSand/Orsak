namespace Tmp
#r "../bin/Debug/net9.0/Orsak.dll"
#r "../bin/Debug/net9.0/Orsak.Myriad.dll"

open System
open System.Threading.Tasks
open Orsak
open Orsak.Myriad

[<GenEffects>]
type IPersistenceService =
    abstract Persistence: int -> Task

    abstract Test1: int -> Task

    abstract Test2: int -> Task

    abstract Test3: int -> Task

namespace Tmp
open Orsak
type IPersistenceServiceProvider =
    abstract Effect: IPersistenceService

module PersistenceService =
    let persistence a =
        Effect.Create(fun (er: #IPersistenceServiceProvider) -> er.Effect.Persistence a)
    let test1 a =
        Effect.Create(fun (er: #IPersistenceServiceProvider) -> er.Effect.Test1 a)
    let test2 a =
        Effect.Create(fun (er: #IPersistenceServiceProvider) -> er.Effect.Test2 a)
    let test3 a =
        Effect.Create(fun (er: #IPersistenceServiceProvider) -> er.Effect.Test3 a)
