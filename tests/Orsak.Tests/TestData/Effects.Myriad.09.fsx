#r "../bin/Debug/net9.0/Orsak.dll"
#r "../bin/Debug/net9.0/Orsak.Myriad.dll"

open System
open System.Threading.Tasks
open Orsak
open Orsak.Myriad

[<GenEffects>]
type IPersistenceService =
    abstract Persistence: a: int * b: int -> Task
