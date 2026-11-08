#r "../bin/Debug/net10.0/Orsak.Remoting.dll"

open System.Threading.Tasks
open Orsak.Remoting

type Order = { Customer: string; Lines: string list }

type PlaceError =
    | EmptyOrder
    | UnknownProduct of string

[<GenRemote>]
type IOrderApi =
    abstract Place: Order -> Task<Result<int, PlaceError>>
    abstract Add: a: int * b: int -> Task<int>
    abstract Scale: factor: int -> amount: int -> Task<int>
    abstract Count: unit -> Async<int>
    abstract Thrice: int -> ValueTask<int>
    abstract Record: message: string -> Task
