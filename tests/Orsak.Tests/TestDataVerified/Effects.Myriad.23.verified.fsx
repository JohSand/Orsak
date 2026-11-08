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

namespace Tmp

module OrderApiProxy =
    let create (client: Orsak.Remoting.RemoteClient) : IOrderApi =
        let place =
            client.Member("IOrderApi", "Place", [| typeof<Order> |], typeof<Result<int, PlaceError>>)

        let add =
            client.Member("IOrderApi", "Add", [| typeof<int>; typeof<int> |], typeof<int>)

        let scale =
            client.Member("IOrderApi", "Scale", [| typeof<int>; typeof<int> |], typeof<int>)

        let count = client.Member("IOrderApi", "Count", [| typeof<unit> |], typeof<int>)
        let thrice = client.Member("IOrderApi", "Thrice", [| typeof<int> |], typeof<int>)

        let record =
            client.Member("IOrderApi", "Record", [| typeof<string> |], typeof<unit>)

        { new IOrderApi with
            member _.Place a = place.Task [| box a |]
            member _.Add(a, b) = add.Task [| box a; box b |]
            member _.Scale a b = scale.Task [| box a; box b |]
            member _.Count() = count.Async [| box () |]
            member _.Thrice a = thrice.ValueTask [| box a |]
            member _.Record a = record.UnitTask [| box a |]
        }
