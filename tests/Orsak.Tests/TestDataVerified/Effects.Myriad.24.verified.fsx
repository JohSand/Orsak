#r "../bin/Debug/net10.0/Orsak.Remoting.dll"

open System.Threading
open System.Threading.Tasks
open Orsak.Remoting

type Order = { Customer: string; Lines: string list }

[<GenRemote>]
type IOrderApi =
    abstract Place: order: Order * cancellationToken: CancellationToken -> Task<int>
    abstract List: CancellationToken -> Task<Order list>
    abstract Scale: factor: int -> ct: CancellationToken -> amount: int -> ValueTask<int>
    abstract Record: message: string * ct: System.Threading.CancellationToken -> Task

namespace Tmp

module OrderApiProxy =
    let create (client: Orsak.Remoting.RemoteClient) : IOrderApi =
        let place = client.Member("IOrderApi", "Place", [| typeof<Order> |], typeof<int>)
        let list = client.Member("IOrderApi", "List", [||], typeof<Order list>)

        let scale =
            client.Member("IOrderApi", "Scale", [| typeof<int>; typeof<int> |], typeof<int>)

        let record =
            client.Member("IOrderApi", "Record", [| typeof<string> |], typeof<unit>)

        { new IOrderApi with
            member _.Place(a, b) = place.Task([| box a |], b)
            member _.List a = list.Task([||], a)
            member _.Scale a b c = scale.ValueTask([| box a; box c |], b)
            member _.Record(a, b) = record.UnitTask([| box a |], b)
        }
