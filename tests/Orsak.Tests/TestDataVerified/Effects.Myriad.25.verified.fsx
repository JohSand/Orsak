#r "../bin/Debug/net10.0/Orsak.Remoting.dll"

open System.Collections.Generic
open System.Threading
open Orsak.Remoting

type Order = { Customer: string; Lines: string list }

[<GenRemote>]
type IOrderFeed =
    abstract Orders: customer: string -> RemoteStream<Order>
    abstract Watch: customer: string * cancellationToken: CancellationToken -> RemoteStream<Result<Order, string>>
    abstract Counts: unit -> IAsyncEnumerable<int>

namespace Tmp

module OrderFeedProxy =
    let create (client: Orsak.Remoting.RemoteClient) : IOrderFeed =
        let orders =
            client.Member("IOrderFeed", "Orders", [| typeof<string> |], typeof<Order>)

        let watch =
            client.Member("IOrderFeed", "Watch", [| typeof<string> |], typeof<Result<Order, string>>)

        let counts = client.Member("IOrderFeed", "Counts", [||], typeof<int>)

        { new IOrderFeed with
            member _.Orders a = orders.Stream [| box a |]
            member _.Watch(a, b) = watch.Stream([| box a |], b)
            member _.Counts() = counts.Stream [||]
        }
