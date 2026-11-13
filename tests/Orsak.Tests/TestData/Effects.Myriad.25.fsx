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
