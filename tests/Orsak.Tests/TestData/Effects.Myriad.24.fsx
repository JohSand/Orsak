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
