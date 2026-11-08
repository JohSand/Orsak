/// The contract shared by the server and the clients: an Orsak interface describing the side effect, its
/// provider, and the effects created from it, as Orsak.Myriad's [<GenEffects>] would write them. The proxy at the end is
/// what a generator would write too, and is written by hand here.
module Remoting.Shared.Contract

open System.Threading.Tasks
open Orsak
open Orsak.Remoting

type OrderLine = { Product: string; Quantity: int }

type Order = {
    Customer: string
    Lines: OrderLine list
    Note: string option
}

type OrderId = OrderId of int

type PlaceError =
    | EmptyOrder
    | UnknownProduct of string

/// The side effect: ordering, wherever it happens. On the server, a real implementation; in the client, the remote one.
type IOrderApi =
    abstract Place: Order -> Task<Result<OrderId, PlaceError>>
    abstract History: customer: string -> Task<Order list>

type IOrderApiProvider =
    abstract OrderApi: IOrderApi

/// The effects, as [<GenEffects>] generates them from IOrderApi.
module OrderApi =
    let place order =
        Effect.Create(fun (p: #IOrderApiProvider) -> p.OrderApi.Place order)

    let history customer =
        Effect.Create(fun (p: #IOrderApiProvider) -> p.OrderApi.History customer)

/// IOrderApi, called remotely: a generator would write it from the interface. It passes each member's types, as Fable
/// erases generic arguments at run time.
type OrderApiProxy(client: RemoteClient) =
    interface IOrderApi with
        member _.Place order = task {
            let! result =
                client.Call("IOrderApi", "Place", [| box order |], [| typeof<Order> |], typeof<Result<OrderId, PlaceError>>)

            return unbox<Result<OrderId, PlaceError>> result
        }

        member _.History customer = task {
            let! result = client.Call("IOrderApi", "History", [| box customer |], [| typeof<string> |], typeof<Order list>)
            return unbox<Order list> result
        }
