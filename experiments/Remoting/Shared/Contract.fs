/// The contract shared by the server and the Fable client: an Orsak interface describing the side effect, its
/// provider, and the effects created from it, as Orsak.Myriad's [<GenEffects>] would write them. The remoting adapter
/// at the end is what a generator would write too, and is written by hand in this spike.
module Remoting.Shared.Contract

open System.Threading.Tasks
open Orsak

type OrderLine = { Product: string; Quantity: int }

type Order = { Customer: string; Lines: OrderLine array }

type OrderId = OrderId of int

type PlaceError =
    | EmptyOrder
    | UnknownProduct of string

/// The side effect: ordering, wherever it happens. On the server, a real implementation; in the client, the remote one.
type IOrderApi =
    abstract Place: Order -> Task<Result<OrderId, PlaceError>>
    abstract History: customer: string -> Task<Order array>

type IOrderApiProvider =
    abstract OrderApi: IOrderApi

/// The effects, as [<GenEffects>] generates them from IOrderApi.
module OrderApi =
    let place order =
        Effect.Create(fun (p: #IOrderApiProvider) -> p.OrderApi.Place order)

    let history customer =
        Effect.Create(fun (p: #IOrderApiProvider) -> p.OrderApi.History customer)

/// The adapter between IOrderApi and Fable.Remoting, whose contract is a record of functions returning Async. A
/// generator would write it from the interface, into the user's project, so Orsak needn't depend on Fable.Remoting.
module OrderApiRemoting =
    type Record = {
        Place: Order -> Async<Result<OrderId, PlaceError>>
        History: string -> Async<Order array>
    }

    /// For the server: Fable.Remoting serves the record, which calls the implementation.
    let toRecord (api: IOrderApi) : Record = {
        Place = fun order -> Async.AwaitTask(api.Place order)
        History = fun customer -> Async.AwaitTask(api.History customer)
    }

    /// For the client: the proxy Fable.Remoting builds is the record, behind the interface.
    let ofRecord (record: Record) : IOrderApi = {
        new IOrderApi with
            member _.Place order = Async.StartAsTask(record.Place order)
            member _.History customer = Async.StartAsTask(record.History customer)
    }

    /// The routes, the same on both sides: /api/{record}/{function}.
    let route (typeName: string) (methodName: string) = $"/api/{typeName}/{methodName}"
