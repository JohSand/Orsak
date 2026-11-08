/// The Fable client: its environment provides IOrderApi with Fable.Remoting's proxy, behind the adapter, so the effects
/// of the shared contract run against the server, unchanged. Prints one line per check, which run.sh compares.
module Remoting.Client.Program

open Fable.Remoting.Client
open Orsak
open Remoting.Shared.Contract

type ClientEnv(api: IOrderApi) =
    interface IOrderApiProvider with
        member _.OrderApi = api

let remote baseUrl : IOrderApi =
    Remoting.createApi ()
    |> Remoting.withBaseUrl baseUrl
    |> Remoting.withRouteBuilder OrderApiRemoting.route
    |> Remoting.buildProxy<OrderApiRemoting.Record>
    |> OrderApiRemoting.ofRecord

let apples = { Customer = "ada"; Lines = [| { Product = "apple"; Quantity = 2 } |] }

/// An effect of the shared contract, which doesn't know it runs remotely.
let orderAndReview order = eff {
    let! id = OrderApi.place order
    let! history = OrderApi.history order.Customer
    return id, Array.length history
}

task {
    let env = ClientEnv(remote "http://localhost:5179")

    let! placed = orderAndReview apples |> Effect.run env
    printfn $"placed: %A{placed}"

    let! empty = OrderApi.place { apples with Lines = [||] } |> Effect.run env
    printfn $"empty: %A{empty}"

    let! unknown = OrderApi.place { apples with Lines = [| { Product = "plum"; Quantity = 1 } |] } |> Effect.run env
    printfn $"unknown: %A{unknown}"

    // a transport failure isn't one of the contract's errors: how does it reach the effect?
    let unreachable = ClientEnv(remote "http://localhost:5180")

    try
        let! result = OrderApi.place apples |> Effect.run unreachable
        printfn $"unreachable: returned %A{result}"
    with ex ->
        printfn $"unreachable: raised {ex.Message}"
}
|> ignore
