/// The client: its environment provides IOrderApi with the proxy, so the effects of the shared contract run against the
/// server, unchanged. It compiles with Fable, sending with fetch, and on .NET, sending with an HttpClient, and prints one
/// line per check, which run.sh compares.
module Remoting.Client.Program

open System.Threading.Tasks
open Orsak
open Orsak.Remoting
open Remoting.Shared.Contract

type ClientEnv(api: IOrderApi) =
    interface IOrderApiProvider with
        member _.OrderApi = api

/// A transport naming the client in the X-Client header, which the server requires, as authentication would; or none.
let transport (client: string option) : IRemoteTransport =
#if FABLE_COMPILER
    match client with
    | Some name -> FetchTransport(headers = (fun () -> task { return [ "X-Client", name ] }))
    | None -> FetchTransport()
#else
    let http = new System.Net.Http.HttpClient()
    client |> Option.iter (fun name -> http.DefaultRequestHeaders.Add("X-Client", name))
    HttpClientTransport(http)
#endif

let remoteAs client baseUrl : IOrderApi =
    OrderApiProxy.create (RemoteClient(baseUrl, "/api", JsonFormat.Instance, transport client))

let remote = remoteAs (Some "e2e")

let status (e: RemoteCallException) =
    match e.StatusCode with
    | Some s -> string s
    | None -> "none"

// a customer of its own on each platform, as both run against the same server
let customer =
#if FABLE_COMPILER
    "ada on Fable"
#else
    "ada on .NET"
#endif

let apples = {
    Customer = customer
    Lines = [ { Product = "apple"; Quantity = 2 } ]
    Note = Some "ring twice"
}

/// An effect of the shared contract, which doesn't know it runs remotely.
let orderAndReview order = eff {
    let! id = OrderApi.place order
    let! history = OrderApi.history order.Customer
    return id, history |> List.map (fun o -> o.Note)
}

// printed by hand, as %A prints an option differently on Fable, where it is erased
let note =
    function
    | Some n -> $"Some {n}"
    | None -> "None"

let placedOrError =
    function
    | Ok(OrderId id) -> $"Ok {id}"
    | Error EmptyOrder -> "Error EmptyOrder"
    | Error(UnknownProduct p) -> $"Error UnknownProduct {p}"

let checks () : Task<unit> = task {
    let env = ClientEnv(remote "http://localhost:5179")

    match! orderAndReview apples |> Effect.run env with
    | Ok(OrderId id, notes) -> printfn $"""placed: Ok {id}, notes {notes |> List.map note |> String.concat "; "}"""
    | Error e -> printfn $"placed: {placedOrError (Error e)}"

    let! withoutNote = OrderApi.place { apples with Note = None } |> Effect.run env
    printfn $"without a note: {placedOrError withoutNote}"

    let! empty = OrderApi.place { apples with Lines = [] } |> Effect.run env
    printfn $"empty: {placedOrError empty}"

    let! unknown = OrderApi.place { apples with Lines = [ { Product = "plum"; Quantity = 1 } ] } |> Effect.run env
    printfn $"unknown: {placedOrError unknown}"

    // a transport failure isn't one of the contract's errors: it is raised
    let unreachable = ClientEnv(remote "http://localhost:5180")

    try
        let! result = OrderApi.place apples |> Effect.run unreachable
        printfn $"unreachable: returned %A{result}"
    with :? RemoteCallException as e ->
        printfn $"unreachable: raised RemoteCallException, status {status e}"

    // a client the server doesn't know is answered 401, which the client sees as the status
    try
        let! result = OrderApi.place apples |> Effect.run (ClientEnv(remoteAs None "http://localhost:5179"))
        printfn $"anonymous: returned %A{result}"
    with :? RemoteCallException as e ->
        printfn $"anonymous: raised RemoteCallException, status {status e}"

    // cancelling a call aborts its request, which cancels the token the server passed the member
    let api = remote "http://localhost:5179"
    let! heldBefore, releasedBefore = api.Holds()
    use cts = new System.Threading.CancellationTokenSource()
    let call = api.Hold cts.Token

    let rec until (condition: int * int -> bool) tries = task {
        let! holds = api.Holds()

        if condition holds || tries = 0 then
            return condition holds
        else
            do! Task.Delay 50
            return! until condition (tries - 1)
    }

    let! held = until (fun (held, _) -> held > heldBefore) 100
    cts.Cancel()

    try
        do! call
        printfn "cancelled: returned"
    with :? System.OperationCanceledException ->
        printfn "cancelled: raised OperationCanceledException"

    let! released = until (fun (_, released) -> released > releasedBefore) 100
    // by hand, as .NET prints a bool True, and Fable true
    let yesNo b = if b then "yes" else "no"
    printfn $"held on the server: {yesNo held}, released: {yesNo released}"
}

#if FABLE_COMPILER
checks () |> ignore
#else
[<EntryPoint>]
let main _ =
    checks().GetAwaiter().GetResult()
    0
#endif
