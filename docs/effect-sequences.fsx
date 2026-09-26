(**
---
title: Effect sequences
category: Guides
categoryindex: 3
index: 2
---
*)
(*** condition: prepare ***)
#r "../src/Orsak/bin/Release/net8.0/Orsak.dll"
(**
# Effect sequences

An `Effect` produces one result. Some work produces many, over time: the pages of an API, the rows of a query, the
messages of a queue. An `EffSeq<'r, 'a, 'e>` is an effect for that: like an effect, it requires an environment
`'r` to start, and can fail with `'e`, but it produces a sequence of `'a`, as an async enumerable, one item at a time.
Like an effect, it is cold: nothing happens until it is enumerated.

Effect sequences are written with the `effSeq` computation expression, and read with a `for` loop in `eff`.

This page is a script: it runs with `dotnet fsi docs/effect-sequences.fsx` once Orsak is built, and the output shown
is from such a run.

## A paged API

The examples read orders from an API that returns them a page at a time, with the number of the next page, if any.
It is described by an interface, with a provider interface and a module creating the effect:
*)
open System.Threading.Tasks
open Orsak

type Order = { Id: int; Total: decimal }

type OrderPage = { Orders: Order list; NextPage: int option }

type ApiError = ServiceUnavailable of page: int

type IOrderApi =
    abstract GetPage: page: int -> Task<Result<OrderPage, ApiError>>

type IOrderApiProvider =
    abstract Orders: IOrderApi

module OrderApi =
    let getPage page =
        Effect.Create(fun (p: #IOrderApiProvider) -> p.Orders.GetPage page)

(**
The implementation here has three pages of two orders each, prints every page it is asked for, and can be made to
fail on a given page:
*)
type OrderApi(?failOnPage: int) =
    interface IOrderApi with
        member _.GetPage page = task {
            printfn $"  fetching page {page}"

            if Some page = failOnPage then
                return Error(ServiceUnavailable page)
            else
                let orders = [ for i in 1..2 -> { Id = (page - 1) * 2 + i; Total = decimal (page * 100 + i * 10) } ]
                return Ok { Orders = orders; NextPage = if page < 3 then Some(page + 1) else None }
        }

type Env(api: IOrderApi) =
    interface IOrderApiProvider with
        member _.Orders = api

(**
## Writing a sequence

`effSeq` works like `eff`: `let!` binds effects, tasks and results, and loops work as usual. In addition, `yield`
produces an item, and `yield!` all the items of a list, another sequence, or an async enumerable. This sequence reads
every order, one page after another:
*)
let allOrders () = effSeq {
    let mutable page = Some 1

    while page.IsSome do
        let! result = OrderApi.getPage page.Value
        yield! result.Orders
        page <- result.NextPage
}

(**
Its type is inferred as `EffSeq<#IOrderApiProvider, Order, ApiError>`: it needs an environment that provides the API,
like the effect it uses.

## Reading a sequence

A `for` loop in `eff` reads a sequence, with the environment of the surrounding effect:
*)
let totalOfAllOrders () = eff {
    let mutable total = 0m

    for order in allOrders () do
        printfn $"order {order.Id}: {order.Total}"
        total <- total + order.Total

    return total
}

let run (effect: Effect<Env, 'a, ApiError>) (api: IOrderApi) =
    let result = effect |> Effect.run (Env api)
    printfn $"result: %A{result.AsTask().Result}"

run (totalOfAllOrders ()) (OrderApi())
(**
```text
  fetching page 1
order 1: 110
order 2: 120
  fetching page 2
order 3: 210
order 4: 220
  fetching page 3
order 5: 310
order 6: 320
result: Ok 1290M
```

The pages are fetched as the loop gets to them, not up front: the sequence only does work when the next item is
asked for.

## Reading part of a sequence

Since the sequence is lazy, reading only the beginning does only the work needed for it. `Invoke` starts the sequence
with an environment, and returns it as an `IAsyncEnumerable` of results, for use with other libraries, such as
FSharp.Control.TaskSeq, or as here by hand:
*)
let firstOrders count (api: IOrderApi) = task {
    let enumerator = (allOrders ()).Invoke(Env api).GetAsyncEnumerator()
    let mutable remaining = count

    while remaining > 0 do
        let! hasNext = enumerator.MoveNextAsync()

        if hasNext then
            match enumerator.Current with
            | Ok order -> printfn $"order {order.Id}: {order.Total}"
            | Error error -> printfn $"error: %A{error}"

            remaining <- remaining - 1
        else
            remaining <- 0

    do! enumerator.DisposeAsync()
}

(firstOrders 3 (OrderApi())).Wait()
(**
```text
  fetching page 1
order 1: 110
order 2: 120
  fetching page 2
order 3: 210
```

Only the two pages holding the first three orders were fetched.

## Building sequences from sequences

A `for` loop in `effSeq` reads another sequence, so sequences can be filtered and transformed with the usual
constructs:
*)
let largeOrders () = effSeq {
    for order in allOrders () do
        if order.Total > 200m then
            yield order
}

let printLargeOrders () = eff {
    for order in largeOrders () do
        printfn $"large order {order.Id}: {order.Total}"
}

run (printLargeOrders ()) (OrderApi())
(**
```text
  fetching page 1
  fetching page 2
large order 3: 210
large order 4: 220
  fetching page 3
large order 5: 310
large order 6: 320
result: Ok ()
```

## Errors

When an effect in a sequence fails, the sequence produces the error as its last item, and ends. The `for` loop in
`eff` then fails the surrounding effect with it, after the items that came before:
*)
run (totalOfAllOrders ()) (OrderApi(failOnPage = 3))
(**
```text
  fetching page 1
order 1: 110
order 2: 120
  fetching page 2
order 3: 210
order 4: 220
  fetching page 3
result: Error (ServiceUnavailable 3)
```

As with effects, errors are values: code that reads the sequence with `Invoke` gets them as `Error` items, and
decides itself what to do with them.
*)
