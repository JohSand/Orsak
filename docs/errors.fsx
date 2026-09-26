(**
---
title: Errors
category: Guides
categoryindex: 3
index: 1
---
*)
(*** condition: prepare ***)
#r "../src/Orsak/bin/Release/net8.0/Orsak.dll"
(**
# Errors

An `Effect<'r, 'a, 'e>` either succeeds with an `'a` or fails with an `'e`. The error is part of the type, so the
ways an effect can fail are visible in its signature, and the compiler checks that they are handled. Exceptions are
left for what is truly unexpected: a bug, or a failure nothing sensible can be done about.

This page shows how to model errors, how to combine effects that fail in different ways, how to handle errors, how
to collect several errors at once, and where exceptions fit in. It is a script: it runs with
`dotnet fsi docs/errors.fsx` once Orsak is built, and the output shown is from such a run.

## Errors as a union

The examples place orders in a shop, which needs a stock service and a payment service. Each is described by an
interface in the usual way, and each fails in its own way, described by a union:
*)
open System
open System.Threading.Tasks
open Orsak

type StockError =
    | OutOfStock of sku: string
    | UnknownProduct of sku: string

type IStock =
    abstract member Reserve: sku: string * quantity: int -> Task<Result<unit, StockError>>

type IStockProvider =
    abstract member Stock: IStock

type PaymentError =
    | Declined of reason: string
    | ProviderUnavailable

type IPayments =
    abstract member Charge: customer: string * amount: decimal -> Task<Result<string, PaymentError>>

type IPaymentsProvider =
    abstract member Payments: IPayments

(**
When the function given to `Effect.Create` returns a `Result`, its error becomes the effect's error:
*)
module Stock =
    let reserve sku quantity =
        Effect.Create(fun (p: #IStockProvider) -> p.Stock.Reserve(sku, quantity))

module Payments =
    let charge customer amount =
        Effect.Create(fun (p: #IPaymentsProvider) -> p.Payments.Charge(customer, amount))

(**
`Stock.reserve` is an `Effect<#IStockProvider, unit, StockError>`, and `Payments.charge` an
`Effect<#IPaymentsProvider, string, PaymentError>`.

Inside `eff`, binding an effect that fails ends the whole effect with that error, so the code only describes the
successful path. `return! Error ...` fails on purpose, and a plain `Result` can be bound like an effect:
*)
let catalog = set [ "mug"; "plate" ]

let checkProduct sku =
    if catalog.Contains sku then Ok sku else Error(UnknownProduct sku)

let reserveAll (lines: (string * int) list) = eff {
    for sku, quantity in lines do
        let! sku = checkProduct sku
        do! Stock.reserve sku quantity
}

(**
The first line that can't be reserved ends `reserveAll`, and the lines after it are never tried.

To run the examples, fake services are enough. The stock has two mugs and no plates, and the payment provider
declines amounts over 100:
*)
type FakeStock() =
    let mutable mugs = 2

    interface IStock with
        member _.Reserve(sku, quantity) =
            Task.FromResult(
                match sku with
                | "mug" when quantity <= mugs ->
                    mugs <- mugs - quantity
                    Ok()
                | "mug"
                | "plate" -> Error(OutOfStock sku)
                | _ -> Error(UnknownProduct sku)
            )

type FakePayments() =
    interface IPayments with
        member _.Charge(customer, amount) =
            Task.FromResult(if amount > 100m then Error(Declined "limit") else Ok $"receipt for {customer}")

type Env() =
    let stock = FakeStock()
    let payments = FakePayments()

    interface IStockProvider with
        member _.Stock = stock

    interface IPaymentsProvider with
        member _.Payments = payments

/// Runs an effect with a new environment, and prints its result.
let show (effect: Effect<Env, 'a, 'e>) =
    let result = effect |> Effect.run (Env())
    printfn $"%A{result.AsTask().Result}"

reserveAll [ "mug", 1; "plate", 1; "mug", 1 ] |> show
reserveAll [ "mug", 1; "bowl", 1 ] |> show
(**
```text
Error (OutOfStock "plate")
Error (UnknownProduct "bowl")
```

## Different errors

An effect has a single error type, so `Stock.reserve` and `Payments.charge` can't be bound in the same `eff` as they
are. The usual approach is a union for the workflow, with a case for each way it can fail, and `Effect.changeError`
to wrap the errors of the effects it uses:
*)
type OrderError =
    | Stock of StockError
    | Payment of PaymentError

let placeOrder customer sku quantity price = eff {
    do! Stock.reserve sku quantity |> Effect.changeError Stock
    let! receipt = Payments.charge customer (decimal quantity * price) |> Effect.changeError Payment
    return receipt
}

placeOrder "ada" "mug" 1 12m |> show
placeOrder "ada" "mug" 1 120m |> show
placeOrder "ada" "mug" 3 12m |> show
(**
```text
Ok "receipt for ada"
Error (Payment (Declined "limit"))
Error (Stock (OutOfStock "mug"))
```

`placeOrder` is an `Effect<#IStockProvider & #IPaymentsProvider, string, OrderError>`: its callers see every way it
can fail, and nothing else. For a plain `Result`, `Result.mapError` does the same as `Effect.changeError`.

## Handling errors

Matching on the result after running the effect handles errors at the composition root. Often an error is better
handled where it happens, and the `Effect` module has functions for that, each running only when the effect fails:

* `Effect.recover` turns the error into a value, so the effect always succeeds;
* `Effect.tryRecover` turns it into a `Result`: a value, or an error;
* `Effect.onError` turns it into another effect, such as a fallback.

Here, a declined payment becomes a message for the customer, while other errors stay errors:
*)
let placeOrderOrExplain customer sku quantity price =
    placeOrder customer sku quantity price
    |> Effect.tryRecover (function
        | Payment(Declined reason) -> Ok $"payment declined ({reason}), the order was not placed"
        | other -> Error other)

placeOrderOrExplain "ada" "mug" 1 120m |> show
(**
```text
Ok "payment declined (limit), the order was not placed"
```

Sometimes the decision depends on more than the error. `Effect.asResult` makes the outcome a value, to be matched on
inside `eff`, while the surrounding effect carries on:
*)
let reserveOrBackorder sku quantity = eff {
    match! Stock.reserve sku quantity |> Effect.asResult with
    | Ok() -> return "reserved"
    | Error(OutOfStock _) -> return "backordered"
    | Error other -> return! Error other
}

reserveOrBackorder "mug" 5 |> show
(**
```text
Ok "backordered"
```

## Collecting errors

Binding one effect after another stops at the first error, which is usually what is wanted: there is no point
charging for an order that couldn't be reserved. When checking input, though, it is friendlier to report everything
that is wrong at once. `and!` binds effects together, running them in parallel where possible, and when more than
one of them fails, their errors are combined with the error type's `+` operator:
*)
type Problems =
    | Problems of string list

    static member (+)(Problems a, Problems b) = Problems(a @ b)

let problem message = Error(Problems [ message ])

let checkCustomer (customer: string) = eff {
    if String.IsNullOrWhiteSpace customer then
        return! problem "the customer is missing"
    else
        return customer
}

let checkLine (sku: string) quantity = eff {
    if quantity <= 0 then
        return! problem $"the quantity of {sku} must be positive"
    else
        return sku, quantity
}

let checkOrder customer sku quantity = eff {
    let! customer = checkCustomer customer
    and! sku, quantity = checkLine sku quantity
    return customer, sku, quantity
}

checkOrder "ada" "mug" 1 |> show
checkOrder "" "mug" 0 |> show
(**
```text
Ok ("ada", "mug", 1)
Error (Problems ["the customer is missing"; "the quantity of mug must be positive"])
```

An error type without `+` can't be used with `and!`, which the compiler reports. `Effect.zip` combines two effects
the same way as `and!`. `Effect.whenAll` and `Effect.par`, which run any number of effects in parallel, don't
combine errors: they fail with the error of the first failing effect, in the order the effects were given.

Checking the input and placing the order fit together with one more case in `OrderError`:

```fsharp
type OrderError =
    | Invalid of string list
    | Stock of StockError
    | Payment of PaymentError

let checkAndPlaceOrder customer sku quantity price = eff {
    let! customer, sku, quantity =
        checkOrder customer sku quantity |> Effect.changeError (fun (Problems problems) -> Invalid problems)
    return! placeOrder customer sku quantity price
}
```

## Exceptions

An exception thrown by an effect isn't turned into an error: it propagates, and the `ValueTask` returned by
`Effect.run` faults with it, as a `Task` would. `try ... with` works inside `eff`, for the exceptions that should
become errors, such as those of a client library that reports failures by throwing:
*)
type IShipping =
    abstract member Book: sku: string -> Task<string>

type IShippingProvider =
    abstract member Shipping: IShipping

type ShippingError = ShippingUnavailable of message: string

let bookShipping sku = eff {
    try
        return! Effect.Create(fun (p: #IShippingProvider) -> p.Shipping.Book sku)
    with :? System.Net.Http.HttpRequestException as e ->
        return! Error(ShippingUnavailable e.Message)
}

type ShippingEnv() =
    interface IShippingProvider with
        member _.Shipping =
            { new IShipping with
                member _.Book _ =
                    Task.FromException<string>(System.Net.Http.HttpRequestException "connection refused")
            }

let booking = bookShipping "mug" |> Effect.run (ShippingEnv())
printfn $"%A{booking.AsTask().Result}"
(**
```text
Error (ShippingUnavailable "connection refused")
```

Going the other way, `Effect.runOrFail` runs an effect and throws when it fails, with the error's string
representation as the message. It suits the edges of a program where a failure should end things, such as a script
or a test, while `Effect.run`, returning the error as a value, suits everywhere else.
*)
