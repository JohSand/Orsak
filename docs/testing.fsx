(**
---
title: Testing
category: Guides
categoryindex: 3
index: 6
---
*)
(*** condition: prepare ***)
#r "nuget: Microsoft.Extensions.TimeProvider.Testing, 9.4.0"
#r "../src/Orsak/bin/Release/net8.0/Orsak.dll"
(**
# Testing

An effect doesn't do anything until it is run, and everything it needs comes from the environment it is run with.
Testing an effect is therefore a matter of running it with an environment of fakes, and checking its result. There
is no container to configure and nothing to mock by reflection: a test environment is an ordinary type, or an object
expression, implementing the provider interfaces the effect asks for.

This page tests a small feature that uses Orsak's built-in effects for ids, randomness and time. It is a script: it
runs with `dotnet fsi docs/testing.fsx` once Orsak is built, and the output shown is from such a run. The tests are
plain functions here; in a test project, they would be the bodies of tests in the framework of your choice.

## The feature

A shop gives its customers discount vouchers, each with a unique code, a random discount of 5 to 15 percent, and an
expiry 30 days out. A customer can hold at most three. The vouchers are kept in a store, described in the usual way:
*)
open System
open System.Collections.Generic
open System.Threading.Tasks
open Microsoft.Extensions.Time.Testing
open Orsak

type Voucher = {
    Code: Guid
    Customer: string
    Percent: int
    Expires: DateTimeOffset
}

type VoucherError =
    | TooManyVouchers
    | StoreUnavailable

type IVoucherStore =
    abstract member CountFor: customer: string -> Task<int>
    abstract member Save: Voucher -> Task<Result<unit, VoucherError>>

type IVoucherStoreProvider =
    abstract member Vouchers: IVoucherStore

module Vouchers =
    let countFor customer =
        Effect.Create(fun (p: #IVoucherStoreProvider) -> p.Vouchers.CountFor customer)

    let save voucher =
        Effect.Create(fun (p: #IVoucherStoreProvider) -> p.Vouchers.Save voucher)

(**
Issuing a voucher uses the store, and three of Orsak's built-in effects: `GuidGenerator.genGuid`, `Random.next` and
`Time.utcNow` (.NET 8 and later):
*)
let issueVoucher customer = eff {
    let! existing = Vouchers.countFor customer

    if existing >= 3 then
        return! Error TooManyVouchers
    else
        let! code = GuidGenerator.genGuid ()
        let! percent = Random.next 5 16
        let! now = Time.utcNow ()

        let voucher = {
            Code = code
            Customer = customer
            Percent = percent
            Expires = now.AddDays 30.
        }

        do! Vouchers.save voucher
        return voucher
}

let isValid voucher = eff {
    let! now = Time.utcNow ()
    return now < voucher.Expires
}

(**
The type of `issueVoucher` is inferred as

```fsharp
string -> Effect<#IVoucherStoreProvider & #IGuidGenProvider & #IRandomProvider & #ITimeProvider, Voucher, VoucherError>
```

which is a complete list of what a test has to provide, checked by the compiler: an environment that misses one of
them is a compile error, not a failure at runtime. `isValid` needs only a clock.

## A test environment

The fakes make every source of variation fixed. The store keeps its vouchers in a list, so tests can look at what
was saved. The id generator hands out known ids, the random numbers are always the lowest possible, and the clock is
a `FakeTimeProvider`, from the Microsoft.Extensions.TimeProvider.Testing package, which only moves when told to:
*)
type InMemoryStore() =
    let saved = List<Voucher>()
    member _.Saved = List.ofSeq saved

    interface IVoucherStore with
        member _.CountFor customer =
            Task.FromResult(saved |> Seq.filter (fun v -> v.Customer = customer) |> Seq.length)

        member _.Save voucher =
            saved.Add voucher
            Task.FromResult(Ok())

type SequentialGuids() =
    let mutable next = 0

    interface IGuidGenerator with
        member _.GenGuid() =
            next <- next + 1
            Guid($"00000000-0000-0000-0000-%012d{next}")

type LowestRandom() =
    interface IRandomGenerator with
        member _.Next((minValue, _)) = minValue
        member _.NextDouble() = 0.0

type TestEnv(store: IVoucherStore) =
    let guids = SequentialGuids()
    let random = LowestRandom()
    let clock = FakeTimeProvider(DateTimeOffset(2026, 1, 1, 12, 0, 0, TimeSpan.Zero))
    member _.Clock = clock

    interface IVoucherStoreProvider with
        member _.Vouchers = store

    interface IGuidGenProvider with
        member _.GuidGenerator = guids

    interface IRandomProvider with
        member _.Effect = random

    interface ITimeProvider with
        member _.Clock = clock

(**
Each test creates its own environment, so tests don't share state and can run in parallel.

## Testing the result

`Effect.run` returns the result as a value, to be matched on. The tests use a small helper to fail with a message:
*)
let expect condition message =
    if not condition then
        failwith message

let ``issues a voucher expiring in 30 days`` () = task {
    let store = InMemoryStore()
    let! result = issueVoucher "ada" |> Effect.run (TestEnv store)

    match result with
    | Ok voucher ->
        expect (voucher.Code = Guid "00000000-0000-0000-0000-000000000001") "the first id"
        expect (voucher.Percent = 5) "the lowest discount"
        expect (voucher.Expires = DateTimeOffset(2026, 1, 31, 12, 0, 0, TimeSpan.Zero)) "30 days out"
        expect (store.Saved = [ voucher ]) "the voucher is saved"
    | Error e -> failwith $"failed with {e}"
}

(**
When a test only cares about success, `Effect.runOrFail` is shorter: it returns the value, and throws when the effect
fails, which fails the test with the error in its message:
*)
let ``a fourth voucher is refused`` () = task {
    let env = TestEnv(InMemoryStore())

    for _ in 1..3 do
        let! _ = issueVoucher "ada" |> Effect.runOrFail env
        ()

    let! fourth = issueVoucher "ada" |> Effect.run env
    expect (fourth = Error TooManyVouchers) "the fourth is refused"
}

(**
## Testing failures

Failures of dependencies are faked like anything else. Here the store is down, and an object expression is enough
to say so:
*)
let ``a store failure is the result`` () = task {
    let downStore =
        { new IVoucherStore with
            member _.CountFor _ = Task.FromResult 0
            member _.Save _ = Task.FromResult(Error StoreUnavailable)
        }

    let! result = issueVoucher "ada" |> Effect.run (TestEnv downStore)
    expect (result = Error StoreUnavailable) "the store's error"
}

(**
## Testing time

Since the clock is part of the environment, a test can move it. A voucher is valid until it expires:
*)
let ``a voucher expires after 30 days`` () = task {
    let env = TestEnv(InMemoryStore())
    let! voucher = issueVoucher "ada" |> Effect.runOrFail env

    env.Clock.Advance(TimeSpan.FromDays 29.)
    let! validAfter29 = isValid voucher |> Effect.runOrFail env
    env.Clock.Advance(TimeSpan.FromDays 2.)
    let! validAfter31 = isValid voucher |> Effect.runOrFail env

    expect validAfter29 "valid after 29 days"
    expect (not validAfter31) "expired after 31 days"
}

(**
`isValid` only needs a clock, so it can also be tested with an environment that only has one:
*)
let ``a voucher is valid before it expires`` () = task {
    let clockOnly =
        { new ITimeProvider with
            member _.Clock = FakeTimeProvider(DateTimeOffset(2026, 1, 1, 0, 0, 0, TimeSpan.Zero))
        }

    let voucher = {
        Code = Guid.Empty
        Customer = "ada"
        Percent = 5
        Expires = DateTimeOffset(2026, 2, 1, 0, 0, 0, TimeSpan.Zero)
    }

    let! valid = isValid voucher |> Effect.runOrFail clockOnly
    expect valid "valid in January"
}

(**
Running them all:
*)
for name, test in
    [ nameof ``issues a voucher expiring in 30 days``, ``issues a voucher expiring in 30 days``
      nameof ``a fourth voucher is refused``, ``a fourth voucher is refused``
      nameof ``a store failure is the result``, ``a store failure is the result``
      nameof ``a voucher expires after 30 days``, ``a voucher expires after 30 days``
      nameof ``a voucher is valid before it expires``, ``a voucher is valid before it expires`` ] do
    test().Wait()
    printfn $"passed: {name}"
(**
```text
passed: issues a voucher expiring in 30 days
passed: a fourth voucher is refused
passed: a store failure is the result
passed: a voucher expires after 30 days
passed: a voucher is valid before it expires
```

## Less boilerplate

A test environment type per test module is short, but it adds up. With [Orsak.Myriad](myriad.html),
`[<GenEnvironment>]` on an interface inheriting the providers generates a `create` function taking an anonymous
record of effects, so a test can build its environment in place, and `run` and `runOrFail` functions that create it
and run an effect in one go.
*)
