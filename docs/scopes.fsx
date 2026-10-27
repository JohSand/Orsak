(**
---
title: Scopes
category: Guides
categoryindex: 3
index: 7
---
*)
(*** condition: prepare ***)
#r "../src/Orsak/bin/Release/net8.0/Orsak.dll"
(**
# Scopes

Some dependencies only exist for a while: a database transaction, a unit of work, a lock, a progress reporter. They
are begun, a few effects run with them, and they are disposed. In Orsak such a dependency is a *scope*, and the
effects running in it see a bigger environment than the effects outside: the outer environment *grows* into an inner
one, which also provides the scope.

The effects in a scope are ordinary effects. One that needs the transaction says so with a provider interface, as
for any other dependency, and the compiler then checks that it only runs where there is a transaction.

This page shows how to define a scope, how an environment grows into it, how to run effects in it, and what happens
when they fail. It is a script: it runs with `dotnet fsi docs/scopes.fsx` once Orsak is built, and the output shown
is from such a run.

## A scope

The examples move money between accounts. The bank stands in for a database, and a transaction for a database
transaction: its changes reach the bank when it is completed, and are forgotten otherwise.

A scope is anything disposable asynchronously, which is what `Scoped` stands for. A `CompletableScope` can also be
completed, which is how a transaction is committed:
*)
open System
open System.Collections.Generic
open System.Threading.Tasks
open Orsak
open Orsak.Scoped

type Bank() =
    let balances = Dictionary<string, decimal>(dict [ "ada", 100m; "grace", 20m ])
    member _.Balances = balances

type Transaction(bank: Bank) =
    let pending = Dictionary<string, decimal>()

    member _.Balance(account: string) =
        match pending.TryGetValue account with
        | true, balance -> balance
        | false, _ -> bank.Balances[account]

    member _.SetBalance(account: string, balance: decimal) = pending[account] <- balance

    interface CompletableScope with
        member _.Complete() =
            for KeyValue(account, balance) in pending do
                bank.Balances[account] <- balance

            printfn "transaction committed"
            ValueTask()

        member _.DisposeAsync() =
            printfn "transaction disposed"
            ValueTask()

(**
## Effects that need the scope

The effects reach the transaction through a provider interface, like any other dependency:
*)
type ITransactionProvider =
    abstract member Transaction: Transaction

type TransferError = InsufficientFunds of account: string

module Accounts =
    let balance account =
        Effect.Create(fun (p: #ITransactionProvider) -> p.Transaction.Balance account)

    let private setBalance account balance =
        Effect.Create(fun (p: #ITransactionProvider) -> p.Transaction.SetBalance(account, balance))

    let withdraw account amount = eff {
        let! current = balance account

        if current < amount then
            return! Error(InsufficientFunds account)
        else
            return! setBalance account (current - amount)
    }

    let deposit account amount = eff {
        let! current = balance account
        return! setBalance account (current + amount)
    }

(**
Effects that need the scope mix freely with effects that don't. The transfers are also written to an audit log,
which has nothing to do with the transaction:
*)
type IAudit =
    abstract member Record: message: string -> unit

type IAuditProvider =
    abstract member Audit: IAudit

module Audit =
    let record message =
        Effect.Create(fun (p: #IAuditProvider) -> p.Audit.Record message)

let transfer source target (amount: decimal) = eff {
    do! Accounts.withdraw source amount
    do! Accounts.deposit target amount
    do! Audit.record $"moved {amount} from {source} to {target}"
}

(**
`transfer` is an effect of any environment with both providers:

```fsharp
val transfer:
  source: string -> target: string -> amount: decimal -> Effect<'r, unit, TransferError>
    when 'r :> ITransactionProvider and 'r :> IAuditProvider
```

## An environment that grows

The environment of the application has no transaction, so it doesn't implement `ITransactionProvider`. Instead it
implements `IGrowsInto`, which says how to begin one, and which environment it grows into once there is one. With one
kind of scope, the inner environment can inherit the outer one, and add the provider of the scope:
*)
type AppEnv(bank: Bank) =
    interface IAuditProvider with
        member _.Audit =
            { new IAudit with
                member _.Record message = printfn $"audit: %s{message}"
            }

    interface IGrowsInto<Transaction, TransactionEnv> with
        member _.BeginScope() =
            printfn "transaction begun"
            ValueTask<Transaction>(Transaction bank)

        member _.Grow transaction = TransactionEnv(bank, transaction)

and TransactionEnv(bank: Bank, transaction: Transaction) =
    inherit AppEnv(bank)

    interface ITransactionProvider with
        member _.Transaction = transaction

(**
`BeginScope` is a member of `ScopeProvider<'Scope>`, which `IGrowsInto` inherits. An environment that only begins
scopes, without growing, implements that alone, also under the name `CompletableScopeProvider`.

## Running effects in a scope

`CompletingScope<'Scope>.Within` takes an effect of the inner environment and gives an effect of the outer one. When
it runs, it begins the scope, grows the environment, runs the effect, completes the scope if the effect succeeded,
and disposes the scope:
*)
let transferInTransaction source target amount =
    transfer source target amount |> CompletingScope<Transaction>.Within

(**
The transaction is gone from the type: what is left is an environment that grows, in a transaction, into one with
what `transfer` needs.

```fsharp
val transferInTransaction:
  source: string -> target: string -> amount: decimal -> Effect<'Outer, unit, TransferError>
    when 'Outer :> IGrowsInto<Transaction, 'Inner>
    and 'Inner :> ITransactionProvider
    and 'Inner :> IAuditProvider
```

`AppEnv` is such an environment:
*)
let bank = Bank()
let env = AppEnv bank

let show (effect: Effect<AppEnv, 'a, 'e>) =
    let result = (Effect.run env effect).AsTask().Result
    printfn $"%A{result}"
    printfn $"""ada: {bank.Balances["ada"]}, grace: {bank.Balances["grace"]}"""

transferInTransaction "ada" "grace" 30m |> show
(**
```text
transaction begun
audit: moved 30 from ada to grace
transaction committed
transaction disposed
Ok ()
ada: 70, grace: 50
```

Running `transfer` itself with `AppEnv` doesn't compile, as `AppEnv` provides no transaction:

```fsharp
transfer "ada" "grace" 30m |> Effect.run env
```

```text
error FS0001: The type 'AppEnv' is not compatible with the type 'ITransactionProvider'
```

## When the effects fail

When the effect fails, the scope is disposed without being completed. Here the first transfer succeeds and the
second fails, so neither reaches the bank:
*)
let twoTransfers =
    eff {
        do! transfer "ada" "grace" 30m
        do! transfer "grace" "ada" 500m
    }
    |> CompletingScope<Transaction>.Within

twoTransfers |> show
(**
```text
transaction begun
audit: moved 30 from ada to grace
transaction disposed
Error (InsufficientFunds "grace")
ada: 70, grace: 50
```

The same goes for an exception. It isn't caught or turned into an error: the scope is disposed without being
completed, and the exception propagates to whoever runs the effect, as described under [Errors](errors.html).

The audit log shows what a scope doesn't do. Only what goes through the scope is undone, so the first transfer is
still in the log.

## A builder for a scope

Opening a scope around an expression is common enough to have a computation expression. A builder is created once
for the type of scope, and is then used like `eff`:
*)
let transaction = CompletableScopeCreatingEffectBuilder<Transaction>()

let payBonus account amount = transaction {
    do! Accounts.deposit account amount
    do! Audit.record $"paid a bonus of {amount} to {account}"
    return! Accounts.balance account
}

payBonus "grace" 5m |> show
(**
```text
transaction begun
audit: paid a bonus of 5 to grace
transaction committed
transaction disposed
Ok 55M
ada: 70, grace: 55
```

Each `transaction { }` is a scope of its own. Binding two of them in an `eff` runs two transactions, one after the
other, and the second runs even though it can't undo the first:
*)
eff {
    let! grace = payBonus "grace" 5m
    let! ada = payBonus "ada" 5m
    return grace, ada
}
|> show
(**
```text
transaction begun
audit: paid a bonus of 5 to grace
transaction committed
transaction disposed
transaction begun
audit: paid a bonus of 5 to ada
transaction committed
transaction disposed
Ok (60M, 75M)
ada: 75, grace: 60
```

So the parts that should share a transaction are written with plain `eff`, as `transfer` is, and the scope is opened
once, around all of them. The type shows which is which: an effect that needs `ITransactionProvider` joins the
transaction it runs in, and one that needs `IGrowsInto<Transaction, 'Inner>` begins its own.

## Scopes that aren't completed

A scope without anything to complete is opened with `Scope<'Scope>.Within`, or a `ScopeCreatingEffectBuilder`. It is
begun and disposed in the same way, whether the effects succeed or fail.

This scope counts the steps of a job, and reports each as it is done:
*)
type ProgressScope() =
    let mutable steps = 0

    member _.Report(step: string) =
        steps <- steps + 1
        printfn $"step %i{steps}: %s{step}"

    interface Scoped with
        member _.DisposeAsync() =
            printfn $"finished after %i{steps} steps"
            ValueTask()

type IProgressProvider =
    abstract member Progress: ProgressScope

module Progress =
    let report step =
        Effect.Create(fun (p: #IProgressProvider) -> p.Progress.Report step)

let progress = ScopeCreatingEffectBuilder<ProgressScope>()

(**
A function can open a scope around effects it is given. `inOrder` runs any steps in one progress scope, reporting
each, and doesn't need to be `inline`:
*)
let inOrder (steps: (string * Effect<'r, unit, 'e>) list) = progress {
    for name, step in steps do
        do! step
        do! Progress.report name
}

(**
```fsharp
val inOrder:
  steps: (string * Effect<'r, unit, 'e>) list -> Effect<'Outer, unit, 'e>
    when 'Outer :> IGrowsInto<ProgressScope, 'r>
    and 'r :> IProgressProvider
```

The steps are effects of the inner environment, so they may use everything the outer environment provides:
*)
type JobEnv() =
    interface IAuditProvider with
        member _.Audit =
            { new IAudit with
                member _.Record message = printfn $"audit: %s{message}"
            }

    interface IGrowsInto<ProgressScope, JobProgressEnv> with
        member _.BeginScope() = ValueTask<ProgressScope>(ProgressScope())
        member _.Grow scope = JobProgressEnv scope

and JobProgressEnv(scope: ProgressScope) =
    inherit JobEnv()

    interface IProgressProvider with
        member _.Progress = scope

let job: Effect<JobEnv, unit, string> =
    inOrder [
        "export", Audit.record "exported the accounts"
        "archive", Audit.record "archived the export"
    ]

let jobResult = (Effect.run (JobEnv()) job).AsTask().Result
printfn $"%A{jobResult}"
(**
```text
audit: exported the accounts
step 1: export
audit: archived the export
step 2: archive
finished after 2 steps
Ok ()
```

## Several scopes

An environment can grow into more than one kind of scope, with an `IGrowsInto` for each. Scopes can then be nested,
such as a job that reports its progress, where each step is a transaction of its own that also reports:
*)
let pay account = transaction {
    do! Accounts.deposit account 1m
    do! Progress.report $"paid {account}"
}

let payAll (accounts: string list) = progress {
    for account in accounts do
        do! pay account
}

(**
The transaction is a function of its own, rather than a `transaction { }` written inside the `progress { }`. An
expression of a scope's builder written directly inside another computation expression doesn't compile, failing
with error FS3401. Where the scope has to be opened in place, `eff { ... } |> CompletingScope<Transaction>.Within`
works.


The inner environment of the progress scope is the outer environment of the transaction, so it is the one that grows
into an environment with both. This is where inheriting the outer environment stops working: an inner environment
that inherits the outer one also inherits how it grows, and would grow into an environment with a transaction but
without the progress. The compiler rejects that, as `Progress.report` then has no provider.

So with several scopes, the environments inherit what they share from a common base, and each says for itself how
it grows. The base begins the scopes, with `ScopeProvider`, so that is written once:
*)
type BatchBase(bank: Bank) =
    interface IAuditProvider with
        member _.Audit =
            { new IAudit with
                member _.Record message = printfn $"audit: %s{message}"
            }

    interface CompletableScopeProvider<Transaction> with
        member _.BeginScope() =
            printfn "transaction begun"
            ValueTask<Transaction>(Transaction bank)

    interface ScopeProvider<ProgressScope> with
        member _.BeginScope() = ValueTask<ProgressScope>(ProgressScope())

type BatchEnv(bank: Bank) =
    inherit BatchBase(bank)

    interface IGrowsInto<Transaction, BatchTransactionEnv> with
        member _.Grow transaction = BatchTransactionEnv(bank, transaction)

    interface IGrowsInto<ProgressScope, BatchProgressEnv> with
        member _.Grow progress = BatchProgressEnv(bank, progress)

and BatchTransactionEnv(bank: Bank, transaction: Transaction) =
    inherit BatchBase(bank)

    interface ITransactionProvider with
        member _.Transaction = transaction

and BatchProgressEnv(bank: Bank, progress: ProgressScope) =
    inherit BatchBase(bank)

    interface IProgressProvider with
        member _.Progress = progress

    interface IGrowsInto<Transaction, BatchProgressTransactionEnv> with
        member _.Grow transaction = BatchProgressTransactionEnv(bank, progress, transaction)

and BatchProgressTransactionEnv(bank: Bank, progress: ProgressScope, transaction: Transaction) =
    inherit BatchBase(bank)

    interface IProgressProvider with
        member _.Progress = progress

    interface ITransactionProvider with
        member _.Transaction = transaction

let batchEnv = BatchEnv bank

let showBatch (effect: Effect<BatchEnv, 'a, 'e>) =
    let result = (Effect.run batchEnv effect).AsTask().Result
    printfn $"%A{result}"

(payAll [ "ada"; "grace" ]: Effect<BatchEnv, unit, TransferError>) |> showBatch
(**
```text
transaction begun
step 1: paid ada
transaction committed
transaction disposed
transaction begun
step 2: paid grace
transaction committed
transaction disposed
finished after 2 steps
Ok ()
```

Every combination of scopes that effects run in is an environment of its own, and so is every order of nesting
them: here a transaction can be opened in a progress scope, but not the other way around, as `BatchTransactionEnv`
doesn't grow. That is checked when compiling, and it is a reason to keep the kinds of scope few, and the nesting
shallow.

### Scopes side by side

Opening two kinds of scope one after the other, rather than one in the other, needs the type of the environment to
be known. Left to be inferred, this doesn't compile:

```fsharp
let settle () = eff {
    let! _ = payBonus "ada" 1m
    do! inOrder [ "export", Audit.record "exported the accounts"; "archive", Audit.record "archived the export" ]
}
```

```text
error FS0041: No overloads match for method 'Bind'.
```

The environment would have to be any type that grows in both a `Transaction` and a `ProgressScope`, and F# can't
require the same interface of a type twice, with different type arguments. Naming the environment of each of the
effects solves it:
*)
let bonus: Effect<BatchEnv, decimal, TransferError> = payBonus "ada" 1m

let export: Effect<BatchEnv, unit, TransferError> =
    inOrder [ "export", Audit.record "exported the accounts"; "archive", Audit.record "archived the export" ]

let settle = eff {
    let! _ = bonus
    do! export
}

settle |> showBatch
(**
```text
transaction begun
audit: paid a bonus of 1 to ada
transaction committed
transaction disposed
audit: exported the accounts
step 1: export
audit: archived the export
step 2: archive
finished after 2 steps
Ok ()
```

Effects that open one kind of scope, however deeply they nest others in it, stay generic, as `payAll` is: each
environment along the way is a type of its own, and only grows once.

## In short

* A scope is an `IAsyncDisposable`, or a `CompletableScope` when there is something to commit.
* Effects ask for the scope with a provider interface of their own, such as `ITransactionProvider`.
* The outer environment implements `IGrowsInto<'Scope, 'Inner>`: it begins the scope, as a `ScopeProvider<'Scope>`,
  and creates the inner environment, which provides the scope.
* `Scope<'Scope>.Within` and `CompletingScope<'Scope>.Within` open a scope around an effect, and
  `ScopeCreatingEffectBuilder` and `CompletableScopeCreatingEffectBuilder` around a computation expression. The
  result is an effect of the outer environment.
* With several kinds of scope, the environments inherit a common base, and each declares how it grows. See
  [Several scopes](#Several-scopes).

Orsak comes with a scope for ADO.NET transactions, `DbTransactional`, with its provider `ITransactionProvider` and
builder `commitEff`. These are experimental, and using them gives warning FS0057 until it is suppressed.

In tests, an environment begins scopes that are fakes, in the way described under [Testing](testing.html): a fake
transaction can record whether it was completed.
*)
