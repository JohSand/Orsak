namespace Orsak.Tests

#nowarn "57"

open System
open System.Data.Common
open System.Threading.Tasks
open FSharp.Control
open Microsoft.Data.Sqlite

open Orsak
open Orsak.Effects
open Orsak.Scoped
open Xunit
open Swensen.Unquote

// Scopes whose effects run in an environment grown from the outer one. The effects are ordinary effects, bound in
// plain eff: those using the transaction ask for it with a provider interface, which only the grown environment has.

type ITransactionProvider =
    abstract member Transaction: DbTransactional

module GrowingTransaction =
    let connectionString = "Data Source=growing-scope-tests.db;Cache=Shared"

    let read () =
        mkEffect (fun (p: #ITransactionProvider) -> vtask {
            let command = p.Transaction.Connection.CreateCommand()
            command.CommandText <- "SELECT pkey, data FROM GrowingData"
            let list = ResizeArray()
            use! reader = command.ExecuteReaderAsync()

            while reader.ReadAsync() do
                list.Add((reader.GetInt32(0), reader.GetString(1)))

            return Ok(List.ofSeq list)
        })

    let insert (data: string) =
        mkEffect (fun (p: #ITransactionProvider) -> vtask {
            let command = p.Transaction.Connection.CreateCommand()
            command.CommandText <- "INSERT INTO GrowingData (data) VALUES (@data)"
            let par = command.CreateParameter()
            par.ParameterName <- "@data"
            par.Value <- data
            command.Parameters.Add par |> ignore
            let! _ = command.ExecuteNonQueryAsync()
            return Ok()
        })

/// The environment of the tests.
type GrowingTestEnv() =
    interface IClockProvider with
        member _.Clock =
            { new IClock with
                member _.UtcNow() = DateTimeOffset.UtcNow
            }

    interface CompletableScopeProvider<DbTransactional> with
        member _.BeginScope() = vtask {
            let conn: DbConnection = new SqliteConnection(GrowingTransaction.connectionString)
            do! conn.OpenAsync()
            let! tran = conn.BeginTransactionAsync()
            return DbTransactional(tran)
        }

    interface IGrowsInto<DbTransactional, GrowingTransactionEnv> with
        member _.Grow transaction = GrowingTransactionEnv(transaction)

/// The environment of the tests in a transaction: everything the outer environment has, and the transaction.
and GrowingTransactionEnv(transaction: DbTransactional) =
    inherit GrowingTestEnv()

    interface ITransactionProvider with
        member _.Transaction = transaction

type IProgress =
    abstract member Report: float -> unit

type IProgressScope =
    inherit Scoped
    abstract member Progress: IProgress

type IProgressProvider =
    abstract member Progress: IProgress

module GrowingProgress =
    let report x =
        Effect.Create(fun (p: #IProgressProvider) -> p.Progress.Report x)

    /// A generic combinator, not inline: runs the effects in a progress scope, reporting as each completes.
    let whenAll (effects: Effect<'r, 'a, 'e> array) =
        eff {
            let! results =
                effects
                |> Array.mapi (fun i e -> eff {
                    let! result = e
                    do! report (float (i + 1) / float effects.Length)
                    return result
                })
                |> Effect.whenAll

            return results
        }
        |> Scope<IProgressScope>.Within

type ProgressScope(reports: ResizeArray<float>) =
    let progress =
        { new IProgress with
            member _.Report x = lock reports (fun () -> reports.Add x)
        }

    member val Disposed = false with get, set

    interface IProgressScope with
        member _.Progress = progress
        member this.DisposeAsync() =
            this.Disposed <- true
            ValueTask()

type ProgressTestEnv(scope: ProgressScope) =
    member _.Scope = scope

    interface IClockProvider with
        member _.Clock =
            { new IClock with
                member _.UtcNow() = DateTimeOffset.UnixEpoch
            }

    interface ScopeProvider<IProgressScope> with
        member _.BeginScope() = ValueTask<IProgressScope>(scope)

    interface IGrowsInto<IProgressScope, ProgressScopeEnv> with
        member this.Grow scope = ProgressScopeEnv(this, scope)

and ProgressScopeEnv(outer: ProgressTestEnv, scope: IProgressScope) =
    inherit ProgressTestEnv(outer.Scope)

    interface IProgressProvider with
        member _.Progress = scope.Progress

/// A completable scope recording what happens to it, numbered in the order the scopes are begun.
type RecordingScope(number: int, log: ResizeArray<string>) =
    member _.Number = number

    interface CompletableScope with
        member _.Complete() =
            log.Add $"complete {number}"
            ValueTask()

        member _.DisposeAsync() =
            log.Add $"dispose {number}"
            ValueTask()

type IRecordingScopeProvider =
    abstract member Scope: RecordingScope

type RecordingEnv(log: ResizeArray<string>) =
    let mutable begun = 0

    interface CompletableScopeProvider<RecordingScope> with
        member _.BeginScope() =
            begun <- begun + 1
            log.Add $"begin {begun}"
            ValueTask<RecordingScope>(RecordingScope(begun, log))

    interface IGrowsInto<RecordingScope, RecordingScopeEnv> with
        member this.Grow scope = RecordingScopeEnv(this, scope)

and RecordingScopeEnv(outer: RecordingEnv, scope: RecordingScope) =
    interface CompletableScopeProvider<RecordingScope> with
        member _.BeginScope() = (outer :> CompletableScopeProvider<RecordingScope>).BeginScope()

    interface IGrowsInto<RecordingScope, RecordingScopeEnv> with
        member _.Grow scope = (outer :> IGrowsInto<RecordingScope, RecordingScopeEnv>).Grow scope

    interface IRecordingScopeProvider with
        member _.Scope = scope

type GrowingScopeTests() =
    do
        use conn = new SqliteConnection(GrowingTransaction.connectionString)
        conn.Open()
        let command = conn.CreateCommand()

        command.CommandText <-
            """DROP TABLE IF EXISTS GrowingData;
               CREATE TABLE GrowingData (pkey INTEGER PRIMARY KEY AUTOINCREMENT, data TEXT NOT NULL);"""

        command.ExecuteNonQuery() |> ignore

    let transaction body =
        CompletingScope<DbTransactional>.Within body

    let run (e: Effect<GrowingTestEnv, 'a, string>) = task {
        let! result = e |> Effect.run (GrowingTestEnv())
        return result
    }

    let committed () = transaction (GrowingTransaction.read ())

    [<Fact(Timeout = 10_000)>]
    let transaction_binds_transactional_and_other_effects_in_any_order () = task {
        let! result =
            eff {
                let! _ = Clock.utcNow ()
                let! empty = GrowingTransaction.read ()
                do! GrowingTransaction.insert "a"
                let! _ = Clock.utcNow ()
                let! one = GrowingTransaction.read ()
                return empty, one
            }
            |> transaction
            |> run

        result =! Ok([], [ 1, "a" ])
    }

    [<Fact(Timeout = 10_000)>]
    let transaction_commits_when_its_effect_succeeds () = task {
        let! _ = transaction (GrowingTransaction.insert "a") |> run
        let! result = committed () |> run
        result =! Ok [ 1, "a" ]
    }

    [<Fact(Timeout = 10_000)>]
    let transaction_rolls_back_when_its_effect_fails () = task {
        let! failed =
            eff {
                do! GrowingTransaction.insert "a"
                return! Error "failed"
            }
            |> transaction
            |> run

        let! result = committed () |> run
        failed =! Error "failed"
        result =! Ok []
    }

    [<Fact(Timeout = 10_000)>]
    let transaction_rolls_back_and_rethrows_when_its_effect_throws () = task {
        let throwing: Effect<GrowingTransactionEnv, unit, string> = eff {
            do! GrowingTransaction.insert "a"
            failwith "thrown"
        }

        let! thrown = Assert.ThrowsAnyAsync<exn>(fun () -> throwing |> transaction |> run :> Task)
        let! result = committed () |> run
        thrown.Message =! "thrown"
        result =! Ok []
    }

    [<Fact(Timeout = 10_000)>]
    let transactional_effects_compose_as_plain_effects () = task {
        // no scope-aware builder: a function of transactional effects is an ordinary effect
        let insertAndCount data = eff {
            do! GrowingTransaction.insert data
            let! rows = GrowingTransaction.read ()
            return rows.Length
        }

        let! result =
            eff {
                let! first = insertAndCount "a"
                let! second = insertAndCount "b"
                return first, second
            }
            |> transaction
            |> run

        result =! Ok(1, 2)
    }

    [<Fact(Timeout = 10_000)>]
    let each_transaction_is_its_own_scope () = task {
        let! failed =
            eff {
                do! transaction (GrowingTransaction.insert "a")

                do! transaction (eff {
                    do! GrowingTransaction.insert "b"
                    return! Error "second failed"
                })
            }
            |> run

        let! result = committed () |> run
        failed =! Error "second failed"
        result =! Ok [ 1, "a" ]
    }

    [<Fact(Timeout = 10_000)>]
    let transaction_commits_the_writes_of_all_its_effects_together () = task {
        let insertOrder = GrowingTransaction.insert "order"
        let insertLines = eff {
            do! GrowingTransaction.insert "line 1"
            do! GrowingTransaction.insert "line 2"
        }

        let! placed =
            eff {
                do! insertOrder
                do! insertLines
            }
            |> transaction
            |> run

        let! result = committed () |> run
        placed =! Ok()
        result =! Ok [ 1, "order"; 2, "line 1"; 3, "line 2" ]
    }

    [<Fact(Timeout = 10_000)>]
    let transaction_rolls_back_the_writes_of_all_its_effects_when_a_later_one_fails () = task {
        let insertOrder = GrowingTransaction.insert "order"
        let insertLines = eff {
            do! GrowingTransaction.insert "line 1"
            do! GrowingTransaction.insert "line 2"
        }
        let checkStock = eff { return! Error "out of stock" }

        let! placed =
            eff {
                do! insertOrder
                do! insertLines
                do! checkStock
            }
            |> transaction
            |> run

        let! result = committed () |> run
        placed =! Error "out of stock"
        result =! Ok []
    }

    [<Fact(Timeout = 10_000)>]
    let transaction_rolls_back_earlier_writes_when_an_effect_deep_in_a_helper_fails () = task {
        let insertOrder = GrowingTransaction.insert "order"

        let reserve item = eff {
            do! GrowingTransaction.insert item
            let! rows = GrowingTransaction.read ()

            if rows.Length > 2 then
                return! Error $"cannot reserve {item}"
        }

        let reserveAll = eff {
            do! reserve "a"
            do! reserve "b"
        }

        let! placed =
            eff {
                do! insertOrder
                do! reserveAll
            }
            |> transaction
            |> run

        let! result = committed () |> run
        placed =! Error "cannot reserve b"
        result =! Ok []
    }

    [<Fact(Timeout = 10_000)>]
    let transaction_commits_writes_of_effects_bound_together_with_and () = task {
        let! placed =
            eff {
                let! () = GrowingTransaction.insert "a"
                and! () = GrowingTransaction.insert "b"
                return ()
            }
            |> transaction
            |> run

        let! result = committed () |> run
        placed =! Ok()
        test <@ result |> Result.map (List.map snd >> List.sort) = Ok [ "a"; "b" ] @>
    }

    [<Fact(Timeout = 10_000)>]
    let a_nested_transaction_is_its_own_scope_and_completes_even_when_the_outer_one_fails () = task {
        // nested transactions don't join the outer one: the inner one is completed on its own
        let log = ResizeArray()

        let scopeNumber () =
            Effect.Create(fun (p: #IRecordingScopeProvider) -> p.Scope.Number)

        let! result =
            eff {
                let! outer = scopeNumber ()
                let! inner = CompletingScope<RecordingScope>.Within(scopeNumber ())
                return! Error $"outer {outer} failed after inner {inner}"
            }
            |> CompletingScope<RecordingScope>.Within
            |> Effect.run (RecordingEnv log)

        result =! Error "outer 1 failed after inner 2"
        List.ofSeq log =! [ "begin 1"; "begin 2"; "complete 2"; "dispose 2"; "dispose 1" ]
    }

    [<Fact(Timeout = 10_000)>]
    let a_completing_scope_is_disposed_without_completing_when_its_effect_throws () = task {
        let log = ResizeArray()
        let throwing: Effect<RecordingScopeEnv, unit, string> = eff { failwith "thrown" }

        let! thrown =
            Assert.ThrowsAnyAsync<exn>(fun () ->
                throwing
                |> CompletingScope<RecordingScope>.Within
                |> Effect.run (RecordingEnv log)
                |> _.AsTask()
                :> Task)

        thrown.Message =! "thrown"
        List.ofSeq log =! [ "begin 1"; "dispose 1" ]
    }

    [<Fact(Timeout = 10_000)>]
    let withEnvironment_runs_the_effect_with_the_derived_environment () = task {
        let length: Effect<string, int, string> = Effect.Create(fun (s: string) -> s.Length)
        let! result = length |> Effect.withEnvironment (fun (n: int) -> String('x', n)) |> Effect.run 3
        result =! Ok 3
    }

    [<Fact(Timeout = 10_000)>]
    let withEnvironment_keeps_the_error_of_the_effect () = task {
        let failing: Effect<string, int, string> = eff { return! Error "failed" }
        let! result = failing |> Effect.withEnvironment string |> Effect.run 3
        result =! Error "failed"
    }

    [<Fact(Timeout = 10_000)>]
    let a_transaction_runs_an_effect_typed_against_the_outer_environment_by_upcasting () = task {
        // typed against the outer environment, so it can't be bound in the transaction's body as it is
        let now: Effect<GrowingTestEnv, DateTimeOffset, string> = Clock.utcNow ()

        let! result =
            eff {
                do! GrowingTransaction.insert "a"
                let! now = now |> Effect.withEnvironment (fun env -> upcast env)
                let! rows = GrowingTransaction.read ()
                return now <= DateTimeOffset.UtcNow, rows.Length
            }
            |> transaction
            |> run

        result =! Ok(true, 1)
    }

    [<Fact(Timeout = 10_000)>]
    let a_generic_combinator_opens_a_scope_for_the_effects_it_is_given () = task {
        let reports = ResizeArray()
        let scope = ProgressScope(reports)

        let! result =
            GrowingProgress.whenAll [| Clock.utcNow (); Clock.utcNow () |]
            |> Effect.run (ProgressTestEnv scope)

        result =! Ok [| DateTimeOffset.UnixEpoch; DateTimeOffset.UnixEpoch |]
        List.sort (List.ofSeq reports) =! [ 0.5; 1.0 ]
        test <@ scope.Disposed @>
    }

    interface IDisposable with
        member _.Dispose() =
            SqliteConnection.ClearAllPools()
