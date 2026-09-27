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
    interface ExceptionHandler<string> with
        member _.Handle e = e.Message

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
    let transaction_rolls_back_and_fails_with_the_handled_error_when_its_effect_throws () = task {
        let! failed =
            eff {
                do! GrowingTransaction.insert "a"
                failwith "thrown"
            }
            |> transaction
            |> run

        let! result = committed () |> run
        failed =! Error "thrown"
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
