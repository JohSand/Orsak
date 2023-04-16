namespace Orsak.Tests

open System

#nowarn "57"

open System.Data.Common
open System.Threading.Tasks
open FSharp.Control
open Microsoft.Data.Sqlite

open Orsak
open Orsak.Scoped
open Xunit

type IClock =
    abstract member UtcNow: unit -> System.DateTimeOffset

type IClockProvider =
    abstract member Clock: IClock

type Clock =
    static member utcNow() =
        Effect.Create(fun (provider: #IClockProvider) -> provider.Clock.UtcNow())



type DbTransactional(tran: DbTransaction) =
    member val Connection = tran.Connection

    interface TransactionScope with
        member this.CommitAsync() : Task = tran.CommitAsync()

        member this.DisposeAsync() : ValueTask = vtask {
            do! tran.DisposeAsync()
            do! this.Connection.DisposeAsync()
        }

type TestProvider() =
    interface ExceptionHandler<string> with
        member this.Handle e = e.ToString()

    interface IClockProvider with
        member this.Clock =
            { new IClock with
                member this.UtcNow() = System.DateTimeOffset.UtcNow
            }

    interface TransactionScopeProvider<DbTransactional> with
        member this.Scope() : ValueTask<DbTransactional> = vtask {
            let conn: DbConnection = new SqliteConnection("Data Source=test.db;Cache=Shared")
            do! conn.OpenAsync()
            let! tran = conn.BeginTransactionAsync()
            return DbTransactional(tran)
        }

type Scoped() =
    do
        use conn = new SqliteConnection("Data Source=test.db;Cache=Shared")
        conn.Open()
        use tran = conn.BeginTransaction()
        let command = conn.CreateCommand()

        command.CommandText <-
            """
                                CREATE TABLE TestData (
	                                pkey INTEGER PRIMARY KEY AUTOINCREMENT,
	                                data TEXT NOT NULL
                                );"""

        command.ExecuteNonQuery() |> ignore
        tran.Commit()
        ()


    let commitEff = TransactionalEffectBuilder<DbTransactional>()

    //Mostly explores what is possible with this api
    //all the async usage is wasted with SQLite, but can be valuable with other providers.
    let read () =
        mkEffect (fun (trans: DbTransactional) -> vtask {
            let command = trans.Connection.CreateCommand()

            command.CommandText <-
                """
                    SELECT pkey, data FROM TestData
                """

            let list = ResizeArray()

            try
                use! reader = command.ExecuteReaderAsync()

                while reader.ReadAsync() do
                    let id = reader.GetInt32(0)
                    let data = reader.GetString(1)
                    list.Add((id, data))

                return Ok list
            with e ->
                return Error(e.ToString())
        })

    let insert () =
        mkEffect (fun (trans: DbTransactional) -> vtask {
            let command = trans.Connection.CreateCommand()

            command.CommandText <-
                """
                    INSERT INTO TestData (data) VALUES (@data)
                """

            let par = command.CreateParameter()
            par.ParameterName <- "@data"
            par.Value <- "test"
            command.Parameters.Add par |> ignore
            command.ExecuteNonQuery() |> ignore
            return Ok()
        })

    let run e =
        Effect.runOrFail<TestProvider, _, string> (TestProvider()) e

    let expectError (error: 'err) (e: Effect<_, _, 'err>) =
        e
        |> Effect.map (fun v -> failwith $"Got value %A{v} when expecting error %A{error}")
        |> Effect.tryRecover (fun es ->
            if es = error then
                Ok()
            else
                failwith $"Got error %O{es} when expecting error %O{error}")

    [<Fact>]
    let ``Connection tests`` () =
        commitEff {
            let! list = read ()
            Assert.Empty list
            let! _now = Clock.utcNow ()
            return 1
        }
        |> run

    [<Fact>]
    let ``Connection tests 2`` () =
        commitEff {
            do! insert ()
            let! _now = Clock.utcNow ()
            let! list = read ()
            let value = Assert.Single(list)
            Assert.Equal((1, "test"), value)
            return 1
        }
        |> run


    [<Fact>]
    let ``Connection tests 3`` () =
        let insertWithError () = commitEff {
            do! insert ()
            let! list = read ()
            let value = Assert.Single(list)
            Assert.Equal((1, "test"), value)
            return! Error "expected"
        }

        let read () = commitEff { return! read () }

        eff {
            do! insertWithError () |> expectError "expected"

            let! list = read ()

            Assert.Empty list
            return ()
        }
        |> run


    [<Fact>]
    let ``Connection tests 4`` () =
        commitEff {
            do! insert ()

            let! list = read ()
            let value = Assert.Single(list)
            Assert.Equal((1, "test"), value)
            return! Clock.utcNow ()
        }
        |> run

    [<Fact>]
    let ``Connection tests 5`` () =
        let insertAndRead () = commitEff {
            do! insert ()
            let! list = read ()
            return list.Count
        }

        eff {
            let! a = insertAndRead ()

            and! b = insertAndRead ()

            Assert.Equal(3, a + b)

            return ()
        }
        |> run

    interface IDisposable with
        member this.Dispose() : unit =
            use conn = new SqliteConnection("Data Source=test.db;Cache=Shared")
            conn.Open()
            use tran = conn.BeginTransaction()
            let command = conn.CreateCommand()
            command.CommandText <- "DROP TABLE IF EXISTS TestData;"
            command.ExecuteNonQuery() |> ignore
            tran.Commit()
