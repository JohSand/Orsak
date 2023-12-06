namespace Orsak.Tests

open System
open System.Security.Cryptography
open Microsoft.FSharp.NativeInterop
open System.Runtime.InteropServices

//#nowarn "9"
#nowarn "57"

open System.Data.Common
open FSharp.Control
open Microsoft.Data.Sqlite

open Orsak
open Orsak.Effects
open Orsak.ScopeAware
open Orsak.Scoped
open Xunit
open Swensen.Unquote

type RNGProvider =
    abstract member Gen: RandomNumberGenerator

module RandomNumberGenerator =
    let getBytes (x: byte array) =

        Effect.Create(fun (provider: #RNGProvider) -> provider.Gen.GetBytes(x))

type IClock =
    abstract member UtcNow: unit -> DateTimeOffset

type IClockProvider =
    abstract member Clock: IClock

module Clock =
    let utcNow () =
        Effect.Create(fun (provider: #IClockProvider) -> provider.Clock.UtcNow())

type TestProvider() =
    let rng = RandomNumberGenerator.Create()

    interface ExceptionHandler<string> with
        member this.Handle e = e.ToString()

    interface IClockProvider with
        member this.Clock =
            { new IClock with
                member this.UtcNow() = DateTimeOffset.UtcNow
            }

    interface RNGProvider with
        member this.Gen: RandomNumberGenerator = rng

    interface CompletableScopeProvider<DbTransactional> with
        member this.BeginScope() = vtask {
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
            """CREATE TABLE IF NOT EXISTS TestData (
	              pkey INTEGER PRIMARY KEY AUTOINCREMENT,
	              data TEXT NOT NULL
                );"""

        command.ExecuteNonQuery() |> ignore
        tran.Commit()
        ()

    let scopeAware = ScopeAwareEffectBuilder<DbTransactional>()

    //Mostly explores what is possible with this api
    //all the async usage is wasted with SQLite, but can be valuable with other providers.
    let read () : Transaction<_, _> =
        mkEffect (fun (trans: DbTransactional) -> vtask {
            let command = trans.Connection.CreateCommand()

            command.CommandText <- "SELECT pkey, data FROM TestData"

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


    let readSingle () : EnlistedTransaction<_,_,_> = scopeAware {
        let! _now = Clock.utcNow ()

        let! list = read ()
        let value = Assert.Single(list)
        Assert.Equal((1, "test"), value)
        return ()
    }

    let readEmpty () = commitEff {
        let! list = read ()
        Assert.Empty list
        return ()
    }

    let readAndCommit () = commitEff { return! read () }

    let readThenFail () = scopeAware {
        let! _now = Clock.utcNow ()
        let! list = read ()
        let value = Assert.Single(list)
        Assert.Equal((1, "test"), value)
        return! Error "Something went wrong"
    }

    let insert () : Transaction<unit, _> =
        mkEffect (fun (trans: DbTransactional) -> vtask {
            let command = trans.Connection.CreateCommand()
            command.CommandText <- """INSERT INTO TestData (data) VALUES (@data)"""
            let par = command.CreateParameter()
            par.ParameterName <- "@data"
            par.Value <- "test"
            command.Parameters.Add par |> ignore
            command.ExecuteNonQuery() |> ignore
            return Ok()
        })

    let scopedInsert () = scopeAware {
        do! insert ()
        return ()
    }

    let insertWithError () = commitEff {
        do! insert ()
        let! list = read ()
        let value = Assert.Single(list)
        Assert.Equal((1, "test"), value)
        return! Error "expected"
    }

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
    let ``We can bind a scoped effect and then a regular effect with proper overload resolution`` () =
        commitEff {
            let! list = read ()

            let! _now = Clock.utcNow ()

            Assert.Empty(list)
            return ()
        }
        |> run

    [<Fact>]
    let ``We can bind a regular effect and then a scoped effect with proper overload resolution`` () =
        commitEff {
            let! _now = Clock.utcNow ()
            let! list = read ()
            Assert.Empty(list)
            return ()
        }
        |> run

    [<Fact>]
    let ``We can interleave effects in a commitEff with proper overload resolution`` () =
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
    let ``A failed commitEff does not execute its commit`` () =
        eff {
            do! insertWithError () |> expectError "expected"

            let! list = readAndCommit ()

            Assert.Empty list
            return ()
        }
        |> run


    [<Fact>]
    let ``We can see the scoped results in commitEff`` () =
        commitEff {
            do! insert ()

            let! list = read ()
            let value = Assert.Single(list)
            Assert.Equal((1, "test"), value)
            return! Clock.utcNow ()
        }
        |> run

    [<Fact>]
    let ``A failed commitEff rolls back its scope`` () =

        commitEff {
            do! scopedInsert ()

            let! list = read ()

            let value = Assert.Single(list)
            Assert.Equal((1, "test"), value)
            return! Error "Error"
        }
        |> expectError "Error"
        |> Effect.bind (readEmpty)
        |> run


    [<Fact>]
    let ``Scope aware effects read with the surrounding scope`` () =
        commitEff {
            do! insert ()
            do! readSingle ()
            let! _now = Clock.utcNow ()
            return 1
        }
        |> run

    [<Fact>]
    let ``Scope aware effects can fail the surrounding scope`` () =
        commitEff {
            do! insert ()
            do! readThenFail ()
            let! _now = Clock.utcNow ()
            return ()
        }
        |> Effect.tryRecover (fun es ->
            if es = "Something went wrong" then
                Ok()
            else
                Error $"Got error %O{es} when expecting error Something went wrong")
        |> Effect.bind readEmpty
        |> run

    [<Fact>]
    let ``Scope aware effects effect with the surrounding scope`` () =
        commitEff {
            let! list = read ()
            Assert.Empty(list)
            do! scopedInsert ()
            let! list = read ()
            let value = Assert.Single(list)
            Assert.Equal((1, "test"), value)
            return ()
        }
        |> run


    [<Fact>]
    let ``Each commitEff should have their own transaction`` () =
        let insertAndRead () = commitEff {
            let! list = read ()
            Assert.True(3 > list.Count)
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
