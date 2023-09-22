namespace Orsak.Tests

open Xunit
open FSharp.Control
open Orsak
open System.Threading.Tasks
open System
open System.Threading
open System.Collections.Generic

[<AutoOpen>]
module Helpers2 =
    let runOrFail (e: Effect<_, _, _>) = e.RunOrFail()

    let evaluatesToSequence s (es: EffSeq<unit, 'a, string>) = task {
        let res = ResizeArray<_>()

        do!
            eff {
                for a in es do
                    res.Add(a)

                return ()
            }
            |> runOrFail

        let list = List.ofSeq res
        Assert.Equal<'a>(s, list)
    }

    let evaulateWithCancellation token (es: EffSeq<unit, 'a, string>) = task {
        let mutable hasRead = false
        do! Task.Yield()

        do!
            eff {
                use enumerator = es.Invoke().GetAsyncEnumerator(token)
                
                while enumerator.MoveNextAsync() do
                    match enumerator.Current with
                    | Ok _ -> hasRead <- true
                    | Error err -> failwith err

                Assert.True hasRead

                return ()
            }
            |> runOrFail
    }


    let expectError (error: 'err) (e: Effect<unit, _, 'err>) =
        e
        |> Effect.map (fun v -> failwith $"Got value %A{v} when expecting error %A{error}")
        |> Effect.tryRecover (fun es ->
            if es = error then
                Ok()
            else
                failwith $"Got error %O{es} when expecting error %O{error}")

    let evaluatesToSequenceThenFails (s) (error) (es: EffSeq<unit, 'a, string>) = task {
        let res = ResizeArray<_>()

        do!
            eff {
                for a in es do
                    res.Add(a)

                return ()
            }
            |> expectError error
            |> runOrFail

        let list = List.ofSeq res
        Assert.Equal<'a>(s, list)
    }

    let makeAsyncEnumerable (list: _ list) = taskSeq {
        for a in list do
            yield a
    }

    let infiniteAsyncEnumerable () = taskSeq {
        let mutable i = 1
        while true do
            i
            i <- i + 1
    }

    let makeEffSeq (list: _ list) = effSeq {
        for a in list do
            yield a
    }

    let makeEffSeqWithError (list: _ list) = effSeq {
        for a in list do
            yield a

        do! Error("Expected Error")

        for a in list do
            yield a
    }

    let inline withCancellation token (e: EffSeq<_, _, _>) = e.WithCancellation(token)

    type DisposeSpy() =
        [<DefaultValue(false)>]
        val mutable WasDisposed: bool

        interface IDisposable with
            member this.Dispose() = this.WasDisposed <- true

        interface IAsyncDisposable with
            member this.DisposeAsync() =
                this.WasDisposed <- true
                ValueTask.CompletedTask


module ``Empty Effect Sequences`` =
    [<Fact>]
    let ``should evaluate empty`` () =
        effSeq { do () } |> evaluatesToSequence []


    [<Fact>]
    let ``should evaluate empty repeatedly`` () = task {
        let s = effSeq { do () }
        do! evaluatesToSequence [] s
        do! evaluatesToSequence [] s
    }

    [<Fact>]
    let ``should work with While-form`` () =
        effSeq {
            let mutable i = 10

            while i > 0 do
                i <- i - 1
        }
        |> evaluatesToSequence []


    [<Fact>]
    let ``should work with For-form`` () =
        effSeq {
            for i in 1..10 do
                do ()
        }
        |> evaluatesToSequence []

    [<Fact>]
    let ``should work with For-form with IAsyncEnumerable`` () =
        effSeq {
            for i in (makeAsyncEnumerable []) do
                do ()
        }
        |> evaluatesToSequence []

    [<Fact>]
    let ``should work with For-form with IAsyncEnumerable and cancellation`` () = task {
        let cts = new CancellationTokenSource()

        let task =
            effSeq {
                for i in infiniteAsyncEnumerable () do
                    i
            }
            |> evaulateWithCancellation cts.Token

        do! Task.Delay(10)
        cts.Cancel()
        do! task
        return ()
    }

    [<Fact>]
    let ``should work with For-form with IAsyncEnumerable and inner cancellation`` () = task {
        let cts = new CancellationTokenSource()

        let task =
            effSeq {
                for i in infiniteAsyncEnumerable () do
                    i
            }
            |> withCancellation CancellationToken.None
            |> evaulateWithCancellation cts.Token

        do! Task.Delay(10)
        cts.Cancel()
        do! task
        return ()
    }

    [<Fact>]
    let ``should work with For-form with IAsyncEnumerable and outer cancellation`` () = task {
        let cts = new CancellationTokenSource()

        let task =
            effSeq {
                for i in infiniteAsyncEnumerable () do
                    i
            }
            |> withCancellation cts.Token
            |> evaulateWithCancellation CancellationToken.None

        do! Task.Delay(10)
        cts.Cancel()
        do! task
        return ()
    }

    [<Fact>]
    let ``should work with For-form with ConfiguredCancelableAsyncEnumerable`` () = task {
        let cts = new CancellationTokenSource()
        let chan = Channels.Channel.CreateUnbounded<int>()
        let enumerable = chan.Reader.ReadAllAsync().WithCancellation(cts.Token).ConfigureAwait(false)
        do! chan.Writer.WriteAsync(1)
        let task =
            effSeq {
                try
                    for i in enumerable do i
                with
                | :? AggregateException as agg ->
                    let inner = agg.InnerException
                    let t = Assert.IsType<TaskCanceledException> inner
                    Assert.Equal(cts.Token, t.CancellationToken)
                    ()
            }
            |> evaluatesToSequence [ 1 ]

        do! Task.Delay(10)
        cts.Cancel()
        do! task
        return ()
    }


    [<Fact>]
    let ``should work with For-form with EffSeq`` () =
        effSeq {
            for i in (makeEffSeq []) do
                do ()
        }
        |> evaluatesToSequence []

    [<Fact>]
    let ``should work with YieldFrom-form`` () =
        effSeq { yield! [] } |> evaluatesToSequence []

    [<Fact>]
    let ``should bind effects`` () =
        effSeq { do! eff { do! Task.Yield() } } |> evaluatesToSequence []

    [<Fact>]
    let ``should bind tasks`` () =
        effSeq { do! Task.Yield() } |> evaluatesToSequence []


    [<Fact>]
    let ``should bind task-likes`` () =
        effSeq { do! vtask { do! Task.Yield() } } |> evaluatesToSequence []

    [<Fact>]
    let ``should bind async`` () =
        effSeq { do! Async.AwaitTask(task { do! Task.Yield() }) }
        |> evaluatesToSequence []

    [<Fact>]
    let ``should bind results`` () =
        effSeq { do! Ok() } |> evaluatesToSequence []

    [<Fact>]
    let ``should propagate errors`` () =
        effSeq { do! Error("Expected Error") }
        |> evaluatesToSequenceThenFails [] "Expected Error"

    [<Fact>]
    let ``should propagate errors from effSeq`` () =
        effSeq {
            for i in (makeEffSeqWithError []) do
                do ()
        }
        |> evaluatesToSequenceThenFails [] "Expected Error"


    [<Fact>]
    let ``should work with TryWith-form`` () =
        effSeq {
            try
                do ()
            with _ ->
                ()
        }
        |> evaluatesToSequence []


    [<Fact>]
    let ``should catch exceptions with TryWith-form`` () =
        effSeq {
            let mutable caughtException = false

            try
                invalidArg "some arg" "my message"
            with :? ArgumentException as exn ->
                Assert.Equal("some arg", exn.ParamName)
                Assert.Equal("my message (Parameter 'some arg')", exn.Message)
                caughtException <- true

            Assert.True caughtException
        }
        |> evaluatesToSequence []

    [<Fact>]
    let ``should work with TryFinally-form`` () =
        effSeq {
            let mutable ranCompensation = false

            try
                do ()
            finally
                ranCompensation <- true

            Assert.True ranCompensation
        }
        |> evaluatesToSequence []

    [<Fact>]
    let ``should run compensation on exception with TryFinally-form`` () = task {
        let mutable ranCompensation = false

        let! exn =
            Assert.ThrowsAnyAsync<ArgumentException>(fun () ->
                effSeq {
                    try
                        invalidArg "some arg" "my message"
                    finally
                        ranCompensation <- true

                }
                |> evaluatesToSequence []
                :> Task)

        Assert.Equal("some arg", exn.ParamName)
        Assert.Equal("my message (Parameter 'some arg')", exn.Message)
        Assert.True ranCompensation
    }

    [<Fact>]
    let ``should work with use-from`` () =
        let spy = new DisposeSpy()

        effSeq {
            use _spy = (spy :> IDisposable)
            do ()
        }
        |> evaluatesToSequence []
        |> Task.map (fun () -> Assert.True spy.WasDisposed)


    [<Fact>]
    let ``should work with disposable-like`` () =
        let spy = new DisposeSpy()
        let spyEnumerator =
            { new IAsyncEnumerator<int> with
                  member this.Current = raise (System.NotImplementedException())
                  member this.DisposeAsync() = (spy :> IAsyncDisposable).DisposeAsync()
                  member this.MoveNextAsync() = vtask { return false } }

        let spyEnumerable =
            { new IAsyncEnumerable<int> with
                  member this.GetAsyncEnumerator(_) = spyEnumerator }

        let dispLike = spyEnumerable.WithCancellation(CancellationToken.None)

        effSeq {
            use _spy = dispLike.GetAsyncEnumerator()
            do ()
        }
        |> evaluatesToSequence []
        |> Task.map (fun () -> Assert.True spy.WasDisposed)

    [<Fact>]
    let ``should run compensation on exception with use-from`` () = task {
        let spy = new DisposeSpy()

        let! exn =
            Assert.ThrowsAnyAsync<ArgumentException>(fun () ->
                effSeq {
                    use _spy = (spy :> IDisposable)
                    invalidArg "some arg" "my message"
                }
                |> evaluatesToSequence []
                :> Task)

        Assert.Equal("some arg", exn.ParamName)
        Assert.Equal("my message (Parameter 'some arg')", exn.Message)
        Assert.True spy.WasDisposed
    }

    [<Fact>]
    let ``should work with use-from and IAsyncDisposable`` () =
        let spy = new DisposeSpy()

        effSeq {
            use _spy = (spy :> IAsyncDisposable)
            do ()
        }
        |> evaluatesToSequence []
        |> Task.map (fun () -> Assert.True spy.WasDisposed)

    [<Fact>]
    let ``should run compensation on exception with use-from and IAsyncDisposable`` () = task {
        let spy = new DisposeSpy()

        let! exn =
            Assert.ThrowsAnyAsync<ArgumentException>(fun () ->
                effSeq {
                    use _spy = (spy :> IAsyncDisposable)
                    invalidArg "some arg" "my message"
                }
                |> evaluatesToSequence []
                :> Task)

        Assert.Equal("some arg", exn.ParamName)
        Assert.Equal("my message (Parameter 'some arg')", exn.Message)
        Assert.True spy.WasDisposed
    }

    [<Fact>]
    let ``should disambiguate IDisposable and IAsyncDisposable`` () =
        let spy = new DisposeSpy()

        effSeq {
            use _spy = spy
            do ()
        }
        |> evaluatesToSequence []
        |> Task.map (fun () -> Assert.True spy.WasDisposed)

    [<Fact>]
    let ``should run all compensations on exception with use-from`` () = task {
        let spy1 = new DisposeSpy()
        let spy2 = new DisposeSpy()
        let spy3 = new DisposeSpy()
        let spy4 = new DisposeSpy()
        let spy5 = new DisposeSpy()

        let! exn =
            Assert.ThrowsAnyAsync<ArgumentException>(fun () ->
                effSeq {
                    use _spy = spy1
                    use _spy = spy2
                    use _spy = spy3
                    use _spy = spy4
                    use _spy = spy5

                    invalidArg "some arg" "my message"
                }
                |> evaluatesToSequence []
                :> Task)

        Assert.Equal("some arg", exn.ParamName)
        Assert.Equal("my message (Parameter 'some arg')", exn.Message)
        Assert.True spy1.WasDisposed
        Assert.True spy2.WasDisposed
        Assert.True spy3.WasDisposed
        Assert.True spy4.WasDisposed
        Assert.True spy5.WasDisposed
    }


module ``Effect Sequences With Elements`` =
    [<Fact>]
    let ``should yield elements`` () =
        effSeq {
            1
            2
            3
        }
        |> evaluatesToSequence [ 1; 2; 3 ]


    [<Fact>]
    let ``should yield elements repeatedly`` () = task {
        let s = effSeq {
            1
            2
            3
        }

        do! evaluatesToSequence [ 1; 2; 3 ] s
        do! evaluatesToSequence [ 1; 2; 3 ] s
    }

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(10)>]
    [<InlineData(100)>]
    let ``should work with While-form`` (size: int) =
        effSeq {
            let mutable i = size

            while i > 0 do
                yield i
                i <- i - 1
        }
        |> evaluatesToSequence [ size .. -1 .. 1 ]


    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(10)>]
    [<InlineData(100)>]
    [<InlineData(1000)>]
    [<InlineData(10_000)>]
    let ``should work with For-form`` (size: int) =
        effSeq {
            for i in 1..size do
                yield i
        }
        |> evaluatesToSequence [ 1..size ]

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(10)>]
    [<InlineData(100)>]
    let ``should work with For-form with IAsyncEnumerable`` (size: int) =
        effSeq {
            for i in (makeAsyncEnumerable [ 1..size ]) do
                i
        }
        |> evaluatesToSequence [ 1..size ]

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(10)>]
    [<InlineData(100)>]
    let ``should work with For-form with EffSeq`` (size: int) =
        effSeq {
            for i in (makeEffSeq [ 1..size ]) do
                i
        }
        |> evaluatesToSequence [ 1..size ]

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(10)>]
    [<InlineData(100)>]
    let ``should work with YieldFrom-form`` (size: int) =
        effSeq { yield! [ 1..size ] } |> evaluatesToSequence [ 1..size ]

    [<Fact>]
    let ``should bind effects`` () =
        effSeq {
            1
            do! eff { do! Task.Yield() }
            2
        }
        |> evaluatesToSequence [ 1; 2 ]

    [<Fact>]
    let ``should bind tasks`` () =
        effSeq {
            1
            do! Task.Yield()
            2
        }
        |> evaluatesToSequence [ 1; 2 ]


    [<Fact>]
    let ``should bind task-likes`` () =
        effSeq {
            1
            do! vtask { do! Task.Yield() }
            2
        }
        |> evaluatesToSequence [ 1; 2 ]

    [<Fact>]
    let ``should bind async`` () =
        effSeq {
            1
            do! Async.AwaitTask(task { do! Task.Yield() })
            2
        }
        |> evaluatesToSequence [ 1; 2 ]

    [<Fact>]
    let ``should bind results`` () =
        effSeq {
            1
            do! Ok()
            2
        }
        |> evaluatesToSequence [ 1; 2 ]

    [<Fact>]
    let ``should propagate errors`` () =
        effSeq {
            1
            do! Error("Expected Error")
            2
        }
        |> evaluatesToSequenceThenFails [ 1 ] "Expected Error"

    [<Fact>]
    let ``should propagate errors from effSeq`` () =
        effSeq {
            for i in (makeEffSeqWithError [ 1 ]) do
                i
        }
        |> evaluatesToSequenceThenFails [ 1 ] "Expected Error"


    [<Fact>]
    let ``should work with TryWith-form`` () =
        effSeq {
            try
                1
                2
                3
            with _ ->
                ()
        }
        |> evaluatesToSequence [ 1; 2; 3 ]


    [<Fact>]
    let ``should catch exceptions with TryWith-form`` () =
        effSeq {
            let mutable caughtException = false

            try
                1
                2
                3
                invalidArg "some arg" "my message"
                4
                5
                6
            with :? ArgumentException as exn ->
                Assert.Equal("some arg", exn.ParamName)
                Assert.Equal("my message (Parameter 'some arg')", exn.Message)
                caughtException <- true

            Assert.True caughtException
        }
        |> evaluatesToSequence [ 1; 2; 3 ]

    [<Fact>]
    let ``should work with TryFinally-form`` () =
        effSeq {
            let mutable ranCompensation = false

            try
                yield! [ 1; 2; 3 ]
            finally
                ranCompensation <- true

            Assert.True ranCompensation
        }
        |> evaluatesToSequence [ 1; 2; 3 ]

    [<Fact>]
    let ``should run compensation on exception with TryFinally-form`` () = task {
        let mutable ranCompensation = false

        let! exn =
            Assert.ThrowsAnyAsync<ArgumentException>(fun () ->
                effSeq {
                    try
                        yield! [ 1; 2; 3 ]
                        invalidArg "some arg" "my message"
                        yield! [ 1; 2; 3 ]
                    finally
                        ranCompensation <- true

                }
                |> evaluatesToSequence [ 1; 2; 3 ]
                :> Task)

        Assert.Equal("some arg", exn.ParamName)
        Assert.Equal("my message (Parameter 'some arg')", exn.Message)
        Assert.True ranCompensation
    }

    [<Fact>]
    let ``should work with use-from`` () =
        let spy = new DisposeSpy()

        effSeq {
            use _spy = (spy :> IDisposable)
            yield! [ 1; 2; 3 ]
        }
        |> evaluatesToSequence [ 1; 2; 3 ]
        |> Task.map (fun () -> Assert.True spy.WasDisposed)


    [<Fact>]
    let ``should work with disposable-like`` () =
        let spy = new DisposeSpy()
        let spyEnumerator =
            { new IAsyncEnumerator<int> with
                  member this.Current = raise (System.NotImplementedException())
                  member this.DisposeAsync() = (spy :> IAsyncDisposable).DisposeAsync()
                  member this.MoveNextAsync() = vtask { return false } }

        let spyEnumerable =
            { new IAsyncEnumerable<int> with
                  member this.GetAsyncEnumerator(_) = spyEnumerator }

        let dispLike = spyEnumerable.WithCancellation(CancellationToken.None)

        effSeq {
            use _spy = dispLike.GetAsyncEnumerator()
            1
            2
            3
            do ()
        }
        |> evaluatesToSequence [ 1; 2; 3; ]
        |> Task.map (fun () -> Assert.True spy.WasDisposed)

    [<Fact>]
    let ``should run compensation on exception with use-from`` () = task {
        let spy = new DisposeSpy()

        let! exn =
            Assert.ThrowsAnyAsync<ArgumentException>(fun () ->
                effSeq {
                    use _spy = (spy :> IDisposable)
                    yield! [ 1; 2; 3 ]
                    invalidArg "some arg" "my message"
                    yield! [ 1; 2; 3 ]
                }
                |> evaluatesToSequence [ 1; 2; 3 ]
                :> Task)

        Assert.Equal("some arg", exn.ParamName)
        Assert.Equal("my message (Parameter 'some arg')", exn.Message)
        Assert.True spy.WasDisposed
    }


    [<Fact>]
    let ``should work with use-from and IAsyncDisposable`` () =
        let spy = new DisposeSpy()

        effSeq {
            use _spy = (spy :> IAsyncDisposable)
            yield! [ 1; 2; 3 ]
        }
        |> evaluatesToSequence [ 1; 2; 3 ]
        |> Task.map (fun () -> Assert.True spy.WasDisposed)

    [<Fact>]
    let ``should run compensation on exception with use-from and IAsyncDisposable`` () = task {
        let spy = new DisposeSpy()

        let! exn =
            Assert.ThrowsAnyAsync<ArgumentException>(fun () ->
                effSeq {
                    use _spy = (spy :> IAsyncDisposable)
                    yield! [ 1; 2; 3 ]
                    invalidArg "some arg" "my message"
                    yield! [ 1; 2; 3 ]
                }
                |> evaluatesToSequence [ 1; 2; 3 ]
                :> Task)

        Assert.Equal("some arg", exn.ParamName)
        Assert.Equal("my message (Parameter 'some arg')", exn.Message)
        Assert.True spy.WasDisposed
    }

    [<Fact>]
    let ``should disambiguate IDisposable and IAsyncDisposable`` () =
        let spy = new DisposeSpy()

        effSeq {
            use _spy = spy
            yield! [ 1; 2; 3 ]
        }
        |> evaluatesToSequence [ 1; 2; 3 ]
        |> Task.map (fun () -> Assert.True spy.WasDisposed)

    [<Fact>]
    let ``should run all compensations on exception with use-from`` () = task {
        let spy1 = new DisposeSpy()
        let spy2 = new DisposeSpy()
        let spy3 = new DisposeSpy()
        let spy4 = new DisposeSpy()
        let spy5 = new DisposeSpy()

        let! exn =
            Assert.ThrowsAnyAsync<ArgumentException>(fun () ->
                effSeq {
                    use _spy = spy1
                    use _spy = spy2
                    use _spy = spy3
                    use _spy = spy4
                    use _spy = spy5
                    yield! [ 1; 2; 3 ]
                    invalidArg "some arg" "my message"
                    yield! [ 1; 2; 3 ]
                }
                |> evaluatesToSequence [ 1; 2; 3 ]
                :> Task)

        Assert.Equal("some arg", exn.ParamName)
        Assert.Equal("my message (Parameter 'some arg')", exn.Message)
        Assert.True spy1.WasDisposed
        Assert.True spy2.WasDisposed
        Assert.True spy3.WasDisposed
        Assert.True spy4.WasDisposed
        Assert.True spy5.WasDisposed
    }
