namespace Orsak.Tests

open Xunit
open FSharp.Control
open Orsak
open System.Threading.Tasks
open System
open System.Threading
open System.Collections.Generic
open Swensen.Unquote

[<AutoOpen>]
module Helpers2 =
    let runOrFail (e: Effect<_, _, _>) = e.RunOrFail()

    let evaluatesToSequence (s: 'a list) (es: EffSeq<unit, 'a, string>) = task {
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

    let evaluateWithCancellation token (es: EffSeq<unit, 'a, string>) = task {
        let mutable hasRead = false

        do!
            eff {
                use enumerator = es.Invoke().GetAsyncEnumerator(token)

                while! enumerator.MoveNextAsync() do
                    match enumerator.Current with
                    | Ok _ -> hasRead <- true
                    | Error err -> failwith err

                    do! Task.Yield()

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
                ValueTask(
                    task = task {
                        this.WasDisposed <- true
                        do! Task.Yield()
                    }
                )

    type IRand =
        abstract member Next: unit -> int

    type IRandProvider =
        abstract member Rand: IRand

    let next () =
        Effect.Create(fun (p: #IRandProvider) -> p.Rand.Next())

    type RandProvider(seed: int) =
        let inner = Random(seed)

        interface IRandProvider with
            member _.Rand =
                { new IRand with
                    member _.Next() = inner.Next()
                }


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
            |> evaluateWithCancellation cts.Token

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
    let ``should propagate errors repeatedly`` () = task {
        let s = effSeq { do! Error("Expected Error") }
        do! s |> evaluatesToSequenceThenFails [] "Expected Error"
        do! s |> evaluatesToSequenceThenFails [] "Expected Error"
    }

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

                member this.DisposeAsync() =
                    (spy :> IAsyncDisposable).DisposeAsync()

                member this.MoveNextAsync() = vtask { return false }
            }

        let spyEnumerable =
            { new IAsyncEnumerable<int> with
                member this.GetAsyncEnumerator(_) = spyEnumerator
            }

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
            do! Task.Yield()
            2
            3
        }

        for i = 0 to 1000 do
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
    let ``should work with bind tasks in While-form`` (size: int) =
        effSeq {
            let mutable i = size

            while i > 0 do
                do! Task.Yield()
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

    [<Fact>]
    let ``should work with For-form with ConfiguredCancelableAsyncEnumerable`` () = task {
        use cts = new CancellationTokenSource()
        let chan = Channels.Channel.CreateUnbounded<int>()

        let enumerable =
            chan.Reader.ReadAllAsync().WithCancellation(cts.Token).ConfigureAwait(false)

        do! chan.Writer.WriteAsync(1)

        let task =
            effSeq {
                try
                    for i in enumerable do
                        i
                with :? AggregateException as agg ->
                    let inner = agg.InnerException
                    let t = Assert.IsType<TaskCanceledException> inner
                    Assert.Equal(cts.Token, t.CancellationToken)
                    ()
            }
            |> evaluatesToSequence [ 1 ]

        cts.Cancel()
        do! task
        return ()
    }

    [<Fact>]
    let ``should work with For-form with IAsyncEnumerable and inner cancellation`` () = task {
        use cts = new CancellationTokenSource()

        let task =
            effSeq {
                for i in infiniteAsyncEnumerable () do
                    i
            }
            |> withCancellation CancellationToken.None
            |> evaluateWithCancellation cts.Token

        cts.Cancel()
        do! task
        return ()
    }

    [<Fact>]
    let ``should work with For-form with IAsyncEnumerable and outer cancellation`` () = task {
        use cts = new CancellationTokenSource()

        let task =
            effSeq {
                for i in infiniteAsyncEnumerable () do
                    i
            }
            |> withCancellation cts.Token
            |> evaluateWithCancellation CancellationToken.None

        cts.Cancel()
        do! task
        return ()
    }


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
    [<InlineData(10_000)>]
    let ``should work with YieldFrom-form`` (size: int) =
        effSeq { yield! [ 1..size ] } |> evaluatesToSequence [ 1..size ]

    [<Fact>]
    let ``should work with YieldFrom-form for effects`` () = task {
        let seed = 102135
        let provider = RandProvider(seed)

        let! x =
            (effSeq {
                for _ in 1..3 do
                    yield! next ()

            })
                .Invoke(provider)
            |> TaskSeq.toListAsync

        let rand = Random(seed)
        let expected = [ Ok(rand.Next()); Ok(rand.Next()); Ok(rand.Next()) ]

        Assert.Equal<Result<_, string> list>(expected, x)

        return ()
    }

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
    let ``should bind tasks yielding after bind`` () =
        effSeq {
            do! Task.Yield()
            1
            2
        }
        |> evaluatesToSequence [ 1; 2 ]


    [<Fact>]
    let ``should bind tasks interleaved`` () =
        effSeq {
            1
            do! Task.Yield()
            2
            do! Task.Yield()
            3
            do! Task.Yield()
            4
        }
        |> evaluatesToSequence [
            1
            2
            3
            4
        ]


    [<Fact>]
    let ``should bind task-likes`` () =
        effSeq {
            1
            do! vtask { do! Task.Yield() }
            2
        }
        |> evaluatesToSequence [ 1; 2 ]

    [<Fact>]
    let ``should support parallel calls to GetEnumerator`` () = task {
        let enumerable = effSeq {
            1
            do! vtask { do! Task.Yield() }
            2
        }

        do!
            Parallel.ForEachAsync(
                [ 1..10 ],
                ParallelOptions(MaxDegreeOfParallelism = 10),
                (fun _ _ ->
                    ValueTask(
                        task = task {
                            do! evaluatesToSequence [ 1; 2 ] enumerable
                            return ()
                        }
                    ))
            )

        return ()
    }

    [<Fact>]
    let ``should bind async`` () =
        effSeq {
            1
            do! Async.AwaitTask(task { do! Task.Delay(10) })
            do! Async.AwaitTask(task { do! Task.Delay(10) })
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
    let ``should propagate errors repeatedly`` () = task {
        let s = effSeq {
            1
            do! Error("Expected Error")
            2
        }
        do! s |> evaluatesToSequenceThenFails [ 1 ] "Expected Error"
        do! s |> evaluatesToSequenceThenFails [ 1 ] "Expected Error"
    }

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

                member this.DisposeAsync() =
                    (spy :> IAsyncDisposable).DisposeAsync()

                member this.MoveNextAsync() = vtask { return false }
            }

        let spyEnumerable =
            { new IAsyncEnumerable<int> with
                member this.GetAsyncEnumerator(_) = spyEnumerator
            }

        let dispLike = spyEnumerable.WithCancellation(CancellationToken.None)

        effSeq {
            use _spy = dispLike.GetAsyncEnumerator()
            1
            2
            3
            do ()
        }
        |> evaluatesToSequence [ 1; 2; 3 ]
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

    [<Fact>]
    let ``should propagate errors from bound effect`` () =
        let failingEffect: Effect<unit, int, string> = eff { return! Error "effect failed" }

        effSeq {
            1
            let! _ = failingEffect
            2
        }
        |> evaluatesToSequenceThenFails [ 1 ] "effect failed"

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(10)>]
    [<InlineData(100)>]
    let ``should work with YieldFrom-form for IAsyncEnumerable`` (size: int) =
        effSeq { yield! makeAsyncEnumerable [ 1..size ] }
        |> evaluatesToSequence [ 1..size ]

    [<Fact>]
    let ``should thread environment into nested EffSeq`` () = task {
        let seed = 42
        let provider = RandProvider(seed)
        let rand = Random(seed)

        let inner = effSeq {
            for _ in 1..3 do
                yield! next ()
        }

        let! results =
            (effSeq {
                for v in inner do
                    yield v * 2
            })
                .Invoke(provider)
            |> TaskSeq.toListAsync

        let expected = [ Ok(rand.Next() * 2); Ok(rand.Next() * 2); Ok(rand.Next() * 2) ]
        Assert.Equal<Result<_, string> list>(expected, results)
    }

    [<Fact>]
    let ``should bind task returning value`` () =
        effSeq {
            let! x = Task.FromResult(42)
            yield x
            let! y = task { return x + 1 }
            yield y
        }
        |> evaluatesToSequence [ 42; 43 ]

    [<Fact>]
    let ``should stop immediately when token is already cancelled`` () = task {
        use cts = new CancellationTokenSource()
        cts.Cancel()
        let mutable readCount = 0

        do!
            eff {
                use enumerator =
                    (effSeq {
                        for i in 1..100 do
                            yield i
                    })
                        .Invoke(())
                        .GetAsyncEnumerator(cts.Token)

                while! enumerator.MoveNextAsync() do
                    readCount <- readCount + 1

                return ()
            }
            |> runOrFail

        Assert.Equal(0, readCount)
    }

    [<Fact>]
    let ``should propagate exception thrown by source IAsyncEnumerable`` () = task {
        let throwingEnumerable =
            { new IAsyncEnumerable<int> with
                member _.GetAsyncEnumerator(_) =
                    { new IAsyncEnumerator<int> with
                        member _.Current = 0
                        member _.DisposeAsync() = ValueTask.CompletedTask

                        member _.MoveNextAsync() =
                            ValueTask<bool>(task { return raise (InvalidOperationException "source error") })
                    }
            }

        let! exn =
            Assert.ThrowsAnyAsync<InvalidOperationException>(fun () ->
                effSeq {
                    for _ in throwingEnumerable do
                        ()
                }
                |> evaluatesToSequence []
                :> Task)

        Assert.Equal("source error", exn.Message)
    }

    [<Fact>]
    let ``should catch exception thrown after async bind in TryWith-form`` () =
        effSeq {
            try
                1
                do! Task.Yield()
                invalidArg "x" "post-await error"
                2
            with :? ArgumentException ->
                ()
        }
        |> evaluatesToSequence [ 1 ]

    [<Fact>]
    let ``should use environment at time of invocation`` () = task {
        let s = effSeq {
            for _ in 1..3 do
                yield! next ()
        }

        let seed1, seed2 = 42, 99
        let! results1 = s.Invoke(RandProvider(seed1)) |> TaskSeq.toListAsync
        let! results2 = s.Invoke(RandProvider(seed2)) |> TaskSeq.toListAsync

        let rand1 = Random(seed1)
        let rand2 = Random(seed2)
        let expected1 = List.init 3 (fun _ -> Ok(rand1.Next()))
        let expected2 = List.init 3 (fun _ -> Ok(rand2.Next()))

        Assert.Equal<Result<_, string> list>(expected1, results1)
        Assert.Equal<Result<_, string> list>(expected2, results2)
    }


module ``EffSeq Suspension and Resumption`` =

    // Extracts the Ok value, failing the test if it's an Error.
    let ok =
        function
        | Ok v -> v
        | Error e -> failwith $"Expected Ok but got Error: {e}"

    // -----------------------------------------------------------------------
    // Core invariant under test:
    //   MoveNextAsync returns ValueTask.FromResult (IsCompleted=true) when the
    //   state machine produces a value synchronously (yield).
    //   It returns a *pending* ValueTask<bool> when the state machine parks on
    //   an incomplete awaiter, and the pending task resolves only when the
    //   awaiter fires enumerator.MoveNext().
    // -----------------------------------------------------------------------

    [<Fact>]
    let ``yields before a pending Task bind complete synchronously`` () = task {
        let tcs = TaskCompletionSource<int>()

        let s = effSeq {
            yield 1
            yield 2
            let! x = tcs.Task
            yield x
        }

        let enumerator = s.Invoke(()).GetAsyncEnumerator(CancellationToken.None)

        // First two MoveNextAsync calls hit yield points and must be synchronous.
        let vt1 = enumerator.MoveNextAsync()
        Assert.True(vt1.IsCompleted, "yield 1 should not suspend")
        let! _ = vt1
        Assert.Equal(1, ok enumerator.Current)

        let vt2 = enumerator.MoveNextAsync()
        Assert.True(vt2.IsCompleted, "yield 2 should not suspend")
        let! _ = vt2
        Assert.Equal(2, ok enumerator.Current)

        // Third call parks on the incomplete TCS — must be pending.
        let vt3 = enumerator.MoveNextAsync()
        Assert.False(vt3.IsCompleted, "bind on incomplete Task should suspend")

        // Completing the TCS resumes the state machine and resolves the pending ValueTask.
        tcs.SetResult(42)
        let! hasNext = vt3
        Assert.True hasNext
        Assert.Equal(42, ok enumerator.Current)

        let! finished = enumerator.MoveNextAsync()
        Assert.False finished
        do! enumerator.DisposeAsync()
    }

    [<Fact>]
    let ``ValueTask bind suspends and resumes with correct value`` () = task {
        let tcs = TaskCompletionSource<int>()
        // ValueTask<'T> wrapping an incomplete Task is not completed.
        let pendingVt = ValueTask<int>(tcs.Task)

        let s = effSeq {
            yield 10
            let! x = pendingVt
            yield x * 2
        }

        let enumerator = s.Invoke(()).GetAsyncEnumerator(CancellationToken.None)

        let vt1 = enumerator.MoveNextAsync()
        Assert.True(vt1.IsCompleted, "yield before pending ValueTask should not suspend")
        let! _ = vt1
        Assert.Equal(10, ok enumerator.Current)

        let vt2 = enumerator.MoveNextAsync()
        Assert.False(vt2.IsCompleted, "bind on incomplete ValueTask should suspend")

        tcs.SetResult(7)
        let! hasNext = vt2
        Assert.True hasNext
        Assert.Equal(14, ok enumerator.Current)

        let! finished = enumerator.MoveNextAsync()
        Assert.False finished
        do! enumerator.DisposeAsync()
    }

    [<Fact>]
    let ``multiple sequential pending binds each suspend and resume independently`` () = task {
        let tcs1 = TaskCompletionSource<int>()
        let tcs2 = TaskCompletionSource<int>()

        let s = effSeq {
            yield 0
            let! x = tcs1.Task
            yield x
            let! y = tcs2.Task
            yield x + y
        }

        let enumerator = s.Invoke(()).GetAsyncEnumerator(CancellationToken.None)

        // Synchronous yield.
        let vt0 = enumerator.MoveNextAsync()
        Assert.True(vt0.IsCompleted, "initial yield should be synchronous")
        let! _ = vt0
        Assert.Equal(0, ok enumerator.Current)

        // First pending bind.
        let vt1 = enumerator.MoveNextAsync()
        Assert.False(vt1.IsCompleted, "first pending bind should suspend")
        tcs1.SetResult(3)
        let! _ = vt1
        Assert.Equal(3, ok enumerator.Current)

        // Second pending bind.
        let vt2 = enumerator.MoveNextAsync()
        Assert.False(vt2.IsCompleted, "second pending bind should suspend")
        tcs2.SetResult(5)
        let! _ = vt2
        Assert.Equal(8, ok enumerator.Current) // x + y = 3 + 5

        let! finished = enumerator.MoveNextAsync()
        Assert.False finished
        do! enumerator.DisposeAsync()
    }

    [<Fact>]
    let ``exception from faulted Task propagates through pending MoveNextAsync`` () = task {
        let tcs = TaskCompletionSource<int>()

        let s = effSeq {
            yield 1
            let! x = tcs.Task
            yield x
        }

        let enumerator = s.Invoke(()).GetAsyncEnumerator(CancellationToken.None)

        let! _ = enumerator.MoveNextAsync()
        Assert.Equal(1, ok enumerator.Current)

        // Park on the incomplete task.
        let vt = enumerator.MoveNextAsync()
        Assert.False(vt.IsCompleted, "bind on incomplete Task should suspend")

        // Fault the task — the exception flows through UnsafeOnCompleted → MoveNext →
        // awaiter.GetResult() throws → ValueTaskSource.SetException → vt faults.
        tcs.SetException(InvalidOperationException "upstream failure")

        let! exn =
            Assert.ThrowsAnyAsync<InvalidOperationException>(fun () ->
                task { let! _ = vt in () } :> Task)

        Assert.Equal("upstream failure", exn.Message)
        do! enumerator.DisposeAsync()
    }
