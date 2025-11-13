module Orsak.Tests.TaskSeqTests

open System.Collections.Generic
open System.Threading
open System.Threading.Channels
open System.Threading.Tasks
open Orsak.Control
open Xunit

let makeTaskSeq (list: _ list) : IAsyncEnumerable<'a> =
    let chan = Channel.CreateBounded(list.Length)
    for a in list do
        chan.Writer.TryWrite(a) |> ignore
    chan.Writer.Complete()
    chan.Reader.ReadAllAsync()

let evaluatesToSequence (s: 't list) (es: IAsyncEnumerable<'t>) = task {
    let rar = ResizeArray<'t>()

    for e in es do
        rar.Add(e)

    let list = List.ofSeq rar
    Assert.Equal<'t>(s, list)
}

let verifyEmpty (ts: IAsyncEnumerable<'t>) = task {
    let rar = ResizeArray<'t>()
    use enumerator = ts.GetAsyncEnumerator()
    while! enumerator.MoveNextAsync() do
        rar.Add(enumerator.Current)

    return Assert.Empty(rar)
}

[<Fact>]
let ``CE taskSeq: use 'do!' with all kinds of overloads at once`` () =
    let mutable value = 0

    // this test should be expanded in case any new overload is added
    // that is supported by `do!`, to ensure the high/low priority
    // overloads still work properly
    taskSeq {
        do! task { do value <- value + 1 }
        do! ValueTask <| task { do value <- value + 1 }
        do! ValueTask(task { do value <- value + 1 })
        do! ValueTask<_>(()) // unit ValueTask that completes immediately
        do! Task.FromResult (()) // unit Task that completes immediately
        do! Task.Delay 0
        do! Async.Sleep 0
        do! async { value <- value + 1 } // eq 4
    }
    |> verifyEmpty
    |> Task.map (fun _ -> Assert.Equal(4, value))

[<Theory>]
[<InlineData(1)>]
[<InlineData(10)>]
[<InlineData(100)>]
let ``should work with For-form with TaskSeq`` (size: int) =
    taskSeq {
        for i in (makeTaskSeq [ 1..size ]) do
            i
    }
    |> evaluatesToSequence [ 1..size ]

[<Theory>]
[<InlineData(1)>]
[<InlineData(10)>]
[<InlineData(100)>]
let ``should work with For-form with TaskSeq with Cancellation`` (size: int) =
    taskSeq {
        for i in (makeTaskSeq [ 1..size ]).WithCancellation(CancellationToken.None) do
            i
    }
    |> evaluatesToSequence [ 1..size ]
