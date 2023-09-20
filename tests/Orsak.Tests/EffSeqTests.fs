module Orsak.Tests.EffResumableSeqTests

open Xunit
open FSharp.Control
open Orsak
open System.Threading.Tasks

let run (e: Effect<unit, 'b, string>) = e.RunOrFail()

let expectError (error: 'err) (e: Effect<unit, _, 'err>) =
    e
    |> Effect.map (fun v -> failwith $"Got value %A{v} when expecting error %A{error}")
    |> Effect.tryRecover (fun es ->
        if es = error then
            Ok()
        else
            failwith $"Got error %O{es} when expecting error %O{error}")


let delay () = task { do! Task.Delay(10) }
let empty () = effSeq { () }

let ``three elements`` () = effSeq {
    1
    2
    3
}

let breakpoint () = eff { return 1 }

let ``three elements with bind`` () = effSeq {
    do! eff { return () }
    let! e = breakpoint ()
    e
    do! Task.Delay(10)
    2
    do! Task.Delay(20)
    3
}

let ``three elements with while`` () = effSeq {
    let mutable i = 3

    while i > 0 do
        yield 4 - i
        i <- i - 1
}

let ``three elements with for`` () = effSeq {
    for i in 1..3 do
        yield 4 - i
}

[<Fact>]
let ``empty sequence`` () =
    (eff {
        let res = ResizeArray<_>()
        let s = empty ()

        for a in s do
            res.Add(a)

        let list = List.ofSeq res
        Assert.Equal<int>([], list)
    })
        .RunOrFail()


[<Fact>]
let ``empty sequence twice`` () =
    (eff {
        let res = ResizeArray<_>()
        let s = empty ()

        for a in s do
            res.Add(a)

        for a in s do
            res.Add(a)

        let list = List.ofSeq res
        Assert.Equal<int>([], list)
    })
        .RunOrFail()

[<Fact>]
let ``three elements test`` () =
    (eff {
        let res = ResizeArray<_>()

        for a in ``three elements`` () do
            res.Add(a)

        let list = List.ofSeq res
        Assert.Equal<int>([ 1; 2; 3 ], list)
    })
        .RunOrFail()

[<Fact>]
let ``three elements twice`` () =
    (eff {
        let res = ResizeArray<_>()
        let s = ``three elements`` ()

        for a in s do
            res.Add(a)

        for a in s do
            res.Add(a)

        let list = List.ofSeq res

        Assert.Equal<int>(
            [
                1
                2
                3
                1
                2
                3
            ],
            list
        )
    })
        .RunOrFail()

[<Fact>]
let ``three elements with delay`` () =
    (eff {
        let res = ResizeArray<_>()
        let s = ``three elements with bind`` ()

        for a in s do
            res.Add(a)

        let list = List.ofSeq res
        Assert.Equal<int>([ 1; 2; 3 ], list)
    })

        .RunOrFail()



[<Fact>]
let ``test three elements with while`` () =
    (eff {
        let res = ResizeArray<_>()
        let s = ``three elements with while`` ()

        for a in s do
            res.Add(a)

        let list = List.ofSeq res
        Assert.Equal<int>([ 1; 2; 3 ], list)
    })

        .RunOrFail()

[<Fact>]
let ``test three elements with for`` () =
    (eff {
        let res = ResizeArray<_>()
        let s = ``three elements with for`` ()

        for a in s do
            res.Add(a)

        let list = List.ofSeq res
        Assert.Equal<int>([ 3; 2; 1 ], list)
    })

        .RunOrFail()

let errorSeq () = effSeq {
    1
    2
    do! eff { return! Error "Expected error" }
    3
    4
}

[<Fact>]
let ``test bind error elements with for`` () = task {
    let res = ResizeArray<_>()
    do!
        eff {
            for a in errorSeq () do
                res.Add(a)            
        }
        |> expectError "Expected error"
        |> run

    let list = List.ofSeq res
    Assert.Equal<int>([ 1; 2 ], list)
}

[<Fact>]
let ``test bind error elements with for and recovery`` () = task {
    let res = ResizeArray<_>()
    let streamThrough () = effSeq {
        for a in errorSeq () do
            res.Add(a)   
            yield a
    }
    do!
        eff {
            for a in streamThrough ()  do
                ignore a         
        }
        |> expectError "Expected error"
        |> run

    let list = List.ofSeq res
    Assert.Equal<int>([ 1; 2 ], list)
}
