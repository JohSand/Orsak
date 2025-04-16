namespace Orsak.Tests

open Orsak
open Xunit
open System.Threading.Tasks

type IFace =
    abstract member A: ValueTask<Result<int, string>>
    abstract member B: Task<Result<int, string>>
    abstract member C: ValueTask<int>
    abstract member D: Task<int>
    abstract member E: Result<int, string>
    abstract member F: int

type Face() =
    interface IFace with
        member this.A = ValueTask<_>(Ok 1)
        member this.B = Task.FromResult(Ok 1)
        member this.C = ValueTask<_>(1)
        member this.D = Task.FromResult(1)
        member this.E = Ok 1
        member this.F = 1

type FaceRunner() =
    interface IProvide<IFace> with
        member _.Effect = Face()

module OverloadTests =
    [<Fact>]
    let ``lambda bind of ValueTask<Result<_,_>> works`` () =
        eff {
            let! _a = fun (f: IFace) -> f.A
            return ()
        }
        |> Effect.runOrFail (FaceRunner())

    [<Fact>]
    let ``lambda bind of Task<Result<_,_>> works`` () =
        eff {
            let! _a = fun (f: IFace) -> f.B
            return ()
        }
        |> Effect.runOrFail (FaceRunner())

    [<Fact>]
    let ``lambda bind of Result<_,_> works`` () =
        eff {
            let! _a = fun (f: IFace) -> f.E
            return ()
        }
        |> Effect.runOrFail (FaceRunner())

    [<Fact>]
    let ``lambda bind of ValueTask<_> works`` () =
        eff {
            let! _a = fun (f: IFace) -> f.C
            return ()
        }
        |> Effect.runOrFail (FaceRunner())

    [<Fact>]
    let ``lambda bind of Task<_> works`` () =
        eff {
            let! _a = fun (f: IFace) -> f.D
            return ()
        }
        |> Effect.runOrFail (FaceRunner())

    [<Fact>]
    let ``lambda bind of value works`` () =
        eff {
            let! _a = fun (f: IFace) -> f.F
            return ()
        }
        |> Effect.runOrFail (FaceRunner())

    [<Fact>]
    let ``lambda return of ValueTask<Result<_,_>> works`` () =
        eff { return! fun (f: IFace) -> f.A } |> Effect.runOrFail (FaceRunner())

    [<Fact>]
    let ``lambda return of Task<Result<_,_>> works`` () =
        eff { return! fun (f: IFace) -> f.B } |> Effect.runOrFail (FaceRunner())

    [<Fact>]
    let ``lambda return of Result<_,_> works`` () =
        eff { return! fun (f: IFace) -> f.E } |> Effect.runOrFail (FaceRunner())

    [<Fact>]
    let ``lambda return of ValueTask<_> works`` () =
        eff { return! fun (f: IFace) -> f.C } |> Effect.runOrFail (FaceRunner())

    [<Fact>]
    let ``lambda return of Task<_> works`` () =
        eff { return! fun (f: IFace) -> f.D } |> Effect.runOrFail (FaceRunner())

    [<Fact>]
    let ``lambda return of value works`` () =
        eff { return! fun (f: IFace) -> f.F } |> Effect.runOrFail (FaceRunner())
