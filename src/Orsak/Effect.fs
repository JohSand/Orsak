namespace Orsak

open FSharp.Control
open System.Threading.Tasks
open System.Runtime.InteropServices
open System

[<RequireQualifiedAccess>]
module Effect =

    /// <summary>
    /// Run the effect
    /// </summary>
    /// <typeparam name="r" > The environment required to run the effect </typeparam>
    /// <typeparam name="a" > The resulting type when the effect runs successfully </typeparam>
    /// <typeparam name="e" > The resulting type when the effect fails</typeparam>///
    let inline run<'r, 'a, 'e> (env: 'r) (e: Effect<'r, 'a, 'e>) = e.Invoke env

    let inline ret<'r, 'a, 'e> (a: 'a) : Effect<'r, 'a, 'e> =
        eff { return a }
        
    //bug https://github.com/dotnet/fsharp/issues/12761
    let inline bind<'r, 'a, 'b, 'e> ([<InlineIfLambda>] f: 'a -> Effect<'r, 'b, 'e>) (e: Effect<'r, 'a, 'e>) =
        eff {
            let! a = e
            return! f a
        }

    let inline join e = bind id e

    let inline map ([<InlineIfLambda>] f: 'a -> 'b) (e: Effect<'r, 'a, 'e>) =
        eff {
            let! a = e
            return f a
        }

    let map2 f s1 s2 = bind (fun a -> map (f a) s2) s1

    let inline recover ([<InlineIfLambda>] f: 'e -> 'a) (e: Effect<'r, 'a, 'e>) : Effect<'r, 'a, 'e> =
        eff.Run(eff.Recover(e, f))

    let inline tryRecover ([<InlineIfLambda>] f: 'e -> Result<'a, 'e>) (e: Effect<'r, 'a, 'e>) =
        eff.Run(eff.TryRecover(e, f))

    let inline onError ([<InlineIfLambda>] f: 'e -> Effect<'r, 'a, 'e>) (e: Effect<'r, 'a, 'e>) =
        eff.Run(eff.TryRecover(e, f))

    let inline joinResult (e: Effect<'r, Result<'a, 'e>, 'e>) : Effect<'r, 'a, 'e> =
        eff {
            let! a = e
            return! a
        }

    let changeError (f: 'e1 -> 'e2) (e: Effect<'r, 'a, 'e1>) : Effect<'r, 'a, 'e2> =
        eff.Run(
            eff.ChangeError(e, f)
        )

    let bindNewError (e: Effect<'r, 'a, 'e1>) (f: 'a -> Effect<'r, 'b, 'e2>) : Effect<_, _, Choice<_, _>> =
        eff {
            let! e = e |> changeError Choice1Of2
            let! a = e |> f |> changeError Choice2Of2
            return a
        }

    let inline zip (m1: Effect<'r, 'a, 'e>) (m2: Effect<'r, 'b, 'e>) ([<InlineIfLambda>] f: struct ('a * 'b) -> 'g) =
        eff {
            let! a = m1
            and! b = m2
            return f (a, b)
        }

    let inline resultJoin (e: Result<Effect<'r, 'a, 'e>, 'e>) : Effect<'r, 'a, 'e> =
        match e with
        | Ok e -> e
        | Error e -> eff { return! Error e  }

    let inline joinTask (e: Effect<'r, Task<'a>, 'e>) : Effect<'r, 'a, 'e> =
        eff {
            let! t = e
            let! a = t
            return a
        }

    let inline taskJoin (t: Task<Effect<'r, 'a, 'e>>) : Effect<'r, 'a, 'e> =
        eff {
            let! e = t
            return! e
        }

    //we want a more efficient version of this
    let inline par (eff: Effect<'r, 'a, 'e> seq) =
        mkEffect (fun rEnv -> vtask {
            let! results =
                eff
                |> Seq.map (run rEnv)
                |> Seq.map (fun (t: AsyncResult<'a, 'e>) -> t.AsTask())
                |> Task.WhenAll

            return
                Ok []
                |> Array.foldBack
                    (fun curr agg ->
                        match agg with
                        | Ok (list: 'a list) ->
                            match curr with
                            | Ok a -> Ok(a :: list)
                            | Error e -> Error e
                        | Error e -> Error e)
                    results
            })

    //todo
    let inline traverse f (eff: Effect<'r, 'a, 'e> array) =
        mkEffect(fun rEnv -> 
            vtask {
                let mutable final = Ok (Array.zeroCreate eff.Length)

                for i = 0 to (eff.Length - 1) do
                    match final with
                    | Ok list ->
                        match! run rEnv eff[i] with
                        | Ok a -> 
                            list[i] <- f a
                        | Error e -> 
                            final <- Error e
                    | Error _ ->                         
                        ()

                return final
            }
        )

    //probably defer to fsharpplus for polymorphic version
    let inline sequence (eff: Effect<'r, 'a, 'e> list) = traverse id (Array.ofList eff)

    let inline timeout (ts: TimeSpan) onTimeout (eff: Effect<_, _, _>) =
        mkEffect(fun rEnv -> 
            vtask {
                try
                    return! eff.Run(rEnv).AsTask().WaitAsync(ts)
                with
                | :? TimeoutException -> return Error onTimeout
            })

    let inline race (eff1: Effect<_, _, _>) (eff2: Effect<_, _, _>) =
        mkEffect(fun rEnv ->
            vtask {
                let t1 = eff1.Run(rEnv).AsTask()
                let t2 = eff2.Run(rEnv).AsTask()
                let! winner = Task.WhenAny(t1, t2)
                return! winner
            }
        )

type Effect =
    static member Error(error: 'e) : Effect<'a, 'b, 'e> =
        eff { return! Error error }

    static member Create<'a, 'b, 'e>(x: 'a -> 'b, [<ParamArray>] _a: Object []) : Effect<'a, 'b, 'e> =
        mkEffect(x >> Ok >> ValueTask.FromResult)

    static member Create(full: 'a -> Task<Result<'b, 'e>>) =
        mkEffect(fun a -> ValueTask<_>(task =  full a ))

    static member Create(full: 'a -> ValueTask<Result<'b, 'e>>) =
        mkEffect(fun a -> full a)

    static member Create(x: 'a -> Async<'b>) : Effect<'a, 'b, _> =
        mkEffect (fun a ->
            vtask {
                let! b = x a
                return Ok b
            })

    static member Create(x: 'a -> Task<'b>, [<Optional>] _medium: byte) : Effect<'a, 'b, _> =
        mkEffect (fun a ->
            vtask {
                let! b = x a
                return Ok b
            })

    static member Create(f: 'a -> ValueTask<'b>, [<Optional>] _medium: byte) : Effect<'a, 'b, _> =
        mkEffect (fun a ->
            vtask {
                let! b = f a
                return Ok b
            })

