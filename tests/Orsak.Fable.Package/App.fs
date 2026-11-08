module App

open System.Threading.Tasks
open Orsak

type IGreeter =
    abstract Greet: string -> string

type Greeter() =
    interface IGreeter with
        member _.Greet name = $"Hello {name}"

let greet name = Effect.Create(fun (g: #IGreeter) -> g.Greet name)

let greetAll names : Effect<Greeter, string list, string> = eff {
    let! greetings = names |> List.map greet |> Effect.par
    do! Task.Delay 1
    return greetings
}

task {
    match! greetAll [ "Fable"; "Node" ] |> Effect.run (Greeter()) with
    | Ok greetings -> printfn "%A" greetings
    | Error e -> printfn "failed: %s" e
}
|> ignore
