module App

open System.Threading.Tasks
open Orsak
open Orsak.Remoting

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
    | Ok greetings ->
        printfn "%A" greetings
        // Orsak.Remoting, from its package: the greetings in its JSON
        let json = (JsonFormat.Instance :> IWireFormat).EncodeResult(box greetings, typeof<string list>)
        printfn "%s" (System.Text.Encoding.UTF8.GetString json)
    | Error e -> printfn "failed: %s" e
}
|> ignore
