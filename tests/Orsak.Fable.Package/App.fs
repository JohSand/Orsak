module App

open System
open Orsak
open Orsak.Remoting

type IGreeter =
    abstract Greet: string -> string

type Greeter() =
    interface IGreeter with
        member _.Greet name = $"Hello {name}"

    // the base effects, from the package: Time.delay waits on this clock
    interface ITimeProvider with
        member _.Clock = TimeProvider.System

let greet name = Effect.Create(fun (g: #IGreeter) -> g.Greet name)

let greetAll names : Effect<Greeter, string list, string> = eff {
    let! greetings = names |> List.map greet |> Effect.par
    do! Time.delay (TimeSpan.FromMilliseconds 1.0)
    return greetings
}

task {
    match! greetAll [ "Fable"; "Node" ] |> Effect.run (Greeter()) with
    | Ok greetings ->
        printfn "%A" greetings
        // Orsak.Remoting, from its package: the greetings in its JSON
        let json = (JsonFormat.Instance :> IWireFormat).CodecFor(typeof<string list>).Encode(box greetings)
        printfn "%s" (System.Text.Encoding.UTF8.GetString json)
    | Error e -> printfn "failed: %s" e
}
|> ignore
