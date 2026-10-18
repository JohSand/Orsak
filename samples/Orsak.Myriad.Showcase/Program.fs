// For more information see https://aka.ms/fsharp-console-apps

open Orsak
open Orsak.Myriad
open System.Threading
open Microsoft.Extensions.Caching.Memory
open Orsak.Myriad.Showcase.Inline
open Orsak.Myriad.Showcase

let myRunner  (cache: IFace) (rnd: IRandomGenerator) =
    Runner.mkRunner {
        fromEffect rnd
        fromEffect cache
    }
    |> fun runner ->
        Face.countBeans 1 "" |> Effect.run runner

let environment =
    AppEnvironment.create {|
        BeanCounter = { new IBeanCounter with member _.Count() = 3 }
        GuidGenerator = GuidGenerator.defaultGen ()
        RandomGenerator = DefaultRandom(System.Random 42)
    |}

let countBeans () =
    Effect.Create(fun (provider: #IBeanCounterProvider) -> provider.Effect.Count())

let workflow () : Effect<_, string, string> = eff {
    let! beans = countBeans ()
    let! extra = Random.next 1 2
    let! id = GuidGenerator.genGuid ()
    return $"{beans + extra} beans, batch {id}"
}

printfn "%s" ((workflow () |> Effect.runOrFail environment).Result)

let inlineWorkflow () : Effect<_, string, string> = eff {
    let! greeting = Inline.greet "inline"
    let! now = Inline.Clock.now ()
    return $"""{greeting} at {now.ToString("HH:mm")}"""
}

printfn "%s" ((inlineWorkflow () |> Effect.runOrFail (Inline.environment ())).Result)



let localizedWorkflow () : Effect<_, string, string> = eff {
    let! greeting = Inline.greet "localized"
    let! culture = Effect.Create(fun (provider: #Inline.ILocaleProvider) -> provider.Effect.Culture)
    return $"{greeting} in {culture}"
}

printfn "%s" ((localizedWorkflow () |> Effect.runOrFail (Inline.localized ())).Result)

#nowarn "57"

let workLog = ResizeArray<string>()

let inUnitOfWork () : Effect<_, string, string> =
    uow {
        let! greeting = Inline.greet "unit of work"
        do! Effect.Create(fun (provider: #Inline.IUnitOfWorkProvider) -> provider.Effect.Record greeting)
        return greeting
    }
   // |> Orsak.Scoped.CompletingScope<Inline.IUnitOfWork>.Within

printfn "%s: %A" ((inUnitOfWork () |> Effect.runOrFail (Inline.environmentWithWork workLog)).Result) (List.ofSeq workLog)
