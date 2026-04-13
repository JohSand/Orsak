module Orsak.Tests.TailCallTests

open System.Diagnostics
open System.Threading.Tasks
open Orsak
open Xunit
open System.Net

let private currentStackDepth () : Effect<unit, int, string> =
    mkEffect (fun _ -> ValueTask.FromResult(Ok(StackTrace(false).FrameCount)))

let rec private tailLoop n : Effect<unit, int, string> = eff {
    if n = 0 then
        return! currentStackDepth ()
    else
        let! x = Dns.GetHostEntryAsync("google.com")
        return! tailLoop (n - 1)
}

[<Fact>]
let ``tail recursive effect should not grow the call stack`` () = task {
    let! r0 = tailLoop 0 |> Effect.runOrFail ()
    let! r100 = tailLoop 100 |> Effect.runOrFail ()

    let frames0 = r0
    let frames100 =  r100

    let growth = frames100 - frames0

    // With the current builder, stack grows O(n): each return! adds ~4 frames
    // for the EffectDelegate.Invoke → MethodBuilder.Start → MoveNext chain,
    // so growth ≈ 100 * 4 ≈ 400 frames.
    // With a proper trampoline, growth should be O(1).
    Assert.True(growth <= 5, $"Stack grew by {growth} frames over 100 tail-recursive calls")
}
