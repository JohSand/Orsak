module Orsak.Tests.TailCallTests

open System.Diagnostics
open System.Threading.Tasks
open Orsak
open Xunit

let private currentStackDepth () : Effect<unit, int, string> =
    mkEffect (fun _ -> ValueTask.FromResult(Ok(StackTrace(false).FrameCount)))

let rec private tailLoop n : Effect<unit, int, string> = eff {
    if n = 0 then return! currentStackDepth ()
    else return! tailLoop (n - 1)
}

[<Fact>]
let ``tail recursive effect should not grow the call stack`` () = task {
    let! r0 = tailLoop 0 |> Effect.run ()
    let! r100 = tailLoop 100 |> Effect.run ()

    let frames0 = Result.defaultWith (fun _ -> 0) r0
    let frames100 = Result.defaultWith (fun _ -> 0) r100

    let growth = frames100 - frames0

    // With the current builder, stack grows O(n): each return! adds ~4 frames
    // for the EffectDelegate.Invoke → MethodBuilder.Start → MoveNext chain,
    // so growth ≈ 100 * 4 ≈ 400 frames.
    // With a proper trampoline, growth should be O(1).
    //
    // This assertion fails today — it documents the expected behaviour after the fix.
    Assert.True(growth <= 5, $"Stack grew by {growth} frames over 100 tail-recursive calls")
}
