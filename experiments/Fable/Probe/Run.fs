module Run

open System
open Probe

type Clock() =
    interface IClock with
        member _.Now() = DateTime(2026, 11, 8)

async {
    let! result = run (Clock()) (workflow ())
    printfn "workflow: %A" result
    let! t = Async.AwaitTask(taskCe 41)
    printfn "task: %d" t
    printfn "cancelled: %b" (cancellation ())
    printfn "isClock on a clock: %b" (isClock (Clock()))
}
|> Async.StartImmediate
