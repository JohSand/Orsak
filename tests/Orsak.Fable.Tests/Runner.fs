/// A small test runner that works both on .NET and compiled with Fable, where test frameworks such as xunit can't
/// run: a test is a name and a function returning a task, and an assertion is a function that throws.
module Orsak.Fable.Tests.Runner

open System.Threading.Tasks

type Test = { Name: string; Body: unit -> Task<unit> }

let test name (body: unit -> Task<unit>) = { Name = name; Body = body }

let equal (expected: 'a) (actual: 'a) =
    if expected <> actual then
        failwith $"expected %A{expected}, but got %A{actual}"

let isTrue message condition =
    if not condition then
        failwith message

/// Runs the tests one at a time, printing each one's outcome, and returns the number that failed.
let run (tests: Test list) : Task<int> = task {
    let mutable failed = 0

    for t in tests do
        try
            do! t.Body()
            printfn $"passed  {t.Name}"
        with ex ->
            failed <- failed + 1
            printfn $"FAILED  {t.Name}: {ex.Message}"

    printfn $"{List.length tests - failed} passed, {failed} failed"
    return failed
}
