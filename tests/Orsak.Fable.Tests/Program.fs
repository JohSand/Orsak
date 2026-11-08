module Orsak.Fable.Tests.Program

let tests = EffTests.tests @ EffectModuleTests.tests @ RemotingTests.tests

#if FABLE_COMPILER
// JavaScript can't block on the tests: a failure rejects the promise, which Node.js reports, exiting with an error
task {
    let! failed = Runner.run tests

    if failed > 0 then
        failwith $"{failed} tests failed"
}
|> ignore
#else
[<EntryPoint>]
let main _ = (Runner.run tests).GetAwaiter().GetResult()
#endif
