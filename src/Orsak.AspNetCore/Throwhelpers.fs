module Orsak.AspNetCore.Throwhelpers

open System


[<NoCompilerInlining>]
let argumentException (reason) = raise (ArgumentException(reason))

[<NoCompilerInlining>]
let invalidCreation () =
    let reason =
        "This expression is expected to be constructed with ReflectedDefinition(includeValue = true)."

    raise (ArgumentException(reason))
