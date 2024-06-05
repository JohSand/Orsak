module Orsak.AspNetCore.Throwhelpers

open System


[<NoCompilerInlining>]
let argumentException (reason) : unit =
    raise (ArgumentException(reason))

