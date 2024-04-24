module Orsak.AspNetCore.Throwhelpers

open Polly
open System


[<NoCompilerInlining>]
let argumentException (reason) : unit =
    raise (ArgumentException(reason))

[<NoCompilerInlining>]
let argumentOutOfRange (t: DelayBackoffType) : unit =
    raise (ArgumentOutOfRangeException("BackoffType", t, "The retry backoff type is not supported."))    
