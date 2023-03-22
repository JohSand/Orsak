namespace Models

open Fleece
open Fleece.SystemTextJson

type Ping = {
    message: string
    a: int
    b: float
} with

    static member ToJson(x: Ping) = jobj [ "message" .= x.message ]
