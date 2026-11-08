/// Orsak.Remoting's JSON, which must be the same text on .NET and Fable, for a server and a client to understand each
/// other: each value's text, and the round trip back.
module Orsak.Fable.Tests.RemotingTests

open System
open System.Text
open Orsak.Remoting
open Orsak.Fable.Tests.Runner

type Line = { Product: string; Quantity: int }

type Order = {
    Customer: string
    Lines: Line list
    Note: string option
    Codes: int array
}

type Tree =
    | Leaf
    | Node of int * Tree list

type Shape =
    | Empty
    | Circle of radius: float
    | Rect of width: int * height: int

let format = JsonFormat.Instance :> IWireFormat

/// Inline, so the type is known on Fable, which erases generic arguments at run time.
let inline writesAndReads (expected: string) (value: 'T) = task {
    let codec = format.CodecFor typeof<'T>
    let text = Encoding.UTF8.GetString(codec.Encode(box value))
    equal expected text
    let back = codec.Decode(Encoding.UTF8.GetBytes text) :?> 'T
    equal value back
}

let order = {
    Customer = "ada"
    Lines = [ { Product = "apple"; Quantity = 2 } ]
    Note = Some "ring twice"
    Codes = [| 1; 2 |]
}

let tests = [
    test "Orsak.Remoting's JSON of a record with a list, an option and an array" (fun () ->
        writesAndReads
            """{"Customer":"ada","Lines":[{"Product":"apple","Quantity":2}],"Note":"ring twice","Codes":[1,2]}"""
            order)

    test "Orsak.Remoting's JSON of None, and of empty lists and arrays" (fun () ->
        writesAndReads
            """{"Customer":"bo","Lines":[],"Note":null,"Codes":[]}"""
            { order with Customer = "bo"; Lines = []; Note = None; Codes = [||] })

    test "Orsak.Remoting's JSON of a list of records, and an option of one" (fun () -> task {
        do! writesAndReads """[{"Product":"a","Quantity":1}]""" [ { Product = "a"; Quantity = 1 } ]
        do! writesAndReads """{"Product":"a","Quantity":1}""" (Some { Product = "a"; Quantity = 1 })
    })

    test "Orsak.Remoting's JSON of Result" (fun () -> task {
        do! writesAndReads """{"Ok":1}""" (Ok 1: Result<int, string>)
        do! writesAndReads """{"Error":"no"}""" (Error "no": Result<int, string>)
    })

    test "Orsak.Remoting's JSON of union cases without, with one and with two fields" (fun () ->
        writesAndReads """["Empty",{"Circle":1.5},{"Rect":[2,3]}]""" [ Empty; Circle 1.5; Rect(2, 3) ])

    test "Orsak.Remoting's JSON of a tuple" (fun () -> writesAndReads """[1,"a",true]""" (1, "a", true))

    test "Orsak.Remoting's JSON of int64, decimal and Guid, as strings" (fun () ->
        writesAndReads
            """["9007199254740993","1.25","6f9619ff-8b86-d011-b42d-00cf4fc964ff"]"""
            (9007199254740993L, 1.25m, Guid "6f9619ff-8b86-d011-b42d-00cf4fc964ff"))

    test "Orsak.Remoting's JSON of a float keeps its digits" (fun () -> writesAndReads "0.1" 0.1)

    test "Orsak.Remoting's JSON of a string with escapes and unicode" (fun () ->
        writesAndReads "\"quote \\\" backslash \\\\ line\\n åäö \\u0001\"" "quote \" backslash \\ line\n åäö \u0001")

    test "Orsak.Remoting's JSON of unit" (fun () -> writesAndReads "null" ())

    test "Orsak.Remoting's JSON of a type that contains itself" (fun () ->
        writesAndReads """{"Node":[1,["Leaf",{"Node":[2,[]]}]]}""" (Node(1, [ Leaf; Node(2, []) ])))

    test "Orsak.Remoting's JSON of bytes, as base64" (fun () -> writesAndReads "\"AAF//w==\"" [| 0uy; 1uy; 127uy; 255uy |])

    test "Orsak.Remoting's JSON reads a float without a fraction, as JavaScript writes it" (fun () ->
        task { equal (box 2.0) (format.CodecFor(typeof<float>).Decode(Encoding.UTF8.GetBytes "2")) })

    test "Orsak.Remoting's JSON encoding passes the conformance checks" (fun () ->
        task { equal ([]: string list) (Conformance.check JsonEncoding.Instance) })
]
