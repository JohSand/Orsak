/// The JSON format's text for each kind of value, and the round trip back, the same on .NET and Node.js: run with
/// dotnet run, and compiled with Fable. The same text on both is what lets a server and a client understand each other.
module Remoting.CodecTests.Program

open System
open System.Text
open Orsak.Remoting

type Line = { Product: string; Quantity: int }

type Order = {
    Customer: string
    Lines: Line list
    Note: string option
    Codes: int array
}

type Shape =
    | Empty
    | Circle of radius: float
    | Rect of width: int * height: int

let format = JsonFormat.Instance :> IWireFormat

let mutable failed = 0

let inline check (name: string) (expected: string) (value: 'T) =
    let text = Encoding.UTF8.GetString(format.EncodeResult(box value, typeof<'T>))
    let back = format.DecodeResult(Encoding.UTF8.GetBytes text, typeof<'T>) :?> 'T

    if text <> expected then
        failed <- failed + 1
        printfn $"FAILED  {name}: wrote {text}, expected {expected}"
    elif back <> value then
        failed <- failed + 1
        printfn $"FAILED  {name}: read back %A{back}, expected %A{value}"
    else
        printfn $"passed  {name}: {text}"

let order = {
    Customer = "ada"
    Lines = [ { Product = "apple"; Quantity = 2 } ]
    Note = Some "ring twice"
    Codes = [| 1; 2 |]
}

check "a record with a list, an option and an array" """{"Customer":"ada","Lines":[{"Product":"apple","Quantity":2}],"Note":"ring twice","Codes":[1,2]}""" order
check "None, and an empty list" """{"Customer":"bo","Lines":[],"Note":null,"Codes":[]}""" { order with Customer = "bo"; Lines = []; Note = None; Codes = [||] }
check "a list of records" """[{"Product":"a","Quantity":1},{"Product":"b","Quantity":2}]""" [ { Product = "a"; Quantity = 1 }; { Product = "b"; Quantity = 2 } ]
check "an option of a record" """{"Product":"a","Quantity":1}""" (Some { Product = "a"; Quantity = 1 })
check "Ok" """{"Ok":1}""" (Ok 1: Result<int, string>)
check "Error" """{"Error":"no"}""" (Error "no": Result<int, string>)
check "union cases without, with one and with two fields" """["Empty",{"Circle":1.5},{"Rect":[2,3]}]""" [ Empty; Circle 1.5; Rect(2, 3) ]
check "a tuple" """[1,"a",true]""" (1, "a", true)
check "int64, decimal and Guid, as strings" """["9007199254740993","1.25","6f9619ff-8b86-d011-b42d-00cf4fc964ff"]""" (9007199254740993L, 1.25m, Guid "6f9619ff-8b86-d011-b42d-00cf4fc964ff")
check "a float keeps its digits" """0.1""" 0.1
check "escapes and unicode" "\"quote \\\" backslash \\\\ line\\n åäö \\u0001\"" "quote \" backslash \\ line\n åäö \u0001"
check "unit" """null""" ()

printfn $"{failed} failed"

#if FABLE_COMPILER
if failed > 0 then failwith $"{failed} failed"
#else
exit failed
#endif
