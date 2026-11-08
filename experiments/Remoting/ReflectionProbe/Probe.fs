module Probe

open System
open Microsoft.FSharp.Reflection

type Line = { Product: string; Quantity: int }
type Choice = | Plain | WithData of string | Pair of int * string

let check name (f: unit -> string) =
    try printfn $"ok    {name}: {f ()}" with ex -> printfn $"FAIL  {name}: {ex.Message}"

let listT = typeof<int list>
let optionT = typeof<string option>

check "type equality, typeof<int>" (fun () -> string (typeof<int> = typeof<int>, typeof<int> = typeof<string>))
check "IsArray, GetElementType" (fun () -> $"{typeof<int[]>.IsArray}, {typeof<int[]>.GetElementType() = typeof<int>}")
check "list: IsGenericType, GetGenericTypeDefinition" (fun () -> $"{listT.IsGenericType}, {listT.GetGenericTypeDefinition() = typedefof<list<_>>}")
check "option: GetGenericTypeDefinition, GetGenericArguments" (fun () -> $"{optionT.GetGenericTypeDefinition() = typedefof<option<_>>}, {optionT.GetGenericArguments()[0] = typeof<string>}")
check "list and option counted as unions" (fun () -> $"list {FSharpType.IsUnion listT}, option {FSharpType.IsUnion optionT}")
check "record fields, read and make" (fun () ->
    let fields = FSharpType.GetRecordFields typeof<Line>
    let values = FSharpValue.GetRecordFields { Product = "apple"; Quantity = 2 }
    let made = FSharpValue.MakeRecord(typeof<Line>, [| box "pear"; box 3 |]) :?> Line
    $"""{fields |> Array.map (fun f -> f.Name) |> String.concat ","} {values[0]} {made}""")
check "union cases, read and make" (fun () ->
    let case, fields = FSharpValue.GetUnionFields(Pair(1, "x"), typeof<Choice>)
    let cases = FSharpType.GetUnionCases typeof<Choice>
    let made = FSharpValue.MakeUnion(cases[1], [| box "y" |]) :?> Choice
    $"{case.Name} {fields.Length} {made}")
check "Result as a union" (fun () ->
    let case, fields = FSharpValue.GetUnionFields((Error "no": Result<int, string>), typeof<Result<int, string>>)
    $"{case.Name} {fields[0]}")
check "tuples" (fun () ->
    let t = typeof<int * string>
    let pair = box (1, "a")
    let second = FSharpValue.GetTupleFields(pair)[1]
    $"{FSharpType.IsTuple t} {FSharpType.GetTupleElements(t).Length} {second}")
check "an option's value, unboxed as obj option" (fun () ->
    let some = unbox<obj option> (box (Some "x"))
    let none = unbox<obj option> (box (None: string option))
    $"%A{some}, %A{none}")
check "UTF-8 bytes" (fun () ->
    let text = "åäö"
    let b = Text.Encoding.UTF8.GetBytes text
    $"{b.Length} {Text.Encoding.UTF8.GetString b}")
check "number text" (fun () ->
    let parsed = Double.Parse "1.5e3"
    let negative = Int32.Parse "-12"
    $"{string 1.5} {string 2} {string 9007199254740993L} {parsed} {negative}")
