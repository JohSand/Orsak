namespace Orsak.Remoting

// Values of any type, converted by reflection: obj is the type meant, wherever it is inferred
#nowarn "3559"

open System
open System.Collections
open System.Globalization
open Microsoft.FSharp.Reflection

/// <summary>
/// Converts the values of one type to <see cref="T:Orsak.Remoting.WireValue"/>s and back, compiled once for the type,
/// by <c>WireMapping.compile</c>.
/// </summary>
type WireMapping = {
    /// Converts a value of the type.
    Encode: obj -> WireValue
    /// Converts a value back to the type.
    Decode: WireValue -> obj
}

/// <summary>
/// Compiles <see cref="T:Orsak.Remoting.WireMapping"/>s for F# types, public or not, by reflection on their types, which
/// .NET and Fable both have for F# types: the same for every encoding, so a contract's types mean the same in each. A
/// mapping is compiled once, for every value it converts after, and compiling it fails for a type it doesn't support,
/// so that a client or server finds out when it starts, not on its first call.
/// </summary>
/// <remarks>
/// A record is a <c>Map</c> of its fields; a union case without fields is its name, one with fields a <c>Map</c> of its
/// name and its field, or an <c>Array</c> of its fields; a list, an array or a tuple is an <c>Array</c>; an option is
/// its value or <c>Nil</c>; a byte array is <c>Bytes</c>; an int64, a decimal and a Guid are strings, so JavaScript's
/// numbers don't round them. Reading, a float may be an <c>Int</c>, and bytes a base64 <c>String</c>, for the
/// encodings that can't tell them apart. Lists and options are checked before unions, which they also are to
/// reflection.
/// </remarks>
module WireMapping =

    let private isGeneric (definition: Type) (t: Type) =
        t.IsGenericType && t.GetGenericTypeDefinition() = definition

    let private fail (t: Type) (value: WireValue) : obj =
        failwith $"Orsak.Remoting can't read %A{value} as {t.FullName}"

    let private mapping encode decode = { Encode = encode; Decode = decode }

    let private primitive (t: Type) (write: obj -> WireValue) (read: WireValue -> obj option) =
        mapping write (fun value ->
            match read value with
            | Some v -> v
            | None -> fail t value)

    /// The mapping of a field or an element, compiled when first used, so a type that contains itself, such as a tree,
    /// is compiled once rather than without end.
    let rec private child (t: Type) =
        let compiled = lazy (compile t)
        mapping (fun v -> compiled.Value.Encode v) (fun value -> compiled.Value.Decode value)

    /// <summary>Compiles the mapping of <paramref name="t"/>, or fails if its type isn't supported.</summary>
    /// <param name="t">The type whose values the mapping converts</param>
    and compile (t: Type) : WireMapping =
        if t = typeof<unit> then
            mapping (fun _ -> WireValue.Nil) (fun _ -> box ())
        elif t = typeof<string> then
            primitive t (fun v -> if isNull v then WireValue.Nil else WireValue.String(unbox v)) (function
                | WireValue.String s -> Some(box s)
                | WireValue.Nil -> Some null
                | _ -> None)
        elif t = typeof<bool> then
            primitive t (fun v -> WireValue.Bool(unbox v)) (function
                | WireValue.Bool b -> Some(box b)
                | _ -> None)
        elif t = typeof<int> then
            primitive t (fun v -> WireValue.Int(unbox v)) (function
                | WireValue.Int i -> Some(box i)
                | _ -> None)
        elif t = typeof<float> then
            primitive t (fun v -> WireValue.Float(unbox v)) (function
                | WireValue.Float f -> Some(box f)
                | WireValue.Int i -> Some(box (float i))
                | _ -> None)
        elif t = typeof<int64> then
            primitive t (fun v -> WireValue.String(string v)) (function
                | WireValue.String s -> Some(box (Int64.Parse(s, CultureInfo.InvariantCulture)))
                | _ -> None)
        elif t = typeof<decimal> then
            primitive t (fun v -> WireValue.String(string v)) (function
                | WireValue.String s -> Some(box (Decimal.Parse(s, CultureInfo.InvariantCulture)))
                | _ -> None)
        elif t = typeof<Guid> then
            primitive t (fun v -> WireValue.String(string v)) (function
                | WireValue.String s -> Some(box (Guid.Parse s))
                | _ -> None)
        elif t = typeof<byte array> then
            primitive t (fun v -> WireValue.Bytes(unbox v)) (function
                | WireValue.Bytes b -> Some(box b)
                | WireValue.String s -> Some(box (Convert.FromBase64String s))
                | _ -> None)
        elif t.IsArray then
            let elementType = t.GetElementType()
            let element = child elementType

            mapping (fun v -> WireValue.Array [ for item in (v :?> IEnumerable) -> element.Encode item ]) (function
                | WireValue.Array items ->
                    let values = [| for item in items -> element.Decode item |]
#if FABLE_COMPILER
                    box values
#else
                    let array = Array.CreateInstance(elementType, values.Length)
                    values |> Array.iteri (fun i v -> array.SetValue(v, i))
                    box array
#endif
                | value -> fail t value)
        elif isGeneric typedefof<list<obj>> t then
            let element = child (t.GetGenericArguments()[0])
#if !FABLE_COMPILER
            // a list of the element type, built from its Cons and Empty cases
            let cases = FSharpType.GetUnionCases(t, true)
            let empty = FSharpValue.MakeUnion(cases[0], [||])
#endif

            mapping (fun v -> WireValue.Array [ for item in (v :?> IEnumerable) -> element.Encode item ]) (function
                | WireValue.Array items ->
                    let values = [ for item in items -> element.Decode item ]
#if FABLE_COMPILER
                    box values
#else
                    List.foldBack (fun (v: obj) tail -> FSharpValue.MakeUnion(cases[1], [| v; tail |])) values empty
#endif
                | value -> fail t value)
        elif isGeneric typedefof<option<obj>> t then
            let inner = child (t.GetGenericArguments()[0])
#if !FABLE_COMPILER
            let some = (FSharpType.GetUnionCases(t, true))[1]
#endif

            mapping
                (fun v ->
#if FABLE_COMPILER
                    match unbox<obj option> v with
                    | Some value -> inner.Encode value
                    | None -> WireValue.Nil
#else
                    if isNull v then
                        WireValue.Nil
                    else
                        let _, fields = FSharpValue.GetUnionFields(v, t, true)
                        inner.Encode fields[0]
#endif
                )
                (function
                | WireValue.Nil -> null
                | value ->
#if FABLE_COMPILER
                    box (Some(inner.Decode value))
#else
                    FSharpValue.MakeUnion(some, [| inner.Decode value |])
#endif
                )
        elif FSharpType.IsTuple t then
            let elements = FSharpType.GetTupleElements t |> Array.map child

            mapping
                (fun v ->
                    WireValue.Array(List.ofArray (Array.map2 (fun (m: WireMapping) f -> m.Encode f) elements (FSharpValue.GetTupleFields v))))
                (function
                | WireValue.Array items when items.Length = elements.Length ->
                    FSharpValue.MakeTuple(Array.map2 (fun (m: WireMapping) item -> m.Decode item) elements (Array.ofList items), t)
                | value -> fail t value)
        elif FSharpType.IsRecord(t, true) then
            let fields = [| for f in FSharpType.GetRecordFields(t, true) -> f.Name, child f.PropertyType |]

            mapping
                (fun v ->
                    let values = FSharpValue.GetRecordFields(v, true)
                    WireValue.Map [ for i, (name, m) in Array.indexed fields -> name, m.Encode values[i] ])
                (function
                | WireValue.Map members as value ->
                    let values = [|
                        for name, m in fields ->
                            match List.tryFind (fun (n, _) -> n = name) members with
                            | Some(_, field) -> m.Decode field
                            | None -> fail t value
                    |]

                    FSharpValue.MakeRecord(t, values, true)
                | value -> fail t value)
        elif FSharpType.IsUnion(t, true) then
            let cases = [|
                for case in FSharpType.GetUnionCases(t, true) -> case, [| for f in case.GetFields() -> child f.PropertyType |]
            |]

            let caseNamed name =
                cases |> Array.tryFind (fun (case: UnionCaseInfo, _) -> case.Name = name)

            mapping
                (fun v ->
                    let case, values = FSharpValue.GetUnionFields(v, t, true)
                    let _, fields = cases[case.Tag]

                    match fields with
                    | [||] -> WireValue.String case.Name
                    | [| field |] -> WireValue.Map [ case.Name, field.Encode values[0] ]
                    | _ -> WireValue.Map [ case.Name, WireValue.Array [ for i, f in Array.indexed fields -> f.Encode values[i] ] ])
                (fun value ->
                    match value with
                    | WireValue.String name ->
                        match caseNamed name with
                        | Some(case, [||]) -> FSharpValue.MakeUnion(case, [||], true)
                        | _ -> fail t value
                    | WireValue.Map [ name, inner ] ->
                        match caseNamed name, inner with
                        | Some(case, [| field |]), _ -> FSharpValue.MakeUnion(case, [| field.Decode inner |], true)
                        | Some(case, fields), WireValue.Array items when fields.Length = items.Length ->
                            FSharpValue.MakeUnion(case, Array.map2 (fun (f: WireMapping) item -> f.Decode item) fields (Array.ofList items), true)
                        | _ -> fail t value
                    | _ -> fail t value)
        else
            failwith $"Orsak.Remoting can't convert values of {t.FullName}: it supports F# records, unions, tuples, lists, options and arrays, of strings, bools, ints, floats, int64s, decimals, Guids and byte arrays"
