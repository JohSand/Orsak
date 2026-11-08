namespace Orsak.Remoting

// Values of any type, converted by reflection: obj is the type meant, wherever it is inferred
#nowarn "3559"

open System
open System.Collections
open System.Globalization
open Microsoft.FSharp.Reflection

/// <summary>
/// Converts the values of one type to JSON and back, compiled once for the type, by <c>JsonCodec.compile</c>.
/// </summary>
type JsonCodec = {
    /// Writes a value of the type as JSON.
    Encode: obj -> Json
    /// Reads a value of the type from JSON.
    Decode: Json -> obj
}

/// <summary>
/// Compiles <see cref="T:Orsak.Remoting.JsonCodec"/>s for F# types, by reflection on their types, which .NET and Fable both
/// have for F# types. A codec is compiled once, for every value it converts after, and compiling it fails for a type it
/// doesn't support, so that a client or server finds out when it starts, not on its first call.
/// </summary>
/// <remarks>
/// The JSON: a record is an object of its fields; a union case without fields is its name, one with fields an object
/// of its name and its field, or an array of its fields; a list, an array or a tuple is an array; an option is its value
/// or null; an int64, a decimal and a Guid are strings, so JavaScript's numbers don't round them; numbers are written
/// and read in the invariant culture. Lists and options are checked before unions, which they also are to reflection.
/// </remarks>
module JsonCodec =

    let private isGeneric (definition: Type) (t: Type) =
        t.IsGenericType && t.GetGenericTypeDefinition() = definition

    let private fail (t: Type) (json: Json) : obj =
        failwith $"Orsak.Remoting can't decode {Json.write json} as {t.FullName}"

    let private codec encode decode = { Encode = encode; Decode = decode }

    let private primitive (t: Type) (write: obj -> Json) (read: Json -> obj option) =
        codec write (fun json ->
            match read json with
            | Some value -> value
            | None -> fail t json)

    /// The codec of a field or an element, compiled when first used, so a type that contains itself, such as a tree,
    /// is compiled once rather than without end.
    let rec private child (t: Type) =
        let compiled = lazy (compile t)
        codec (fun value -> compiled.Value.Encode value) (fun json -> compiled.Value.Decode json)

    /// <summary>Compiles the codec of <paramref name="t"/>, or fails if its type isn't supported.</summary>
    /// <param name="t">The type whose values the codec converts</param>
    and compile (t: Type) : JsonCodec =
        if t = typeof<unit> then
            codec (fun _ -> JNull) (fun _ -> box ())
        elif t = typeof<string> then
            primitive t (fun v -> if isNull v then JNull else JString(unbox v)) (function
                | JString s -> Some(box s)
                | JNull -> Some null
                | _ -> None)
        elif t = typeof<bool> then
            primitive t (fun v -> JBool(unbox v)) (function
                | JBool b -> Some(box b)
                | _ -> None)
        elif t = typeof<int> then
            primitive t (fun v -> JNumber(string (unbox<int> v))) (function
                | JNumber n -> Some(box (Int32.Parse(n, CultureInfo.InvariantCulture)))
                | _ -> None)
        elif t = typeof<float> then
            primitive t (fun v -> JNumber((unbox<float> v).ToString("R", CultureInfo.InvariantCulture))) (function
                | JNumber n -> Some(box (Double.Parse(n, CultureInfo.InvariantCulture)))
                | _ -> None)
        elif t = typeof<int64> then
            primitive t (fun v -> JString(string v)) (function
                | JString s -> Some(box (Int64.Parse(s, CultureInfo.InvariantCulture)))
                | _ -> None)
        elif t = typeof<decimal> then
            primitive t (fun v -> JString(string v)) (function
                | JString s -> Some(box (Decimal.Parse(s, CultureInfo.InvariantCulture)))
                | _ -> None)
        elif t = typeof<Guid> then
            primitive t (fun v -> JString(string v)) (function
                | JString s -> Some(box (Guid.Parse s))
                | _ -> None)
        elif t.IsArray then
            let elementType = t.GetElementType()
            let element = child elementType

            codec (fun v -> JArray [ for item in (v :?> IEnumerable) -> element.Encode item ]) (function
                | JArray items ->
                    let values = [| for item in items -> element.Decode item |]
#if FABLE_COMPILER
                    box values
#else
                    let array = Array.CreateInstance(elementType, values.Length)
                    values |> Array.iteri (fun i v -> array.SetValue(v, i))
                    box array
#endif
                | json -> fail t json)
        elif isGeneric typedefof<list<obj>> t then
            let element = child (t.GetGenericArguments()[0])
#if !FABLE_COMPILER
            // a list of the element type, built from its Cons and Empty cases
            let cases = FSharpType.GetUnionCases t
            let empty = FSharpValue.MakeUnion(cases[0], [||])
#endif

            codec (fun v -> JArray [ for item in (v :?> IEnumerable) -> element.Encode item ]) (function
                | JArray items ->
                    let values = [ for item in items -> element.Decode item ]
#if FABLE_COMPILER
                    box values
#else
                    List.foldBack (fun (v: obj) tail -> FSharpValue.MakeUnion(cases[1], [| v; tail |])) values empty
#endif
                | json -> fail t json)
        elif isGeneric typedefof<option<obj>> t then
            let inner = child (t.GetGenericArguments()[0])
#if !FABLE_COMPILER
            let some = (FSharpType.GetUnionCases t)[1]
#endif

            codec
                (fun v ->
#if FABLE_COMPILER
                    match unbox<obj option> v with
                    | Some value -> inner.Encode value
                    | None -> JNull
#else
                    if isNull v then
                        JNull
                    else
                        let _, fields = FSharpValue.GetUnionFields(v, t)
                        inner.Encode fields[0]
#endif
                )
                (function
                | JNull -> null
                | json ->
#if FABLE_COMPILER
                    box (Some(inner.Decode json))
#else
                    FSharpValue.MakeUnion(some, [| inner.Decode json |])
#endif
                )
        elif FSharpType.IsTuple t then
            let elements = FSharpType.GetTupleElements t |> Array.map child

            codec (fun v -> JArray(List.ofArray (Array.map2 (fun (c: JsonCodec) f -> c.Encode f) elements (FSharpValue.GetTupleFields v)))) (function
                | JArray items when items.Length = elements.Length ->
                    FSharpValue.MakeTuple(Array.map2 (fun (c: JsonCodec) item -> c.Decode item) elements (Array.ofList items), t)
                | json -> fail t json)
        elif FSharpType.IsRecord t then
            let fields = [| for f in FSharpType.GetRecordFields t -> f.Name, child f.PropertyType |]

            codec
                (fun v ->
                    let values = FSharpValue.GetRecordFields v
                    JObject [ for i, (name, c) in Array.indexed fields -> name, c.Encode values[i] ])
                (function
                | JObject members as json ->
                    let values = [|
                        for name, c in fields ->
                            match List.tryFind (fun (n, _) -> n = name) members with
                            | Some(_, value) -> c.Decode value
                            | None -> fail t json
                    |]

                    FSharpValue.MakeRecord(t, values)
                | json -> fail t json)
        elif FSharpType.IsUnion t then
            let cases = [|
                for case in FSharpType.GetUnionCases t -> case, [| for f in case.GetFields() -> child f.PropertyType |]
            |]

            let caseNamed name =
                cases |> Array.tryFind (fun (case: UnionCaseInfo, _) -> case.Name = name)

            codec
                (fun v ->
                    let case, values = FSharpValue.GetUnionFields(v, t)
                    let _, fields = cases[case.Tag]

                    match fields with
                    | [||] -> JString case.Name
                    | [| field |] -> JObject [ case.Name, field.Encode values[0] ]
                    | _ -> JObject [ case.Name, JArray [ for i, f in Array.indexed fields -> f.Encode values[i] ] ])
                (fun json ->
                    match json with
                    | JString name ->
                        match caseNamed name with
                        | Some(case, [||]) -> FSharpValue.MakeUnion(case, [||])
                        | _ -> fail t json
                    | JObject [ name, value ] ->
                        match caseNamed name, value with
                        | Some(case, [| field |]), _ -> FSharpValue.MakeUnion(case, [| field.Decode value |])
                        | Some(case, fields), JArray items when fields.Length = items.Length ->
                            FSharpValue.MakeUnion(case, Array.map2 (fun (f: JsonCodec) item -> f.Decode item) fields (Array.ofList items))
                        | _ -> fail t json
                    | _ -> fail t json)
        else
            failwith $"Orsak.Remoting can't convert values of {t.FullName}: it supports F# records, unions, tuples, lists, options and arrays, of strings, bools, ints, floats, int64s, decimals and Guids"
