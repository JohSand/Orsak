namespace Orsak.Remoting

open System
open System.Collections
open System.Globalization
open Microsoft.FSharp.Reflection

/// Converts F# values to JSON and back, by reflection on their types, which .NET and Fable both have for F# types.
///
/// The JSON: a record is an object of its fields; a union case without fields is its name, one with fields an object
/// of its name and its field, or an array of its fields; a list, an array or a tuple is an array; an option is its value
/// or null; an int64, a decimal and a Guid are strings, so JavaScript's numbers don't round them. Lists and options are
/// checked before unions, which they also are, to F# reflection.
module JsonCodec =

    let private isGeneric (definition: Type) (t: Type) =
        t.IsGenericType && t.GetGenericTypeDefinition() = definition

    let private number (value: obj) =
        match value with
        | :? float as f -> f.ToString("R", CultureInfo.InvariantCulture)
        | _ -> string value

    let rec encode (t: Type) (value: obj) : Json =
        if t = typeof<unit> then JNull
        elif t = typeof<string> then (if isNull value then JNull else JString(unbox value))
        elif t = typeof<bool> then JBool(unbox value)
        elif t = typeof<int> || t = typeof<float> then JNumber(number value)
        elif t = typeof<int64> || t = typeof<decimal> || t = typeof<Guid> then JString(string value)
        elif t.IsArray then JArray [ for item in (value :?> IEnumerable) -> encode (t.GetElementType()) item ]
        elif isGeneric typedefof<list<_>> t then
            JArray [ for item in (value :?> IEnumerable) -> encode (t.GetGenericArguments()[0]) item ]
        elif isGeneric typedefof<option<_>> t then
            let inner = t.GetGenericArguments()[0]
#if FABLE_COMPILER
            match unbox<obj option> value with
            | Some v -> encode inner v
            | None -> JNull
#else
            if isNull value then
                JNull
            else
                let _, fields = FSharpValue.GetUnionFields(value, t)
                encode inner fields[0]
#endif
        elif FSharpType.IsTuple t then
            JArray(List.ofArray (Array.map2 encode (FSharpType.GetTupleElements t) (FSharpValue.GetTupleFields value)))
        elif FSharpType.IsRecord t then
            let values = FSharpValue.GetRecordFields value

            JObject [
                for i, field in Array.indexed (FSharpType.GetRecordFields t) -> field.Name, encode field.PropertyType values[i]
            ]
        elif FSharpType.IsUnion t then
            let case, values = FSharpValue.GetUnionFields(value, t)

            match case.GetFields() with
            | [||] -> JString case.Name
            | [| field |] -> JObject [ case.Name, encode field.PropertyType values[0] ]
            | fields -> JObject [ case.Name, JArray [ for i, f in Array.indexed fields -> encode f.PropertyType values[i] ] ]
        else
            failwith $"Orsak.Remoting can't encode a value of {t.FullName}"

    let private fail (t: Type) (json: Json) =
        failwith $"Orsak.Remoting can't decode {Json.write json} as {t.FullName}"

    let rec decode (t: Type) (json: Json) : obj =
        match json with
        | _ when t = typeof<unit> -> box ()
        | JNull when t = typeof<string> -> null
        | JString s when t = typeof<string> -> box s
        | JBool b when t = typeof<bool> -> box b
        | JNumber n when t = typeof<int> -> box (Int32.Parse(n, CultureInfo.InvariantCulture))
        | JNumber n when t = typeof<float> -> box (Double.Parse(n, CultureInfo.InvariantCulture))
        | JString s when t = typeof<int64> -> box (Int64.Parse(s, CultureInfo.InvariantCulture))
        | JString s when t = typeof<decimal> -> box (Decimal.Parse(s, CultureInfo.InvariantCulture))
        | JString s when t = typeof<Guid> -> box (Guid.Parse s)
        | JArray items when t.IsArray ->
            let element = t.GetElementType()
            let values = [| for item in items -> decode element item |]
#if FABLE_COMPILER
            box values
#else
            let array = Array.CreateInstance(element, values.Length)
            values |> Array.iteri (fun i v -> array.SetValue(v, i))
            box array
#endif
        | JArray items when isGeneric typedefof<list<_>> t ->
            let element = t.GetGenericArguments()[0]
            let values = [ for item in items -> decode element item ]
#if FABLE_COMPILER
            box values
#else
            // a list of the element type, built from its Cons and Empty cases
            let cases = FSharpType.GetUnionCases t
            let empty = FSharpValue.MakeUnion(cases[0], [||])
            List.foldBack (fun v tail -> FSharpValue.MakeUnion(cases[1], [| v; tail |])) values empty
#endif
        | _ when isGeneric typedefof<option<_>> t ->
            match json with
            | JNull -> null
            | _ ->
                let value = decode (t.GetGenericArguments()[0]) json
#if FABLE_COMPILER
                box (Some value)
#else
                FSharpValue.MakeUnion((FSharpType.GetUnionCases t)[1], [| value |])
#endif
        | JArray items when FSharpType.IsTuple t ->
            let values = Array.map2 decode (FSharpType.GetTupleElements t) (Array.ofList items)
            FSharpValue.MakeTuple(values, t)
        | JObject fields when FSharpType.IsRecord t ->
            let values = [|
                for field in FSharpType.GetRecordFields t ->
                    match List.tryFind (fun (name, _) -> name = field.Name) fields with
                    | Some(_, value) -> decode field.PropertyType value
                    | None -> fail t json
            |]

            FSharpValue.MakeRecord(t, values)
        | JString name when FSharpType.IsUnion t ->
            match FSharpType.GetUnionCases t |> Array.tryFind (fun c -> c.Name = name) with
            | Some case -> FSharpValue.MakeUnion(case, [||])
            | None -> fail t json
        | JObject [ name, value ] when FSharpType.IsUnion t ->
            match FSharpType.GetUnionCases t |> Array.tryFind (fun c -> c.Name = name) with
            | Some case ->
                match case.GetFields(), value with
                | [| field |], _ -> FSharpValue.MakeUnion(case, [| decode field.PropertyType value |])
                | fields, JArray items when fields.Length = items.Length ->
                    let values = Array.map2 (fun (f: Reflection.PropertyInfo) item -> decode f.PropertyType item) fields (Array.ofList items)
                    FSharpValue.MakeUnion(case, values)
                | _ -> fail t json
            | None -> fail t json
        | _ -> fail t json
