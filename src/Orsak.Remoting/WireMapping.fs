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
/// its value or <c>Nil</c>, or, when the value can itself be <c>Nil</c>, as in <c>Some None</c>, an <c>Array</c> of
/// the value; a map keyed by strings is a <c>Map</c>, and by anything else an <c>Array</c> of <c>[key, value]</c>
/// <c>Array</c>s; a set is an <c>Array</c>; a byte array is <c>Bytes</c>; a byte, an sbyte, an int16 and a uint16 are
/// <c>Int</c>s, read only within their bounds; an int64, a uint32, a uint64, a decimal and a Guid are strings, so
/// JavaScript's numbers don't round them; an enum's value is its name, or its number if it has none, such as flags
/// combined. Dates and times are ISO 8601 strings, the text of .NET's round-trip format:
/// a DateTime <c>2026-11-09T08:30:00.0000000Z</c>, or without the <c>Z</c> if its kind is unspecified, and a local one
/// as UTC; a DateTimeOffset with its offset, <c>+01:00</c>; a DateOnly <c>2026-11-09</c>; a TimeOnly
/// <c>08:30:00.0000000</c>; a TimeSpan <c>1.02:03:04.5000000</c>. On Fable, which keeps milliseconds, a fraction of a
/// millisecond is lost. Reading, a float may be an <c>Int</c>, and bytes a base64 <c>String</c>, for the
/// encodings that can't tell them apart. Lists and options are checked before unions, which they also are to
/// reflection.
/// </remarks>
module WireMapping =

    let private isGeneric (definition: Type) (t: Type) =
        t.IsGenericType && t.GetGenericTypeDefinition() = definition

    let private fail (t: Type) (value: WireValue) : obj =
        failwith $"Orsak.Remoting can't read %A{value} as {t.FullName}"

    let private mapping encode decode = { Encode = encode; Decode = decode }

    // Dates and times as ISO 8601 text, written and read here rather than by ToString and Parse, whose formats differ
    // between .NET and Fable: the text of .NET's round-trip format, "o", with seven digits of a second's fraction.

    let private pad width (n: int) = (string n).PadLeft(width, '0')

    let private ticksPerSecond = 10_000_000L

    /// The ticks of a second's fraction, as seven digits.
    let private fraction (ticks: int64) = pad 7 (int (ticks % ticksPerSecond))

    let private date (year: int) (month: int) (day: int) = $"{pad 4 year}-{pad 2 month}-{pad 2 day}"

    let private time (hour: int) (minute: int) (second: int) (ticks: int64) =
        $"{pad 2 hour}:{pad 2 minute}:{pad 2 second}.{fraction ticks}"

    let private offset (o: TimeSpan) =
        let sign = if o < TimeSpan.Zero then "-" else "+"
        $"{sign}{pad 2 (abs o.Hours)}:{pad 2 (abs o.Minutes)}"

    let private number (s: string) = Int32.Parse(s, CultureInfo.InvariantCulture)

    /// A time of day, "hh:mm:ss" with an optional fraction of any number of digits, as ticks, and what follows it.
    let private readTime (s: string) =
        let seconds = (number (s.Substring(0, 2)) * 60 + number (s.Substring(3, 2))) * 60 + number (s.Substring(6, 2))
        let mutable i = 8

        let digits =
            if i < s.Length && s[i] = '.' then
                i <- i + 1
                let start = i

                while i < s.Length && Char.IsDigit s[i] do
                    i <- i + 1

                s.Substring(start, i - start)
            else
                ""

        let fractionTicks = Int64.Parse((digits + "0000000").Substring(0, 7), CultureInfo.InvariantCulture)
        int64 seconds * ticksPerSecond + fractionTicks, s.Substring i

    /// A date and a time, "yyyy-MM-ddThh:mm:ss" with an optional fraction, as the date, the ticks since its midnight,
    /// and what follows them: a zone, "Z" or an offset, or nothing.
    let private readStamp (s: string) =
        let day = DateTime(number (s.Substring(0, 4)), number (s.Substring(5, 2)), number (s.Substring(8, 2)))
        let ticks, zone = readTime (s.Substring 11)
        day, ticks, zone

    /// An offset, "+hh:mm" or "-hh:mm", or "Z".
    let private readOffset (zone: string) =
        if zone = "Z" || zone = "" then
            TimeSpan.Zero
        else
            let o = TimeSpan(number (zone.Substring(1, 2)), number (zone.Substring(4, 2)), 0)
            if zone[0] = '-' then -o else o

    let private dateTimeText (d: DateTime) =
        let d = if d.Kind = DateTimeKind.Local then d.ToUniversalTime() else d
        let zone = if d.Kind = DateTimeKind.Utc then "Z" else ""
        $"{date d.Year d.Month d.Day}T{time d.Hour d.Minute d.Second d.Ticks}{zone}"

    let private readDateTime (s: string) =
        let day, ticks, zone = readStamp s

        // made from the date's parts, as Fable's DateTime and DateTimeOffset from another DateTime depend on the machine's
        // time zone
        match zone with
        | "" -> DateTime(day.Year, day.Month, day.Day, 0, 0, 0, DateTimeKind.Unspecified).Add(TimeSpan.FromTicks ticks)
        | "Z" -> DateTime(day.Year, day.Month, day.Day, 0, 0, 0, DateTimeKind.Utc).Add(TimeSpan.FromTicks ticks)
        | zone ->
            DateTimeOffset(day.Year, day.Month, day.Day, 0, 0, 0, readOffset zone).Add(TimeSpan.FromTicks ticks).UtcDateTime

    let private dateTimeOffsetText (d: DateTimeOffset) =
        $"{date d.Year d.Month d.Day}T{time d.Hour d.Minute d.Second d.Ticks}{offset d.Offset}"

    let private readDateTimeOffset (s: string) =
        let day, ticks, zone = readStamp s
        DateTimeOffset(day.Year, day.Month, day.Day, 0, 0, 0, readOffset zone).Add(TimeSpan.FromTicks ticks)

    /// A TimeSpan as .NET's constant format, "c": "[-][d.]hh:mm:ss[.fffffff]".
    let private timeSpanText (t: TimeSpan) =
        let sign = if t < TimeSpan.Zero then "-" else ""
        let t = t.Duration()
        let days = if t.Days > 0 then $"{t.Days}." else ""
        let ticks = t.Ticks % ticksPerSecond
        let fraction = if ticks = 0L then "" else $".{fraction ticks}"
        $"{sign}{days}{pad 2 t.Hours}:{pad 2 t.Minutes}:{pad 2 t.Seconds}{fraction}"

    let private readTimeSpan (s: string) =
        let negative = s.StartsWith "-"
        let s = if negative then s.Substring 1 else s
        let colon = s.IndexOf ':'
        let dot = s.IndexOf '.'
        let days, s = if dot >= 0 && dot < colon then number (s.Substring(0, dot)), s.Substring(dot + 1) else 0, s
        let ticks, _ = readTime s
        let t = TimeSpan.FromTicks(int64 days * TimeSpan.TicksPerDay + ticks)
        if negative then -t else t

    let private primitive (t: Type) (write: obj -> WireValue) (read: WireValue -> obj option) =
        mapping write (fun value ->
            match read value with
            | Some v -> v
            | None -> fail t value)

    /// An integer type smaller than int, as an Int, read only within the type's bounds.
    let private integer (t: Type) (low: int) (high: int) (toInt: obj -> int) (ofInt: int -> obj) =
        primitive t (fun v -> WireValue.Int(toInt v)) (function
            | WireValue.Int i when i >= low && i <= high -> Some(ofInt i)
            | _ -> None)

    /// An enum's value as its name, or as its number if it has none, such as flags combined.
    let private enumeration (t: Type) =
        let named = [ for name in Enum.GetNames t -> Enum.Parse(t, name), name ]

        primitive
            t
            (fun v ->
                match named |> List.tryFind (fun (value, _) -> value.Equals v) with
                | Some(_, name) -> WireValue.String name
#if FABLE_COMPILER
                | None -> WireValue.Int(unbox v))
#else
                | None -> WireValue.Int(Convert.ToInt32 v))
#endif
            (function
            | WireValue.String s -> named |> List.tryFind (fun (_, name) -> name = s) |> Option.map fst
#if FABLE_COMPILER
            | WireValue.Int i -> Some(box i)
#else
            | WireValue.Int i -> Some(Enum.ToObject(t, i))
#endif
            | _ -> None)

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
        elif t = typeof<byte> then
            integer t 0 255 (fun v -> int (unbox<byte> v)) (fun i -> box (byte i))
        elif t = typeof<sbyte> then
            integer t -128 127 (fun v -> int (unbox<sbyte> v)) (fun i -> box (sbyte i))
        elif t = typeof<int16> then
            integer t -32768 32767 (fun v -> int (unbox<int16> v)) (fun i -> box (int16 i))
        elif t = typeof<uint16> then
            integer t 0 65535 (fun v -> int (unbox<uint16> v)) (fun i -> box (uint16 i))
        elif t = typeof<uint32> then
            primitive t (fun v -> WireValue.String(string v)) (function
                | WireValue.String s -> Some(box (UInt32.Parse(s, CultureInfo.InvariantCulture)))
                | _ -> None)
        elif t = typeof<uint64> then
            primitive t (fun v -> WireValue.String(string v)) (function
                | WireValue.String s -> Some(box (UInt64.Parse(s, CultureInfo.InvariantCulture)))
                | _ -> None)
        elif t.IsEnum then
            enumeration t
        elif t = typeof<decimal> then
            primitive t (fun v -> WireValue.String(string v)) (function
                | WireValue.String s -> Some(box (Decimal.Parse(s, CultureInfo.InvariantCulture)))
                | _ -> None)
        elif t = typeof<Guid> then
            primitive t (fun v -> WireValue.String(string v)) (function
                | WireValue.String s -> Some(box (Guid.Parse s))
                | _ -> None)
        elif t = typeof<DateTime> then
            primitive t (fun v -> WireValue.String(dateTimeText (unbox v))) (function
                | WireValue.String s -> Some(box (readDateTime s))
                | _ -> None)
        elif t = typeof<DateTimeOffset> then
            primitive t (fun v -> WireValue.String(dateTimeOffsetText (unbox v))) (function
                | WireValue.String s -> Some(box (readDateTimeOffset s))
                | _ -> None)
        elif t = typeof<TimeSpan> then
            primitive t (fun v -> WireValue.String(timeSpanText (unbox v))) (function
                | WireValue.String s -> Some(box (readTimeSpan s))
                | _ -> None)
        elif t = typeof<DateOnly> then
            primitive
                t
                (fun v ->
                    let d = unbox<DateOnly> v
                    WireValue.String(date d.Year d.Month d.Day))
                (function
                | WireValue.String s ->
                    Some(box (DateOnly(number (s.Substring(0, 4)), number (s.Substring(5, 2)), number (s.Substring(8, 2)))))
                | _ -> None)
        elif t = typeof<TimeOnly> then
            primitive
                t
                (fun v ->
                    let d = unbox<TimeOnly> v
                    WireValue.String(time d.Hour d.Minute d.Second d.Ticks))
                (function
                | WireValue.String s -> Some(box (TimeOnly.FromTimeSpan(TimeSpan.FromTicks(fst (readTime s)))))
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
        elif isGeneric typedefof<Map<string, obj>> t then
            let keyType, valueType = t.GetGenericArguments()[0], t.GetGenericArguments()[1]
            let key, value = child keyType, child valueType
#if !FABLE_COMPILER
            let pair = typedefof<Collections.Generic.KeyValuePair<obj, obj>>.MakeGenericType(keyType, valueType)
            let pairKey, pairValue = pair.GetProperty "Key", pair.GetProperty "Value"
            let tupleType = FSharpType.MakeTupleType [| keyType; valueType |]
#endif

            let pairs (v: obj) : (obj * obj) list =
#if FABLE_COMPILER
                [ for KeyValue(k, v) in unbox<Map<IComparable, obj>> v -> box k, v ]
#else
                [ for p in (v :?> IEnumerable) -> pairKey.GetValue p, pairValue.GetValue p ]
#endif

            let make (pairs: (obj * obj) list) : obj =
#if FABLE_COMPILER
                box (Map.ofList [ for k, v in pairs -> unbox<IComparable> k, v ])
#else
                let tuples = Array.CreateInstance(tupleType, pairs.Length)
                pairs |> List.iteri (fun i (k, v) -> tuples.SetValue(FSharpValue.MakeTuple([| k; v |], tupleType), i))
                Activator.CreateInstance(t, [| box tuples |])
#endif

            // keyed by strings, a map is a Map, as a JSON object; keyed by anything else, an Array of [key, value]
            if keyType = typeof<string> then
                mapping (fun v -> WireValue.Map [ for k, v in pairs v -> unbox<string> k, value.Encode v ]) (function
                    | WireValue.Map members -> make [ for k, v in members -> box k, value.Decode v ]
                    | other -> fail t other)
            else
                mapping
                    (fun v -> WireValue.Array [ for k, v in pairs v -> WireValue.Array [ key.Encode k; value.Encode v ] ])
                    (function
                    | WireValue.Array items ->
                        make [
                            for item in items ->
                                match item with
                                | WireValue.Array [ k; v ] -> key.Decode k, value.Decode v
                                | other -> fail t other, null
                        ]
                    | other -> fail t other)
        elif isGeneric typedefof<Set<int>> t then
            let element = child (t.GetGenericArguments()[0])

            mapping (fun v -> WireValue.Array [ for item in (v :?> IEnumerable) -> element.Encode item ]) (function
                | WireValue.Array items ->
                    let values = [| for item in items -> element.Decode item |]
#if FABLE_COMPILER
                    box (Set.ofArray (Array.map unbox<IComparable> values))
#else
                    let typed = Array.CreateInstance(t.GetGenericArguments()[0], values.Length)
                    values |> Array.iteri (fun i v -> typed.SetValue(v, i))
                    Activator.CreateInstance(t, [| box typed |])
#endif
                | value -> fail t value)
        elif isGeneric typedefof<option<obj>> t then
            let valueType = t.GetGenericArguments()[0]
            let inner = child valueType
            // a value that can itself be Nil, such as None in Some None, is in an Array of one, so it isn't read as None
            let nested = valueType = typeof<unit> || isGeneric typedefof<option<obj>> valueType
            let encodeSome v = if nested then WireValue.Array [ inner.Encode v ] else inner.Encode v
#if !FABLE_COMPILER
            let some = (FSharpType.GetUnionCases(t, true))[1]
#endif

            mapping
                (fun v ->
#if FABLE_COMPILER
                    match unbox<obj option> v with
                    | Some value -> encodeSome value
                    | None -> WireValue.Nil
#else
                    if isNull v then
                        WireValue.Nil
                    else
                        let _, fields = FSharpValue.GetUnionFields(v, t, true)
                        encodeSome fields[0]
#endif
                )
                (fun value ->
                    let decoded =
                        match value with
                        | WireValue.Nil -> None
                        | WireValue.Array [ v ] when nested -> Some(inner.Decode v)
                        | v when not nested -> Some(inner.Decode v)
                        | v -> Some(fail t v)

                    match decoded with
                    | None -> null
                    | Some v ->
#if FABLE_COMPILER
                        box (Some v)
#else
                        FSharpValue.MakeUnion(some, [| v |])
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
            failwith $"Orsak.Remoting can't convert values of {t.FullName}: it supports F# records, unions, tuples, lists, options, arrays, maps and sets, of strings, bools, integers, floats, decimals, enums, Guids, byte arrays, DateTimes, DateTimeOffsets, DateOnlys, TimeOnlys and TimeSpans"
