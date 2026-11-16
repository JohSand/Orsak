namespace Orsak.Remoting

// Values of any type, converted by reflection: obj is the type meant, wherever it is inferred
#nowarn "3559"

open System
open System.Collections
open System.Globalization
open Microsoft.FSharp.Reflection
#if FABLE_COMPILER
open Fable.Core
#endif

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

#if !FABLE_COMPILER
/// Collections of an element type known only at run time, made by these methods of the type, as delegates made once
/// per type, rather than by a reflection call per item.
type internal TypedCollections =
    static member List<'T>(values: obj array) : obj = box (List.ofArray (Array.map unbox<'T> values))
    static member Array<'T>(values: obj array) : obj = box (Array.map unbox<'T> values)
    static member Set<'T when 'T: comparison>(values: obj array) : obj = box (Set.ofArray (Array.map unbox<'T> values))

    static member HashSet<'T>(values: obj array) : obj =
        box (Collections.Generic.HashSet<'T>(Array.map unbox<'T> values))

    static member Map<'K, 'V when 'K: comparison>(pairs: (obj * obj) list) : obj =
        box (Map.ofList [ for k, v in pairs -> unbox<'K> k, unbox<'V> v ])

    static member Pairs<'K, 'V>(v: obj) : (obj * obj) list =
        let pairs = v :?> Collections.Generic.IEnumerable<Collections.Generic.KeyValuePair<'K, 'V>>
        [ for p in pairs -> box p.Key, box p.Value ]

    /// The delegate of the method of this type with the name, of the type arguments.
    static member Of<'F when 'F :> Delegate>(name: string, [<ParamArray>] types: Type array) : 'F =
        typeof<TypedCollections>
            .GetMethod(name, Reflection.BindingFlags.Static ||| Reflection.BindingFlags.Public ||| Reflection.BindingFlags.NonPublic)
            .MakeGenericMethod(types)
            .CreateDelegate<'F>()
#endif

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
/// the value; a map or a Dictionary keyed by strings is a <c>Map</c>, and by anything else an <c>Array</c> of <c>[key, value]</c>
/// <c>Array</c>s; a set, a ResizeArray or a HashSet is an <c>Array</c>; a float32 is the <c>Float</c> of its shortest text, 0.1 for 0.1f; a byte array is <c>Bytes</c>; a byte, an sbyte, an int16 and a uint16 are
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

#if FABLE_COMPILER
    [<Emit("Number($0.toPrecision($1))")>]
    let private toPrecision (f: float) (digits: int) : float = jsNative

    [<Emit("Math.fround($0)")>]
    let private fround (f: float) : float = jsNative
#endif

    /// A float32 as the float of its shortest text, 0.1 for 0.1f rather than its binary value, 0.10000000149011612: the
    /// fewest significant digits that read back as the same float32, as .NET's "R" finds them.
    let private shortest (f: float32) : float =
#if FABLE_COMPILER
        let f = float f
        let rec digits n =
            let d = toPrecision f n
            if n >= 9 || fround d = f then d else digits (n + 1)

        if Double.IsNaN f || Double.IsInfinity f || f = 0.0 then f else digits 1
#else
        Double.Parse(f.ToString("R", CultureInfo.InvariantCulture), CultureInfo.InvariantCulture)
#endif

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

    // Reading and making records, unions and tuples: on .NET, readers and constructors made once per type, as
    // FSharpValue's functions look the type's members up on every call; on Fable, those functions, which don't.

    let private recordReader (t: Type) : obj -> obj array =
#if FABLE_COMPILER
        // a value knows its type
        ignore t
        fun v -> FSharpValue.GetRecordFields(v, true)
#else
        FSharpValue.PreComputeRecordReader(t, true)
#endif

    let private recordConstructor (t: Type) : obj array -> obj =
#if FABLE_COMPILER
        fun values -> FSharpValue.MakeRecord(t, values, true)
#else
        FSharpValue.PreComputeRecordConstructor(t, true)
#endif

    let private tupleReader (t: Type) : obj -> obj array =
#if FABLE_COMPILER
        ignore t
        FSharpValue.GetTupleFields
#else
        FSharpValue.PreComputeTupleReader t
#endif

    let private tupleConstructor (t: Type) : obj array -> obj =
#if FABLE_COMPILER
        fun values -> FSharpValue.MakeTuple(values, t)
#else
        FSharpValue.PreComputeTupleConstructor t
#endif

    /// A union's tag, of the case of a value.
    let private unionTagReader (t: Type) : obj -> int =
#if FABLE_COMPILER
        fun v -> (fst (FSharpValue.GetUnionFields(v, t, true))).Tag
#else
        FSharpValue.PreComputeUnionTagReader(t, true)
#endif

    let private unionReader (t: Type) (case: UnionCaseInfo) : obj -> obj array =
#if FABLE_COMPILER
        // the value knows its case
        ignore case
        fun v -> snd (FSharpValue.GetUnionFields(v, t, true))
#else
        // the case knows its type
        ignore t
        FSharpValue.PreComputeUnionReader(case, true)
#endif

    let private unionConstructor (case: UnionCaseInfo) : obj array -> obj =
#if FABLE_COMPILER
        fun values -> FSharpValue.MakeUnion(case, values, true)
#else
        FSharpValue.PreComputeUnionConstructor(case, true)
#endif

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
        elif t = typeof<float32> then
            primitive t (fun v -> WireValue.Float(shortest (unbox v))) (function
                | WireValue.Float f -> Some(box (float32 f))
                | WireValue.Int i -> Some(box (float32 i))
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
#if !FABLE_COMPILER
            let make = TypedCollections.Of<Func<obj array, obj>>("Array", elementType)
#endif

            mapping (fun v -> WireValue.Array [ for item in (v :?> IEnumerable) -> element.Encode item ]) (function
                | WireValue.Array items ->
                    let values = [| for item in items -> element.Decode item |]
#if FABLE_COMPILER
                    box values
#else
                    make.Invoke values
#endif
                | value -> fail t value)
        elif isGeneric typedefof<list<obj>> t then
            let element = child (t.GetGenericArguments()[0])
#if !FABLE_COMPILER
            let make = TypedCollections.Of<Func<obj array, obj>>("List", t.GetGenericArguments()[0])
#endif

            mapping (fun v -> WireValue.Array [ for item in (v :?> IEnumerable) -> element.Encode item ]) (function
                | WireValue.Array items ->
#if FABLE_COMPILER
                    box [ for item in items -> element.Decode item ]
#else
                    make.Invoke [| for item in items -> element.Decode item |]
#endif
                | value -> fail t value)
        elif isGeneric typedefof<ResizeArray<obj>> t then
            let element = child (t.GetGenericArguments()[0])

            mapping (fun v -> WireValue.Array [ for item in (v :?> IEnumerable) -> element.Encode item ]) (function
                | WireValue.Array items ->
                    let values = [ for item in items -> element.Decode item ]
#if FABLE_COMPILER
                    box (ResizeArray<obj> values)
#else
                    let list = Activator.CreateInstance t :?> IList
                    values |> List.iter (list.Add >> ignore)
                    box list
#endif
                | value -> fail t value)
        elif isGeneric typedefof<Generic.HashSet<obj>> t then
            let element = child (t.GetGenericArguments()[0])
#if !FABLE_COMPILER
            let make = TypedCollections.Of<Func<obj array, obj>>("HashSet", t.GetGenericArguments()[0])
#endif

            mapping (fun v -> WireValue.Array [ for item in (v :?> IEnumerable) -> element.Encode item ]) (function
                | WireValue.Array items ->
#if FABLE_COMPILER
                    box (Generic.HashSet<obj> [ for item in items -> element.Decode item ])
#else
                    make.Invoke [| for item in items -> element.Decode item |]
#endif
                | value -> fail t value)
        elif isGeneric typedefof<Map<string, obj>> t || isGeneric typedefof<Generic.Dictionary<string, obj>> t then
            let isMap = isGeneric typedefof<Map<string, obj>> t
            let keyType, valueType = t.GetGenericArguments()[0], t.GetGenericArguments()[1]
            let key, value = child keyType, child valueType
#if !FABLE_COMPILER
            let readPairs = TypedCollections.Of<Func<obj, (obj * obj) list>>("Pairs", keyType, valueType)
            let makeMap = TypedCollections.Of<Func<(obj * obj) list, obj>>("Map", keyType, valueType)
#endif

            let pairs (v: obj) : (obj * obj) list =
#if FABLE_COMPILER
                if isMap then
                    [ for KeyValue(k, v) in unbox<Map<IComparable, obj>> v -> box k, v ]
                else
                    [ for KeyValue(k, v) in unbox<Generic.Dictionary<obj, obj>> v -> k, v ]
#else
                readPairs.Invoke v
#endif

            // a key read twice keeps its last value, as in a Map
            let make (pairs: (obj * obj) list) : obj =
#if FABLE_COMPILER
                if isMap then
                    box (Map.ofList [ for k, v in pairs -> unbox<IComparable> k, v ])
                else
                    let dictionary = Generic.Dictionary<obj, obj>()
                    pairs |> List.iter (fun (k, v) -> dictionary[k] <- v)
                    box dictionary
#else
                if isMap then
                    makeMap.Invoke pairs
                else
                    let dictionary = Activator.CreateInstance t :?> IDictionary
                    pairs |> List.iter (fun (k, v) -> dictionary[k] <- v)
                    box dictionary
#endif

            // keyed by strings, a map or a dictionary is a Map, as a JSON object; keyed by anything else, an Array of
            // [key, value]
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
#if !FABLE_COMPILER
            let make = TypedCollections.Of<Func<obj array, obj>>("Set", t.GetGenericArguments()[0])
#endif

            mapping (fun v -> WireValue.Array [ for item in (v :?> IEnumerable) -> element.Encode item ]) (function
                | WireValue.Array items ->
                    let values = [| for item in items -> element.Decode item |]
#if FABLE_COMPILER
                    box (Set.ofArray (Array.map unbox<IComparable> values))
#else
                    make.Invoke values
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
            let readSome = unionReader t some
            let makeSome = unionConstructor some
#endif

            mapping
                (fun v ->
#if FABLE_COMPILER
                    match unbox<obj option> v with
                    | Some value -> encodeSome value
                    | None -> WireValue.Nil
#else
                    if isNull v then WireValue.Nil else encodeSome (readSome v).[0]
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
                        makeSome [| v |]
#endif
                )
        elif FSharpType.IsTuple t then
            let elements = FSharpType.GetTupleElements t |> Array.map child
            let read, make = tupleReader t, tupleConstructor t

            mapping
                (fun v -> WireValue.Array(List.ofArray (Array.map2 (fun (m: WireMapping) f -> m.Encode f) elements (read v))))
                (function
                | WireValue.Array items when items.Length = elements.Length ->
                    make (Array.map2 (fun (m: WireMapping) item -> m.Decode item) elements (Array.ofList items))
                | value -> fail t value)
        elif FSharpType.IsRecord(t, true) then
            let fields = [| for f in FSharpType.GetRecordFields(t, true) -> f.Name, child f.PropertyType |]
            let read, make = recordReader t, recordConstructor t

            mapping
                (fun v ->
                    let values = read v
                    WireValue.Map [ for i, (name, m) in Array.indexed fields -> name, m.Encode values[i] ])
                (function
                | WireValue.Map members as value ->
                    let members = Array.ofList members

                    let values =
                        fields
                        |> Array.mapi (fun i (name, m: WireMapping) ->
                            // in the fields' order, as written, or else looked for
                            if i < members.Length && fst members[i] = name then
                                m.Decode(snd members[i])
                            else
                                match Array.tryFind (fun (n, _) -> n = name) members with
                                | Some(_, field) -> m.Decode field
                                | None -> fail t value)

                    make values
                | value -> fail t value)
        elif FSharpType.IsUnion(t, true) then
            let cases = [|
                for case in FSharpType.GetUnionCases(t, true) ->
                    case, [| for f in case.GetFields() -> child f.PropertyType |], unionReader t case, unionConstructor case
            |]

            let tag = unionTagReader t

            let caseNamed name =
                cases |> Array.tryFind (fun (case: UnionCaseInfo, _, _, _) -> case.Name = name)

            mapping
                (fun v ->
                    let case, fields, read, _ = cases[tag v]

                    match fields with
                    | [||] -> WireValue.String case.Name
                    | [| field |] -> WireValue.Map [ case.Name, field.Encode((read v).[0]) ]
                    | _ ->
                        let values = read v
                        WireValue.Map [ case.Name, WireValue.Array [ for i, f in Array.indexed fields -> f.Encode values[i] ] ])
                (fun value ->
                    match value with
                    | WireValue.String name ->
                        match caseNamed name with
                        | Some(_, [||], _, make) -> make [||]
                        | _ -> fail t value
                    | WireValue.Map [ name, inner ] ->
                        match caseNamed name, inner with
                        | Some(_, [| field |], _, make), _ -> make [| field.Decode inner |]
                        | Some(_, fields, _, make), WireValue.Array items when fields.Length = items.Length ->
                            make (Array.map2 (fun (f: WireMapping) item -> f.Decode item) fields (Array.ofList items))
                        | _ -> fail t value
                    | _ -> fail t value)
        else
            failwith $"Orsak.Remoting can't convert values of {t.FullName}: it supports F# records, unions, tuples, lists, options, arrays, maps and sets, ResizeArrays, HashSets and Dictionaries, of strings, bools, integers, floats, decimals, enums, Guids, byte arrays, DateTimes, DateTimeOffsets, DateOnlys, TimeOnlys and TimeSpans"
