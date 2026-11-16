namespace Orsak.Remoting

open System
open System.Globalization
open System.Text

/// <summary>
/// Writes and parses JSON text, in plain F#, so .NET and Fable read and write it the same way. <c>Bytes</c> are a base64
/// string, and numbers are in the invariant culture: a number without a fraction or an exponent that fits an int is
/// read as an <c>Int</c>, any other as a <c>Float</c>.
/// </summary>
module Json =

    /// Whether a character is escaped in a JSON string.
    let private mustEscape (c: char) = c = '"' || c = '\\' || c < ' '

    let private writeEscaped (sb: StringBuilder) (s: string) =
        for c in s do
            match c with
            | '"' -> sb.Append "\\\"" |> ignore
            | '\\' -> sb.Append "\\\\" |> ignore
            | '\n' -> sb.Append "\\n" |> ignore
            | '\r' -> sb.Append "\\r" |> ignore
            | '\t' -> sb.Append "\\t" |> ignore
            | '\b' -> sb.Append "\\b" |> ignore
            | '\f' -> sb.Append "\\f" |> ignore
            | c when c < ' ' -> sb.Append("\\u").Append((int c).ToString("x4")) |> ignore
            | c -> sb.Append c |> ignore

    let private writeString (sb: StringBuilder) (s: string) =
        sb.Append '"' |> ignore
        let mutable first = 0

        while first < s.Length && not (mustEscape s[first]) do
            first <- first + 1

        // most strings have nothing to escape, and are written whole
        if first = s.Length then
            sb.Append s |> ignore
        else
            sb.Append(s.Substring(0, first)) |> ignore
            writeEscaped sb (s.Substring first)

        sb.Append '"' |> ignore

    let rec private writeTo (sb: StringBuilder) (digits: char array) json =
        match json with
        | WireValue.Nil -> sb.Append "null" |> ignore
        | WireValue.Bool b -> sb.Append(if b then "true" else "false") |> ignore
#if FABLE_COMPILER
        | WireValue.Int i ->
            ignore digits
            sb.Append(string i) |> ignore
#else
        // into the buffer of the digits, rather than a string first, in the invariant culture, as StringBuilder's
        // Append of an int is in the current one, whose minus sign may not be '-'
        | WireValue.Int i ->
            let mutable written = 0
            i.TryFormat(Span digits, &written, ReadOnlySpan.Empty, CultureInfo.InvariantCulture) |> ignore
            sb.Append(digits, 0, written) |> ignore
#endif
        | WireValue.Float f -> sb.Append(f.ToString("R", CultureInfo.InvariantCulture)) |> ignore
        | WireValue.String s -> writeString sb s
        | WireValue.Bytes b -> writeString sb (Convert.ToBase64String b)
        | WireValue.Array items ->
            sb.Append '[' |> ignore
            let mutable first = true

            for item in items do
                if not first then
                    sb.Append ',' |> ignore

                first <- false
                writeTo sb digits item

            sb.Append ']' |> ignore
        | WireValue.Map fields ->
            sb.Append '{' |> ignore
            let mutable first = true

            for name, value in fields do
                if not first then
                    sb.Append ',' |> ignore

                first <- false
                writeString sb name
                sb.Append ':' |> ignore
                writeTo sb digits value

            sb.Append '}' |> ignore

    /// The text of <paramref name="json"/>, without whitespace.
    let write json =
        let sb = StringBuilder()
        writeTo sb (Array.zeroCreate 11) json
        sb.ToString()

    let private hexValue (c: char) =
        if c >= '0' && c <= '9' then int c - int '0'
        elif c >= 'a' && c <= 'f' then int c - int 'a' + 10
        elif c >= 'A' && c <= 'F' then int c - int 'A' + 10
        else failwith $"Invalid hex digit '{c}' in JSON"

    let private isDigit (c: char) = c >= '0' && c <= '9'

    /// Reads one JSON text, from its start, keeping its place as a field rather than in closures.
    type private Parser(text: string) =
        let mutable i = 0

        let fail expected =
            failwith $"Invalid JSON at {i}: expected {expected}"

        let skipWhitespace () =
            while i < text.Length && Char.IsWhiteSpace text[i] do
                i <- i + 1

        let expect (c: char) =
            skipWhitespace ()

            if i < text.Length && text[i] = c then
                i <- i + 1
            else
                fail $"'{c}'"

        /// The rest of a string with escapes, from the first.
        let parseEscaped (sb: StringBuilder) =
            while i < text.Length && text[i] <> '"' do
                if text[i] = '\\' then
                    i <- i + 1

                    match text[i] with
                    | '"' -> sb.Append '"' |> ignore
                    | '\\' -> sb.Append '\\' |> ignore
                    | '/' -> sb.Append '/' |> ignore
                    | 'n' -> sb.Append '\n' |> ignore
                    | 'r' -> sb.Append '\r' |> ignore
                    | 't' -> sb.Append '\t' |> ignore
                    | 'b' -> sb.Append '\b' |> ignore
                    | 'f' -> sb.Append '\f' |> ignore
                    | 'u' ->
                        let code = Seq.fold (fun acc c -> acc * 16 + hexValue c) 0 (text.Substring(i + 1, 4))
                        sb.Append(char code) |> ignore
                        i <- i + 4
                    | c -> fail $"an escape, not '{c}'"
                else
                    sb.Append text[i] |> ignore

                i <- i + 1

        let parseString () =
            expect '"'
            let start = i

            while i < text.Length && text[i] <> '"' && text[i] <> '\\' do
                i <- i + 1

            // most strings have no escapes, and are read whole
            if i < text.Length && text[i] = '"' then
                i <- i + 1
                text.Substring(start, i - 1 - start)
            else
                let sb = StringBuilder()
                sb.Append(text.Substring(start, i - start)) |> ignore
                parseEscaped sb
                expect '"'
                sb.ToString()

        let parseLiteral (literal: string) value =
            if i + literal.Length <= text.Length && text.Substring(i, literal.Length) = literal then
                i <- i + literal.Length
                value
            else
                fail literal

        /// The number from start to i: an Int if it has no fraction or exponent and fits an int, else a Float.
        let number start =
            let negative = text[start] = '-'
            let digits = if negative then start + 1 else start
            let mutable plain = i - digits >= 1 && i - digits <= 10
            let mutable k = digits

            while plain && k < i do
                plain <- isDigit text[k]
                k <- k + 1

            let mutable n = 0.0

            if plain then
                // ten digits at most, which a double holds exactly
                for k in digits .. i - 1 do
                    n <- n * 10.0 + float (int text[k] - int '0')

            let n = if negative then -n else n

            if plain && n >= -2147483648.0 && n <= 2147483647.0 then
                WireValue.Int(int n)
            else
                let number = text.Substring(start, i - start)
                let value = Double.Parse(number, CultureInfo.InvariantCulture)

                // a double holds every int exactly
                if number.IndexOfAny [| '.'; 'e'; 'E' |] < 0 && value >= -2147483648.0 && value <= 2147483647.0 then
                    WireValue.Int(int value)
                else
                    WireValue.Float value

        let rec parseValue () =
            skipWhitespace ()

            if i >= text.Length then
                fail "a value"

            match text[i] with
            | '{' ->
                i <- i + 1
                skipWhitespace ()

                if text[i] = '}' then
                    i <- i + 1
                    WireValue.Map []
                else
                    let fields = ResizeArray()
                    let mutable more = true

                    while more do
                        let name = parseString ()
                        expect ':'
                        fields.Add((name, parseValue ()))
                        skipWhitespace ()

                        if text[i] = ',' then i <- i + 1 else more <- false

                    expect '}'
                    WireValue.Map(List.ofSeq fields)
            | '[' ->
                i <- i + 1
                skipWhitespace ()

                if text[i] = ']' then
                    i <- i + 1
                    WireValue.Array []
                else
                    let items = ResizeArray()
                    let mutable more = true

                    while more do
                        items.Add(parseValue ())
                        skipWhitespace ()

                        if text[i] = ',' then i <- i + 1 else more <- false

                    expect ']'
                    WireValue.Array(List.ofSeq items)
            | '"' -> WireValue.String(parseString ())
            | 't' -> parseLiteral "true" (WireValue.Bool true)
            | 'f' -> parseLiteral "false" (WireValue.Bool false)
            | 'n' -> parseLiteral "null" WireValue.Nil
            | _ ->
                let start = i

                while i < text.Length
                      && (Char.IsDigit text[i]
                          || text[i] = '-'
                          || text[i] = '+'
                          || text[i] = '.'
                          || text[i] = 'e'
                          || text[i] = 'E') do
                    i <- i + 1

                if i = start then
                    fail "a value"

                number start

        member _.Parse() =
            let value = parseValue ()
            skipWhitespace ()

            if i <> text.Length then
                fail "the end of the text"

            value

    /// Parses JSON text.
    let parse (text: string) : WireValue = Parser(text).Parse()


/// <summary>
/// Orsak.Remoting's JSON, written and parsed in plain F#, so it is the same text on .NET and Fable. Its content type,
/// <c>application/vnd.orsak+json</c>, names the way Orsak.Remoting writes F# types in it, which plain
/// <c>application/json</c> doesn't say.
/// </summary>
type JsonEncoding() =
    /// The encoding, which keeps no state.
    static member val Instance = JsonEncoding()

    interface IWireEncoding with
        member _.ContentType = "application/vnd.orsak+json"
        member _.Write value = Json.write value |> Encoding.UTF8.GetBytes
        member _.Read body = Encoding.UTF8.GetString body |> Json.parse
