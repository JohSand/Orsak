namespace Orsak.Remoting

open System
open System.Text

/// A JSON value. Numbers keep their text, so a format decides how they are read, without a round trip through float.
type Json =
    | JNull
    | JBool of bool
    | JNumber of string
    | JString of string
    | JArray of Json list
    | JObject of (string * Json) list

/// Writes and parses JSON text, in plain F#, so .NET and Fable read and write it the same way.
module Json =

    let private writeString (sb: StringBuilder) (s: string) =
        sb.Append '"' |> ignore

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

        sb.Append '"' |> ignore

    let rec private writeTo (sb: StringBuilder) json =
        match json with
        | JNull -> sb.Append "null" |> ignore
        | JBool b -> sb.Append(if b then "true" else "false") |> ignore
        | JNumber n -> sb.Append n |> ignore
        | JString s -> writeString sb s
        | JArray items ->
            sb.Append '[' |> ignore

            items
            |> List.iteri (fun i item ->
                if i > 0 then
                    sb.Append ',' |> ignore

                writeTo sb item)

            sb.Append ']' |> ignore
        | JObject fields ->
            sb.Append '{' |> ignore

            fields
            |> List.iteri (fun i (name, value) ->
                if i > 0 then
                    sb.Append ',' |> ignore

                writeString sb name
                sb.Append ':' |> ignore
                writeTo sb value)

            sb.Append '}' |> ignore

    /// The text of <paramref name="json"/>, without whitespace.
    let write json =
        let sb = StringBuilder()
        writeTo sb json
        sb.ToString()

    let private hexValue (c: char) =
        if c >= '0' && c <= '9' then int c - int '0'
        elif c >= 'a' && c <= 'f' then int c - int 'a' + 10
        elif c >= 'A' && c <= 'F' then int c - int 'A' + 10
        else failwith $"Invalid hex digit '{c}' in JSON"

    /// Parses JSON text.
    let parse (text: string) : Json =
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

        let parseString () =
            expect '"'
            let sb = StringBuilder()

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

            expect '"'
            sb.ToString()

        let parseLiteral (literal: string) value =
            if i + literal.Length <= text.Length && text.Substring(i, literal.Length) = literal then
                i <- i + literal.Length
                value
            else
                fail literal

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
                    JObject []
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
                    JObject(List.ofSeq fields)
            | '[' ->
                i <- i + 1
                skipWhitespace ()

                if text[i] = ']' then
                    i <- i + 1
                    JArray []
                else
                    let items = ResizeArray()
                    let mutable more = true

                    while more do
                        items.Add(parseValue ())
                        skipWhitespace ()

                        if text[i] = ',' then i <- i + 1 else more <- false

                    expect ']'
                    JArray(List.ofSeq items)
            | '"' -> JString(parseString ())
            | 't' -> parseLiteral "true" (JBool true)
            | 'f' -> parseLiteral "false" (JBool false)
            | 'n' -> parseLiteral "null" JNull
            | _ ->
                let start = i

                while i < text.Length && (Char.IsDigit text[i] || "+-.eE".IndexOf text[i] >= 0) do
                    i <- i + 1

                if i = start then fail "a value" else JNumber(text.Substring(start, i - start))

        let value = parseValue ()
        skipWhitespace ()

        if i <> text.Length then
            fail "the end of the text"

        value
