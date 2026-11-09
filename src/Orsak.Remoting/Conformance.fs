namespace Orsak.Remoting

// Values of any type, converted by reflection: obj is the type meant, wherever it is inferred
#nowarn "3559"

open System

/// <summary>
/// Checks an <see cref="T:Orsak.Remoting.IWireEncoding"/>: that the values of every kind of type
/// <see cref="T:Orsak.Remoting.WireMapping"/> supports cross it and come back the same, and that two encodings of a
/// content type read what the other writes. Each check returns its failures, empty if it passes, for a test of any
/// framework to assert on.
/// </summary>
module Conformance =

    type internal Line = { Product: string; Quantity: int }

    type internal Order = {
        Customer: string
        Lines: Line list
        Note: string option
        Codes: int array
    }

    type internal Shape =
        | Empty
        | Circle of radius: float
        | Rect of width: int * height: int

    type internal Tree =
        | Leaf
        | Node of int * Tree list

    type internal Sample = { Name: string; Type: Type; Value: obj }

    /// Inline, so the type is known on Fable, which erases generic arguments at run time.
    let inline private sample name (value: 'T) = { Name = name; Type = typeof<'T>; Value = box value }

    let private order = {
        Customer = "ada"
        Lines = [ { Product = "apple"; Quantity = 2 } ]
        Note = Some "ring twice"
        Codes = [| 1; 2 |]
    }

    let private samples = [
        sample "a string" "plain"
        sample "a string with escapes and unicode" "quote \" backslash \\ line\n tab\t åäö \u0001 \U0001F600"
        sample "an empty string" ""
        sample "bools" [ true; false ]
        sample "ints, to their bounds" [ 0; 1; -1; Int32.MaxValue; Int32.MinValue ]
        sample "floats, with and without fractions" [ 0.0; 1.0; -2.0; 0.1; 1.5e300; 5e-324; 3e9 ]
        sample "int64s, to their bounds" [ 0L; Int64.MaxValue; Int64.MinValue ]
        sample "decimals" [ 1.25m; -0.0001m ]
        sample "a Guid" (Guid "6f9619ff-8b86-d011-b42d-00cf4fc964ff")
        sample "bytes" [| 0uy; 1uy; 127uy; 128uy; 255uy |]
        sample "empty bytes" ([||]: byte array)
        sample "unit" ()
        sample "a record with a list, an option and an array" order
        sample "a record with None, and empty lists and arrays" { order with Lines = []; Note = None; Codes = [||] }
        sample "options of a record" [ Some { Product = "a"; Quantity = 1 }; None ]
        sample "a tuple" (1, "a", true)
        sample "union cases without, with one and with two fields" [ Empty; Circle 1.5; Rect(2, 3) ]
        sample "Results" [ Ok 1; Error "no" ]
        sample "a type that contains itself" (Node(1, [ Leaf; Node(2, []) ]))
        sample "nested options" [ Some(Some 1); Some None; None ]
        sample "a map keyed by strings" (Map [ "a", 1; "b", 2 ])
        sample "a map keyed by ints" (Map [ 1, "a"; 2, "b" ])
        sample "an empty map" (Map.empty: Map<string, int>)
        sample "a set" (Set [ "a"; "b" ])
        sample "DateTimes, UTC and of no kind" [
            DateTime(2026, 11, 9, 8, 30, 15, 123, DateTimeKind.Utc)
            DateTime(2026, 11, 9, 8, 30, 0, DateTimeKind.Unspecified)
        ]
        sample "a DateTimeOffset" (DateTimeOffset(2026, 11, 9, 8, 30, 0, TimeSpan.FromMinutes -330.0))
        sample "a DateOnly and a TimeOnly" (DateOnly(2026, 11, 9), TimeOnly(8, 30, 15, 250))
        sample "TimeSpans" [ TimeSpan(1, 2, 3, 4, 500); TimeSpan.FromMinutes -90.0; TimeSpan.Zero ]
    ]

    let private attempt name f =
        try
            f ()
        with e ->
            [ $"{name}: {e.Message}" ]

    let private same name (expected: obj) (actual: obj) =
        if expected = actual then [] else [ $"{name}: %A{expected} came back as %A{actual}" ]

    /// <summary>
    /// Checks that the values of every kind of type the mapping supports, written by <paramref name="encoding"/>, are
    /// read back by it the same, and a member's arguments too.
    /// </summary>
    /// <param name="encoding">The encoding to check</param>
    /// <returns>The failures, empty if the encoding passes</returns>
    let check (encoding: IWireEncoding) : string list =
        let format = WireFormat(encoding) :> IWireFormat

        [
            for s in samples do
                yield!
                    attempt s.Name (fun () ->
                        let codec = format.CodecFor s.Type
                        same s.Name s.Value (codec.Decode(codec.Encode s.Value)))

            yield!
                attempt "a member's arguments" (fun () ->
                    let arguments = [| box order; box 3; box "x" |]
                    let codec = format.ArgumentsCodecFor [| typeof<Order>; typeof<int>; typeof<string> |]
                    same "a member's arguments" (box arguments) (box (codec.Decode(codec.Encode arguments))))
        ]

    /// <summary>
    /// Checks that two encodings of a content type agree: that each reads the values of every kind of type the mapping
    /// supports, written by the other, the same. They needn't write the same bytes.
    /// </summary>
    /// <param name="reference">An encoding known to be right, such as <c>JsonEncoding.Instance</c></param>
    /// <param name="encoding">The encoding to check against it</param>
    /// <returns>The failures, empty if the encodings agree</returns>
    let agree (reference: IWireEncoding) (encoding: IWireEncoding) : string list =
        let reference' = WireFormat(reference) :> IWireFormat
        let encoding' = WireFormat(encoding) :> IWireFormat

        [
            if reference.ContentType <> encoding.ContentType then
                $"The content types differ: {reference.ContentType} and {encoding.ContentType}"

            for s in samples do
                yield!
                    attempt s.Name (fun () ->
                        let fromReference = reference'.CodecFor s.Type
                        let fromEncoding = encoding'.CodecFor s.Type

                        same $"{s.Name}, written by the reference" s.Value (fromEncoding.Decode(fromReference.Encode s.Value))
                        @ same $"{s.Name}, read by the reference" s.Value (fromReference.Decode(fromEncoding.Encode s.Value)))
        ]
