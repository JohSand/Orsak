/// Reads the XML docs the compiler writes next to an assembly, when the project sets GenerateDocumentationFile.
module internal Orsak.AspNetCore.OpenApi.XmlDocs

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Text
open System.Xml.Linq

let private cache = ConcurrentDictionary<Assembly, IReadOnlyDictionary<string, XElement>>()

let private load (assembly: Assembly) : IReadOnlyDictionary<string, XElement> =
    let path =
        if String.IsNullOrEmpty assembly.Location then
            None
        else
            Some(Path.ChangeExtension(assembly.Location, ".xml"))

    match path with
    | Some path when File.Exists path ->
        XDocument.Load(path).Descendants(XName.Get "member")
        |> Seq.choose (fun m -> m.Attribute(XName.Get "name") |> Option.ofObj |> Option.map (fun name -> name.Value, m))
        |> Seq.distinctBy fst
        |> readOnlyDict
    | _ -> readOnlyDict []

/// The docs of an assembly, by member id, or none if it has no XML docs.
let ofAssembly (assembly: Assembly) = cache.GetOrAdd(assembly, load)

/// The name of a type in a member id: `System.Int32`, `Microsoft.FSharp.Core.FSharpOption{System.Int32}`, or
/// ``` ``0 ``` for a method's type parameter.
let rec private typeId (t: Type) =
    if t.IsGenericMethodParameter then
        $"``{t.GenericParameterPosition}"
    elif t.IsGenericParameter then
        $"`{t.GenericParameterPosition}"
    elif t.IsArray then
        typeId (t.GetElementType()) + "[]"
    elif t.IsGenericType then
        let name = t.GetGenericTypeDefinition().FullName.Replace('+', '.')
        let name = name.Substring(0, name.IndexOf '`')
        let args = t.GetGenericArguments() |> Array.map typeId |> String.concat ","
        $"{name}{{{args}}}"
    else
        t.FullName.Replace('+', '.')

/// The member id of a method, as the compiler writes it: `M:Ns.Module.getOrder(System.Int32)`.
let methodId (mi: MethodInfo) =
    let mi = if mi.IsGenericMethod then mi.GetGenericMethodDefinition() else mi
    let arity = if mi.IsGenericMethod then $"``{mi.GetGenericArguments().Length}" else ""

    let parameters =
        match mi.GetParameters() with
        | [||] -> ""
        | ps -> "(" + (ps |> Array.map (fun p -> typeId p.ParameterType) |> String.concat ",") + ")"

    $"M:{mi.DeclaringType.FullName.Replace('+', '.')}.{mi.Name}{arity}{parameters}"

/// The docs of a method, if its assembly has XML docs that document it.
let ofMethod (mi: MethodInfo) =
    match (ofAssembly mi.DeclaringType.Assembly).TryGetValue(methodId mi) with
    | true, docs -> Some docs
    | _ -> None

/// The text of an element on one line: `<see cref="T:X.Y"/>` is written as `Y`, and the source's line breaks and
/// indentation are dropped.
let text (e: XElement) =
    let rec write (sb: StringBuilder) (node: XNode) =
        match node with
        | :? XText as t -> sb.Append(t.Value) |> ignore
        | :? XElement as e when e.Name.LocalName = "see" || e.Name.LocalName = "paramref" ->
            let target =
                [ "cref"; "name"; "langword" ]
                |> List.tryPick (fun a -> e.Attribute(XName.Get a) |> Option.ofObj)
                |> Option.map (fun a -> a.Value.Split(':') |> Array.last |> _.Split('.') |> Array.last)

            sb.Append(defaultArg target "") |> ignore
        | :? XElement as e -> e.Nodes() |> Seq.iter (write sb)
        | _ -> ()

    let sb = StringBuilder()
    e.Nodes() |> Seq.iter (write sb)

    sb.ToString().Split('\n')
    |> Array.map _.Trim()
    |> Array.filter (fun l -> l <> "")
    |> String.concat " "

/// The text of the first child element named `name`, if it has any.
let element (name: string) (docs: XElement) =
    docs.Element(XName.Get name)
    |> Option.ofObj
    |> Option.map text
    |> Option.filter (fun t -> t <> "")

/// The text of the child elements named `name`, by the value of their attribute `key`, such as each `<param>` by name.
let elementsBy (name: string) (key: string) (docs: XElement) = [
    for e in docs.Elements(XName.Get name) do
        match e.Attribute(XName.Get key) with
        | null -> ()
        | attribute -> attribute.Value, text e
]
