/// An exploration of what OpenAPI metadata can be inferred for effect endpoints, without anything written for OpenAPI:
/// from the route, the handler's method and its signature, and the handler's XML docs.
module Orsak.Tests.OpenApiInferenceTests

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Text
open System.Xml.Linq
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Routing
open Microsoft.FSharp.Reflection
open Orsak
open Orsak.AspNetCore
open Orsak.AspNetCore.V2
open VerifyXunit
open Xunit

/// The XML docs of an assembly, by member id, read from the .xml file next to it.
module XmlDocs =
    let private cache = Dictionary<Assembly, IReadOnlyDictionary<string, XElement>>()

    let ofAssembly (assembly: Assembly) : IReadOnlyDictionary<string, XElement> =
        lock cache (fun () ->
            match cache.TryGetValue assembly with
            | true, docs -> docs
            | _ ->
                let path = Path.ChangeExtension(assembly.Location, ".xml")

                let docs =
                    if File.Exists path then
                        XDocument.Load(path).Descendants(XName.Get "member")
                        |> Seq.map (fun m -> m.Attribute(XName.Get "name").Value, m)
                        |> readOnlyDict
                    else
                        readOnlyDict []

                cache[assembly] <- docs
                docs)

    /// The name of a type in a member id: `System.Int32`, `Microsoft.FSharp.Core.FSharpOption{System.Int32}`,
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

    /// The member id of a method, as the compiler writes it in the XML docs: `M:Ns.Module.getOrder(System.Int32)`.
    let methodId (mi: MethodInfo) =
        let mi = if mi.IsGenericMethod then mi.GetGenericMethodDefinition() else mi
        let arity = if mi.IsGenericMethod then $"``{mi.GetGenericArguments().Length}" else ""

        let parameters =
            match mi.GetParameters() with
            | [||] -> ""
            | ps -> "(" + (ps |> Array.map (fun p -> typeId p.ParameterType) |> String.concat ",") + ")"

        $"M:{mi.DeclaringType.FullName.Replace('+', '.')}.{mi.Name}{arity}{parameters}"

    let typeIdOf (t: Type) = "T:" + typeId t

    /// The text of an element, with `<see cref="T:X.Y"/>` written as `Y`, and the indentation of the source dropped.
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

/// What can be inferred for an endpoint.
type InferredOperation = {
    Verb: string
    Route: string
    OperationId: string option
    Tag: string option
    Summary: string option
    Description: string option
    Parameters: InferredParameter list
    Success: Type option
    SuccessDescription: string option
    Error: Type option
    ErrorCases: InferredErrorCase list
    Responses: (string * string) list
    Environment: Type option
}

and InferredParameter = {
    Name: string
    Type: string
    Format: string option
    Description: string option
}

and InferredErrorCase = {
    Case: string
    Fields: (string * Type) list
    Description: string option
}

module Infer =
    /// The OpenAPI type and format of a route value, from its route constraint, which the route format sets.
    let private schemaOf (constraints: string list) =
        match constraints with
        | [ "int" ] -> "integer", Some "int32"
        | [ "long" ] -> "integer", Some "int64"
        | [ "double" ] -> "number", Some "double"
        | [ "bool" ] -> "boolean", None
        | [ "guid" ] -> "string", Some "uuid"
        | [ "length(1)" ] -> "string", Some "char"
        | _ -> "string", None

    /// `Effect<'r, 'a, 'e>`, from the type of the handler's effect.
    let private effectTypes (t: Type) =

        if t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<Effect<_, _, _>> then
            match t.GetGenericArguments() with
            | [| r; a; e |] -> Some(r, a, e)
            | _ -> None
        else
            None

    /// The cases of a union error, documented in the assembly of the error type.
    let private errorCases (error: Type) = [
        let docs = XmlDocs.ofAssembly error.Assembly

        if FSharpType.IsUnion error then
            for case in FSharpType.GetUnionCases error do
                // a case with fields is a nested type; one without is a property
                let ids = [
                    $"T:{error.FullName.Replace('+', '.')}.{case.Name}"
                    $"P:{error.FullName.Replace('+', '.')}.{case.Name}"
                ]

                let description =
                    ids
                    |> List.tryPick (fun id ->
                        match docs.TryGetValue id with
                        | true, m -> m.Element(XName.Get "summary") |> Option.ofObj |> Option.map XmlDocs.text
                        | _ -> None)

                {
                    Case = case.Name
                    Fields = [ for f in case.GetFields() -> f.Name, f.PropertyType ]
                    Description = description
                }
    ]

    let operation (endpoint: RouteEndpoint) : InferredOperation =
        let handler =
            endpoint.Metadata.GetMetadata<HandlingMethod>()
            |> Option.ofObj
            |> Option.map (fun (HandlingMethod mi) -> mi)

        let docs =
            handler |> Option.map (fun mi -> XmlDocs.ofAssembly mi.DeclaringType.Assembly)

        let doc =
            match handler, docs with
            | Some mi, Some docs ->
                match docs.TryGetValue(XmlDocs.methodId mi) with
                | true, m -> Some m
                | _ -> None
            | _ -> None

        let element name =
            doc |> Option.bind (fun d -> d.Element(XName.Get name) |> Option.ofObj) |> Option.map XmlDocs.text

        let paramDoc name =
            doc
            |> Option.bind (fun d ->
                d.Elements(XName.Get "param")
                |> Seq.tryFind (fun p -> p.Attribute(XName.Get "name").Value = name))
            |> Option.map XmlDocs.text

        let effect =
            endpoint.Metadata.GetMetadata<EffectType>()
            |> Option.ofObj
            |> Option.bind (fun (EffectType t) -> effectTypes t)

        let verb =
            endpoint.Metadata.GetMetadata<IHttpMethodMetadata>().HttpMethods |> Seq.exactlyOne

        {
            Verb = verb
            Route = endpoint.RoutePattern.RawText
            // an explicit name wins over the handler's
            OperationId =
                endpoint.Metadata.GetMetadata<IEndpointNameMetadata>()
                |> Option.ofObj
                |> Option.map _.EndpointName
                |> Option.orElse (handler |> Option.map _.Name)
            Tag = handler |> Option.map _.DeclaringType.Name
            Summary = element "summary"
            Description = element "remarks"
            Parameters = [
                for p in endpoint.RoutePattern.Parameters do
                    let constraints =
                        endpoint.RoutePattern.ParameterPolicies.GetValueOrDefault(p.Name, [])
                        |> Seq.map _.Content
                        |> List.ofSeq

                    let typ, format = schemaOf constraints
                    { Name = p.Name; Type = typ; Format = format; Description = paramDoc p.Name }
            ]
            Success = effect |> Option.map (fun (_, a, _) -> a)
            SuccessDescription = element "returns"
            Error = effect |> Option.map (fun (_, _, e) -> e)
            ErrorCases =
                match effect with
                | Some(_, _, e) -> errorCases e
                | None -> []
            // the convention Swashbuckle reads: <response code="404">Not found</response>
            Responses =
                doc
                |> Option.map (fun d -> [
                    for r in d.Elements(XName.Get "response") -> r.Attribute(XName.Get "code").Value, XmlDocs.text r
                ])
                |> Option.defaultValue []
            Environment = effect |> Option.map (fun (r, _, _) -> r)
        }

/// Handlers of an order API, documented as an application would, and nothing else.
module Orders =
    type OrderLine = { Sku: string; Quantity: int }

    type Order = {
        Id: int
        Customer: Guid
        Lines: OrderLine list
        Note: string option
    }

    /// Why a request about orders failed.
    type OrderError =
        /// No order has the id.
        | OrderNotFound of orderId: int
        /// The order has no line with the number.
        | LineNotFound of orderId: int * line: int
        /// The caller may not see the order.
        | Forbidden

    let private order id = {
        Id = id
        Customer = Guid.Empty
        Lines = []
        Note = None
    }

    /// <summary>Gets an order, with its lines.</summary>
    /// <remarks>
    /// The lines are in the order they were added. See <see cref="T:Orsak.Tests.OpenApiInferenceTests.Orders.OrderLine"/>.
    /// </remarks>
    /// <param name="orderId">The id of the order.</param>
    /// <returns>The order.</returns>
    /// <response code="404">No order has the id.</response>
    let getOrder (orderId: int) : Effect<unit, Order, OrderError> = eff { return order orderId }

    /// Gets a line of an order.
    let getOrderLine (orderId: int) (line: int) : Effect<unit, OrderLine, OrderError> = eff {
        return { Sku = $"{orderId}-{line}"; Quantity = 1 }
    }

    /// Lists the orders of a customer.
    /// <param name="customer">The customer's id.</param>
    let listOrders (customer: Guid) : Effect<unit, Order list, OrderError> = eff { return [] }

    let ping () : Effect<unit, string, OrderError> = eff { return "pong" }

/// Stands in for an application's runner.
type Runner =
    | Runner

    static member inline ( *>> )(_: Effect<unit, 'a, Orders.OrderError>, Runner) : RequestDelegate =
        RequestDelegate(fun _ -> Threading.Tasks.Task.CompletedTask)

let endpoints () = [
    Runner.RouteGet("/orders/%i", Orders.getOrder)
    Runner.RouteGet2("/orders/%i/lines/%i", Orders.getOrderLine)
    Runner.RouteGet2("/customers/%O/orders", Orders.listOrders).WithName("ListCustomerOrders")
    Runner.RouteGet2("/ping", Orders.ping)
    // a lambda doing more than calling a function: no handling method, so no name or docs, but its effect type
    Runner.RouteDelete("/orders/%i", fun (orderId: int) -> Orders.getOrder (orderId + 1))
]

let private optional (label: string) (value: string option) =
    value |> Option.map (fun v -> $"  {label}: {v}") |> Option.toList

/// F#-ish type names, for the report.
let rec private typeName (t: Type) =
    if t.IsGenericType then
        let name = t.Name.Substring(0, t.Name.IndexOf '`')
        let args = t.GetGenericArguments() |> Array.map typeName |> String.concat ", "
        $"{name}<{args}>"
    else
        t.Name

let report (operations: InferredOperation list) =
    [
        for o in operations do
            $"{o.Verb} {o.Route}"
            yield! optional "operationId" o.OperationId
            yield! optional "tag" o.Tag
            yield! optional "summary" o.Summary
            yield! optional "description" o.Description

            for p in o.Parameters do
                let format = p.Format |> Option.map (sprintf " (%s)") |> Option.defaultValue ""
                let description = p.Description |> Option.map (sprintf ": %s") |> Option.defaultValue ""
                $"  parameter {p.Name}, in path, {p.Type}{format}{description}"

            yield! optional "success" (o.Success |> Option.map typeName)
            yield! optional "success description" o.SuccessDescription
            yield! optional "error" (o.Error |> Option.map typeName)

            for c in o.ErrorCases do
                let fields =
                    match c.Fields with
                    | [] -> ""
                    | fs -> fs |> List.map (fun (n, t) -> $"{n}: {typeName t}") |> String.concat ", " |> sprintf " of %s"

                let description = c.Description |> Option.map (sprintf ": %s") |> Option.defaultValue ""
                $"    case {c.Case}{fields}{description}"

            for code, description in o.Responses do
                $"  response {code}: {description}"

            yield! optional "environment" (o.Environment |> Option.map typeName)
            ""
    ]
    |> String.concat "\n"

[<Fact>]
let ``what can be inferred for effect endpoints`` () = task {
    let app = WebApplication.CreateSlimBuilder().Build()
    app.MapEffectEndpoints(endpoints ())

    let operations =
        (app :> IEndpointRouteBuilder).DataSources
        |> Seq.collect _.Endpoints
        |> Seq.choose (function :? RouteEndpoint as e -> Some(Infer.operation e) | _ -> None)
        |> List.ofSeq

    let! _ = Verifier.Verify(report operations, extension = "txt")
    ()
}
