namespace Orsak.AspNetCore.OpenApi

open System
open System.Collections.Generic
open System.Net.Http
open System.Reflection
open System.Runtime.CompilerServices
open System.Threading
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Http.Metadata
open Microsoft.AspNetCore.OpenApi
open Microsoft.AspNetCore.Routing
open Microsoft.AspNetCore.Routing.Patterns
open Microsoft.AspNetCore.WebUtilities
open Microsoft.Extensions.DependencyInjection
open Microsoft.OpenApi
open Orsak
open Orsak.AspNetCore

/// ASP.NET Core's endpoint, which Orsak.AspNetCore's shadows.
type private AspNetEndpoint = Microsoft.AspNetCore.Http.Endpoint

/// A response of an operation, before its schema is created.
type private Response = {
    StatusCode: int
    BodyType: Type option
    ContentTypes: string list
    /// What the response means, from the endpoint's own metadata.
    Description: string option
    /// What the response means, as the runner describes it, which the endpoint's docs come before.
    RunnerDescription: string option
}

/// Builds the operation of an effect endpoint from what can be read from it: its route, its metadata, the method that
/// handles it and that method's XML docs. Explicit metadata, such as <c>WithName</c> or <c>Produces</c>, comes first,
/// then the docs, then what is inferred.
module private EffectOperation =
    /// `/orders/{orderId}`, without the route constraints.
    let path (pattern: RoutePattern) =
        let part (p: RoutePatternPart) =
            match p with
            | :? RoutePatternLiteralPart as l -> l.Content
            | :? RoutePatternParameterPart as p -> "{" + p.Name + "}"
            | :? RoutePatternSeparatorPart as s -> s.Content
            | _ -> ""

        let segments =
            pattern.PathSegments |> Seq.map (fun s -> s.Parts |> Seq.map part |> String.concat "")

        "/" + String.concat "/" segments

    /// The schema of a route value, from its route constraint, which the route format sets.
    let routeValueSchema (constraints: string list) =
        let schema typ (format: string) = OpenApiSchema(Type = Nullable typ, Format = format)

        match constraints with
        | [ "int" ] -> schema JsonSchemaType.Integer "int32"
        | [ "long" ] -> schema JsonSchemaType.Integer "int64"
        | [ "double" ] -> schema JsonSchemaType.Number "double"
        | [ "bool" ] -> schema JsonSchemaType.Boolean null
        | [ "guid" ] -> schema JsonSchemaType.String "uuid"
        | [ "length(1)" ] -> OpenApiSchema(Type = Nullable JsonSchemaType.String, MinLength = Nullable 1, MaxLength = Nullable 1)
        | _ -> schema JsonSchemaType.String null

    /// `'a`, of `Effect<'r, 'a, 'e>`.
    let resultType (effectType: Type) =
        if effectType.IsGenericType && effectType.GetGenericTypeDefinition() = typedefof<Effect<obj, obj, obj>> then
            Some(effectType.GetGenericArguments()[1])
        else
            None

    let metadata<'T when 'T: not struct> (endpoint: AspNetEndpoint) =
        match box (endpoint.Metadata.GetMetadata<'T>()) with
        | null -> None
        | m -> Some(unbox<'T> m)

    /// The responses: those in the metadata, a later one replacing an earlier one with its status code, and those the
    /// docs describe with `<response code="404">`. Without a success response, a 200 with the effect's result. A
    /// response is described by the endpoint's metadata, then its docs, then its runner.
    let responses (endpoint: AspNetEndpoint) (docs: Xml.Linq.XElement option) =
        let fromRunner =
            match metadata<RunnerResponses> endpoint with
            | Some(RunnerResponses responses) -> fun (r: obj) -> responses |> List.exists (fun d -> obj.ReferenceEquals(d, r))
            | None -> fun _ -> false

        let declared =
            endpoint.Metadata.GetOrderedMetadata<IProducesResponseTypeMetadata>()
            |> Seq.fold
                (fun (responses: Map<int, Response>) (r: IProducesResponseTypeMetadata) ->
                    let description =
                        match r with
                        | :? ResponseDescription as d -> d.Description
                        | _ -> Option.ofObj r.Description

                    let ofRunner = fromRunner r

                    responses.Add(
                        r.StatusCode,
                        {
                            StatusCode = r.StatusCode
                            BodyType = Option.ofObj r.Type
                            ContentTypes = List.ofSeq r.ContentTypes
                            Description = if ofRunner then None else description
                            RunnerDescription = if ofRunner then description else None
                        }
                    ))
                Map.empty

        let documented =
            docs
            |> Option.map (XmlDocs.elementsBy "response" "code")
            |> Option.defaultValue []
            |> List.choose (fun (code, text) ->
                match Int32.TryParse code with
                | true, code -> Some(code, text)
                | _ -> None)
            |> Map.ofList

        let returns = docs |> Option.bind (XmlDocs.element "returns")
        let isSuccess code = code >= 200 && code < 300

        let inferred =
            if declared |> Map.exists (fun code _ -> isSuccess code) then
                []
            else
                let body =
                    metadata<EffectType> endpoint
                    |> Option.bind (fun (EffectType t) -> resultType t)
                    // a unit or an IResult tells nothing of the body
                    |> Option.filter (fun t -> t <> typeof<unit> && not (typeof<IResult>.IsAssignableFrom t))

                [
                    {
                        StatusCode = 200
                        BodyType = body
                        ContentTypes = []
                        Description = None
                        RunnerDescription = None
                    }
                ]

        let undeclared =
            documented
            |> Map.toList
            |> List.filter (fun (code, _) -> not (declared.ContainsKey code))
            |> List.map (fun (code, _) -> {
                StatusCode = code
                BodyType = None
                ContentTypes = []
                Description = None
                RunnerDescription = None
            })

        [ yield! Map.values declared; yield! inferred; yield! undeclared ]
        |> List.distinctBy _.StatusCode
        |> List.sortBy _.StatusCode
        |> List.map (fun r ->
            let description =
                r.Description
                |> Option.orElse (documented.TryFind r.StatusCode)
                |> Option.orElse (if isSuccess r.StatusCode then returns else None)
                |> Option.orElse r.RunnerDescription
                |> Option.defaultWith (fun () -> ReasonPhrases.GetReasonPhrase r.StatusCode)

            { r with Description = Some description })

    /// The reference id the generator gives a schema, from `OpenApiOptions.CreateSchemaReferenceId`: the type's name for
    /// a complex type, and none for a primitive, which stays inline.
    let private referenceId (schema: OpenApiSchema) =
        match schema.Metadata with
        | null -> None
        | metadata ->
            match metadata.TryGetValue "x-schema-id" with
            | true, (:? string as id) when id <> "" -> Some id
            | _ -> None

    /// Registers `schema`, and the schemas in it, as components of the document, by their reference ids, as the
    /// generator does its own, and returns a reference to it. A schema without a reference id is returned as it is,
    /// with the schemas in it registered. A component that is already registered, such as one the generator registered,
    /// is kept.
    let rec referenced (document: OpenApiDocument) (schema: IOpenApiSchema) : IOpenApiSchema =
        match schema with
        | :? OpenApiSchema as s ->
            let walk (child: IOpenApiSchema) =
                if isNull child then child else referenced document child

            let walkAll (children: IList<IOpenApiSchema>) =
                if not (isNull children) then
                    for i in 0 .. children.Count - 1 do
                        children[i] <- walk children[i]

            if not (isNull s.Properties) then
                for key in List.ofSeq s.Properties.Keys do
                    s.Properties[key] <- walk s.Properties[key]

            s.Items <- walk s.Items
            s.AdditionalProperties <- walk s.AdditionalProperties
            walkAll s.AllOf
            walkAll s.AnyOf
            walkAll s.OneOf

            match referenceId s with
            | Some id ->
                if isNull document.Components then
                    document.Components <- OpenApiComponents()

                if isNull document.Components.Schemas then
                    document.Components.Schemas <- Dictionary<string, IOpenApiSchema>()

                if not (document.Components.Schemas.ContainsKey id) then
                    document.Components.Schemas[id] <- s

                OpenApiSchemaReference(id, document) :> IOpenApiSchema
            | None -> s
        | _ -> schema

    let create
        (context: OpenApiDocumentTransformerContext)
        (document: OpenApiDocument)
        (operationIds: HashSet<string>)
        (endpoint: RouteEndpoint)
        (cancellationToken: CancellationToken)
        =
        task {
            let handler = metadata<HandlingMethod> endpoint |> Option.map (fun (HandlingMethod mi) -> mi)
            let docs = handler |> Option.bind XmlDocs.ofMethod
            let doc name = docs |> Option.bind (XmlDocs.element name)
            let parameterDocs = docs |> Option.map (XmlDocs.elementsBy "param" "name" >> Map.ofList)

            let operation = OpenApiOperation()

            // the handler's name is a default, so not taken twice, as by a function handling two routes
            operation.OperationId <-
                match metadata<IEndpointNameMetadata> endpoint, handler with
                | Some name, _ -> name.EndpointName
                | None, Some mi when not (operationIds.Contains mi.Name) -> mi.Name
                | _ -> null

            if not (isNull operation.OperationId) then
                operationIds.Add operation.OperationId |> ignore

            operation.Summary <-
                metadata<IEndpointSummaryMetadata> endpoint
                |> Option.map _.Summary
                |> Option.orElse (doc "summary")
                |> Option.toObj

            operation.Description <-
                metadata<IEndpointDescriptionMetadata> endpoint
                |> Option.map _.Description
                |> Option.orElse (doc "remarks")
                |> Option.toObj

            let tags =
                match metadata<ITagsMetadata> endpoint with
                | Some tags -> List.ofSeq tags.Tags
                | None -> handler |> Option.map (fun mi -> mi.DeclaringType.Name) |> Option.toList

            if not tags.IsEmpty then
                if isNull document.Tags then
                    document.Tags <- HashSet<OpenApiTag>()

                operation.Tags <- HashSet<OpenApiTagReference>()

                for tag in tags do
                    if not (document.Tags |> Seq.exists (fun t -> t.Name = tag)) then
                        document.Tags.Add(OpenApiTag(Name = tag)) |> ignore

                    operation.Tags.Add(OpenApiTagReference(tag, document, null)) |> ignore

            operation.Parameters <- List<IOpenApiParameter>()

            for p in endpoint.RoutePattern.Parameters do
                let constraints =
                    match endpoint.RoutePattern.ParameterPolicies.TryGetValue p.Name with
                    | true, policies -> [ for policy in policies -> policy.Content ]
                    | _ -> []

                operation.Parameters.Add(
                    OpenApiParameter(
                        Name = p.Name,
                        In = Nullable ParameterLocation.Path,
                        Required = true,
                        Schema = routeValueSchema constraints,
                        Description = (parameterDocs |> Option.bind (Map.tryFind p.Name) |> Option.toObj)
                    )
                )

            operation.Responses <- OpenApiResponses()

            for r in responses endpoint docs do
                let response = OpenApiResponse(Description = Option.toObj r.Description)

                match r.BodyType with
                | Some body ->
                    let! schema = context.GetOrCreateSchemaAsync(body, null, cancellationToken)
                    let schema = referenced document schema
                    response.Content <- Dictionary<string, OpenApiMediaType>()

                    let contentTypes =
                        if r.ContentTypes.IsEmpty then [ "application/json" ] else r.ContentTypes

                    for contentType in contentTypes do
                        response.Content[contentType] <- OpenApiMediaType(Schema = schema)
                | None -> ()

                operation.Responses[string r.StatusCode] <- response

            return operation
        }

/// <summary>
/// Adds the endpoints mapped with <c>MapEffectEndpoints</c> to an OpenAPI document. Their operations are described
/// from what can be read from them, with explicit metadata first:
/// <list type="bullet">
/// <item>the operation id from <c>WithName</c>, or the name of the handler's function;</item>
/// <item>the tag from <c>WithTags</c>, or the module of the handler's function;</item>
/// <item>the summary and description from <c>WithSummary</c> and <c>WithDescription</c>, or the
/// <c>&lt;summary&gt;</c> and <c>&lt;remarks&gt;</c> of the handler's XML docs;</item>
/// <item>the path parameters, typed by their route constraints and described by the docs' <c>&lt;param&gt;</c>;</item>
/// <item>the responses in the metadata, such as those the runner describes with
/// <see cref="T:Orsak.AspNetCore.IDescribeResponses"/>, or <c>Produces</c>, described by the docs'
/// <c>&lt;response code="..."&gt;</c> and <c>&lt;returns&gt;</c>. Without a success response, the operation has a
/// 200 with the effect's result as its body. Their schemas are components of the document, referred to by the
/// ids of <c>OpenApiOptions.CreateSchemaReferenceId</c>, as the generator's own are.</item>
/// </list>
/// </summary>
/// <remarks>
/// The XML docs are read from the file next to the handler's assembly, which the project writes when it sets
/// <c>GenerateDocumentationFile</c>. An endpoint is in the documents of its group name, from <c>WithGroupName</c>, or
/// in every document without one, and not at all with <c>ExcludeFromDescription</c>.
/// </remarks>
[<Sealed>]
type EffectEndpointsTransformer() =
    interface IOpenApiDocumentTransformer with
        member _.TransformAsync(document, context, cancellationToken) =
            task {
                let endpoints =
                    context.ApplicationServices.GetRequiredService<EndpointDataSource>().Endpoints
                    |> Seq.choose (function
                        | :? RouteEndpoint as e when not (isNull (box (e.Metadata.GetMetadata<EffectType>()))) -> Some e
                        | _ -> None)
                    |> Seq.filter (fun e ->
                        match e.Metadata.GetMetadata<IExcludeFromDescriptionMetadata>() with
                        | null -> true
                        | exclude -> not exclude.ExcludeFromDescription)
                    |> Seq.filter (fun e ->
                        match e.Metadata.GetMetadata<IEndpointGroupNameMetadata>() with
                        | null -> true
                        | group -> group.EndpointGroupName = context.DocumentName)
                    |> List.ofSeq

                if isNull document.Paths then
                    document.Paths <- OpenApiPaths()

                let operationIds = HashSet<string>()

                for path in document.Paths.Values do
                    if not (isNull path.Operations) then
                        for operation in path.Operations.Values do
                            if not (isNull operation.OperationId) then
                                operationIds.Add operation.OperationId |> ignore

                for endpoint in endpoints do
                    let path = EffectOperation.path endpoint.RoutePattern

                    let pathItem =
                        match document.Paths.TryGetValue path with
                        | true, (:? OpenApiPathItem as item) -> item
                        | _ ->
                            let item = OpenApiPathItem()
                            document.Paths[path] <- item
                            item

                    if isNull pathItem.Operations then
                        pathItem.Operations <- Dictionary<HttpMethod, OpenApiOperation>()

                    let verbs =
                        match endpoint.Metadata.GetMetadata<IHttpMethodMetadata>() with
                        | null -> []
                        | m -> List.ofSeq m.HttpMethods

                    for verb in verbs do
                        let verb = HttpMethod verb

                        // an operation the generator found itself is left as it is
                        if not (pathItem.Operations.ContainsKey verb) then
                            let! operation = EffectOperation.create context document operationIds endpoint cancellationToken
                            pathItem.Operations[verb] <- operation
            }
            :> Task

/// <summary>Adds Orsak's effect endpoints to OpenAPI documents.</summary>
[<Extension>]
type OpenApiOptionsExtensions =
    /// <summary>
    /// Adds the endpoints mapped with <c>MapEffectEndpoints</c> to the document, with
    /// <see cref="T:Orsak.AspNetCore.OpenApi.EffectEndpointsTransformer"/>.
    /// </summary>
    /// <param name="options">The options of the document</param>
    /// <example>
    /// <code lang="fsharp">
    /// builder.Services.AddOpenApi(fun options -> options.AddEffectEndpoints() |> ignore)
    /// </code>
    /// </example>
    [<Extension>]
    static member AddEffectEndpoints(options: OpenApiOptions) =
        options.AddDocumentTransformer(EffectEndpointsTransformer())
