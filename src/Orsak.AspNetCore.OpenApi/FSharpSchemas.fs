namespace Orsak.AspNetCore.OpenApi

open System
open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Threading.Tasks
open Microsoft.AspNetCore.OpenApi
open Microsoft.OpenApi

module private FSharpTypes =
    let private isGeneric (definition: Type) (t: Type) =
        t.IsGenericType && t.GetGenericTypeDefinition() = definition

    /// `'T` of `'T option` or `'T voption`, which System.Text.Json writes as the value, or `null` for none.
    let optionValue (t: Type) =
        if isGeneric typedefof<option<obj>> t || isGeneric typedefof<voption<obj>> t then
            Some(t.GetGenericArguments()[0])
        else
            None

    /// An F# list, set or map, which is never `null`.
    let isCollection (t: Type) =
        isGeneric typedefof<list<obj>> t || isGeneric typedefof<Set<int>> t || isGeneric typedefof<Map<int, obj>> t

    let isSet (t: Type) = isGeneric typedefof<Set<int>> t

    let schemaId = "x-schema-id"

    /// Whether the generator gives the schema a component of its own: complex types have a reference id.
    let hasReferenceId (schema: OpenApiSchema) =
        match schema.Metadata with
        | null -> false
        | metadata ->
            match metadata.TryGetValue schemaId with
            | true, (:? string as id) -> id <> ""
            | _ -> false

    /// Makes `schema` that of the option of a value with schema `value`: the value's own, or `null`.
    let describeOption (schema: OpenApiSchema) (value: OpenApiSchema) =
        // the option is the value, so it has no component of its own
        if not (isNull schema.Metadata) then
            schema.Metadata.Remove schemaId |> ignore

        if hasReferenceId value then
            // a component can't be made nullable, so it is one of the component, or null
            schema.Type <- Nullable()
            schema.Properties <- null
            schema.Required <- null
            schema.OneOf <- List<IOpenApiSchema>([ OpenApiSchema(Type = Nullable JsonSchemaType.Null) :> IOpenApiSchema; value ])
        else
            schema.Type <-
                if value.Type.HasValue then
                    Nullable(value.Type.Value ||| JsonSchemaType.Null)
                else
                    Nullable()

            schema.Format <- value.Format
            schema.Pattern <- value.Pattern
            schema.Items <- value.Items
            schema.Enum <- value.Enum
            schema.Minimum <- value.Minimum
            schema.Maximum <- value.Maximum
            schema.MinLength <- value.MinLength
            schema.MaxLength <- value.MaxLength
            schema.Properties <- value.Properties
            schema.AdditionalProperties <- value.AdditionalProperties

/// <summary>
/// Describes common F# types as System.Text.Json writes them, which the generator describes as it would any .NET type:
/// <list type="bullet">
/// <item>an option or value option is its value, or <c>null</c>, rather than a component of its own, and a record's
/// option fields aren't required, since a missing one is <c>None</c>;</item>
/// <item>an F# list, set or map is never <c>null</c>, and a set's items are unique.</item>
/// </list>
/// </summary>
/// <remarks>
/// Unions aren't described: System.Text.Json can't write them by itself, and how a converter such as
/// FSharp.SystemTextJson writes them depends on its options.
/// </remarks>
[<Sealed>]
type FSharpSchemaTransformer() =
    interface IOpenApiSchemaTransformer with
        member _.TransformAsync(schema, context, cancellationToken) =
            task {
                let t = context.JsonTypeInfo.Type

                match FSharpTypes.optionValue t with
                | Some valueType ->
                    let! value = context.GetOrCreateSchemaAsync(valueType, null, cancellationToken)
                    FSharpTypes.describeOption schema value
                | None -> ()

                if FSharpTypes.isCollection t && schema.Type.HasValue then
                    schema.Type <- Nullable(schema.Type.Value &&& ~~~JsonSchemaType.Null)

                if FSharpTypes.isSet t then
                    schema.UniqueItems <- Nullable true

                // a record's missing option field is None
                if not (isNull schema.Required) then
                    for property in context.JsonTypeInfo.Properties do
                        if (FSharpTypes.optionValue property.PropertyType).IsSome then
                            schema.Required.Remove property.Name |> ignore
            }
            :> Task

/// <summary>Adds the description of F# types to OpenAPI documents.</summary>
[<Extension>]
type FSharpSchemaOptionsExtensions =
    /// <summary>
    /// Describes common F# types as System.Text.Json writes them, with
    /// <see cref="T:Orsak.AspNetCore.OpenApi.FSharpSchemaTransformer"/>.
    /// </summary>
    /// <param name="options">The options of the document</param>
    /// <example>
    /// <code lang="fsharp">
    /// builder.Services.AddOpenApi(fun options -> options.AddEffectEndpoints().AddFSharpSchemas() |> ignore)
    /// </code>
    /// </example>
    [<Extension>]
    static member AddFSharpSchemas(options: OpenApiOptions) =
        options.AddSchemaTransformer(FSharpSchemaTransformer())
