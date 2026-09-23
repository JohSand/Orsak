module Orsak.Myriad.Ast

open Myriad.Core
open System
open Orsak.Myriad
open Fantomas.FCS.Syntax
open System.Text

let (|Ident|_|) (s: string) (ident: Ident) =
    if ident.idText = s then Some() else None

let toString (ident: LongIdent) =
    String.Join(".", ident |> List.map (fun i -> i.idText))

let getName (SynTypeDefn(c, _, _, _, _, _)) =
    match c with
    | SynComponentInfo({ Attributes = { TypeName = SynLongIdent([ Ident "GenRunner" ], _, _) } as runnerAttr :: _ } :: _, _, _, [ ident ], _, _, _, _) ->
        //as long as GenRunnerAttribute only has these properties, these matches will work.
        //If we add more, we need to match better.
        match runnerAttr.ArgExpr with
        //UserInterfaceName = true
        | SynExpr.Paren(SynExpr.App(_, _, _funcExpr, SynExpr.Const(SynConst.Bool true, _), _), _, _, _) ->
            //ident is the interface name
            ident.idText.TrimStart('I')
        //Name = {name}
        | SynExpr.Paren(SynExpr.App(_, _, _funcExpr, SynExpr.Const(SynConst.String(s, _, _), _), _), _, _, _) ->
            //s is name set equal to Name.
            s
        | _ -> ""
    | _ -> ""

let (|Provider|_|) =
    function
    | SynType.App(SynType.LongIdent(SynLongIdent([ xx: Ident ], _, _)), _, [ SynType.LongIdent a ], _, _, _, _) ->
        if xx.idText = "IProvide" then Some a.LongIdent else None
    | _ -> None

let getInherits (context: GeneratorContext) (SynTypeDefn(_c, typeRepr: SynTypeDefnRepr, _, _, _, _)) = [|
    match typeRepr with
    | SynTypeDefnRepr.ObjectModel(_kind, members: SynMemberDefn list, _range) ->
        for mem in members do
            match mem with
            | SynMemberDefn.Inherit(Provider name, _, _) ->
                let effectName = name |> List.last |> _.idText

                let providerName =
                    if effectName = "ILoggerFactory" then
                        "ILoggerFactory"
                    else
                        context.ConfigGetter effectName
                        |> Seq.tryPick (fun (key, value) ->
                            if key = "ProviderName" then
                                Some(value :?> string)
                            else
                                None)
                        |> Option.defaultValue $"%s{effectName}Provider"

                let effectMember =
                    context.ConfigGetter effectName
                    |> Seq.tryPick (fun (key, value) ->
                        if key = "ProviderPropertyName" then
                            Some(value :?> string)
                        else
                            None)
                    |> Option.defaultValue "Effect"

                yield {
                    name = effectName
                    fullName = toString name
                    providerName = providerName
                    providerPropertyName = effectMember
                }
            | _ ->

                ()
    | _ -> ()
|]

let mkEffectAttributeMatches (context: GeneratorContext) (s: SynTypeDefn) =
    { effects = getInherits context s; nameOverride = getName s }

let rec createContextScope (context: GeneratorContext) (decls: SynModuleDecl list) (acc: ContextWriterScope) =
    match decls with
    | [] -> acc
    | x :: xs ->
        match x with
        | SynModuleDecl.Types(types, _) ->
            let attributedTypes =
                types
                |> List.filter Ast.hasAttribute<GenRunnerAttribute>
                |> List.map (mkEffectAttributeMatches context)

            createContextScope context xs { acc with effects = attributedTypes @ acc.effects }

        | SynModuleDecl.Open(SynOpenDeclTarget.ModuleOrNamespace(SynLongIdent(target, _, _), _), _) ->
            let ns = toString target

            createContextScope context xs { acc with openStatements = ns :: acc.openStatements }

        | SynModuleDecl.NestedModule(SynComponentInfo(_, _, _, _longId, _, _, _, _), _, _decls, _, _, _) -> createContextScope context xs acc
        | _other -> createContextScope context xs acc


let parseRunnerDefn (context: GeneratorContext) (ast: ParsedInput) = [
    match ast with
    | ParsedInput.ImplFile(ParsedImplFileInput(_name, _, _, _, _, modules, _, _, _)) ->
        for SynModuleOrNamespace(namespaceId, _, _, moduleDecls, _, _, _, _, _) in modules do
            createContextScope context moduleDecls { openStatements = []; effects = []; ns = toString namespaceId }
    | _ -> ()
]

let isTuple (t: SynType) =
    match t with
    | SynType.Fun(SynType.Tuple _, _returnType, _range, _) -> true
    | _ -> false

let isUnit (t: SynType) =
    match t with
    | SynType.Fun(SynType.LongIdent(SynLongIdent([ x ], _, _)), _returnType, _range, _) -> x.idText = "unit"
    | _ -> false

let parseTypeName (SynComponentInfo(_, _, _, longId, _, _, _, _)) = longId |> List.last |> _.idText

let parseInParamCount (m: SynMemberDefn) =
    match m with
    | SynMemberDefn.AbstractSlot(slotSig = s) ->
        let (SynValSig(synType = d)) = s

        let rec countArguments synType acc =
            match synType with
            //if returnType is a fun, we are of type 'a -> 'b -> ...
            | SynType.Fun(_argType, (SynType.Fun _ as t), _range, _) -> countArguments t (acc + 1)
            | SynType.Fun(argType, _, _range, _) -> countArguments argType acc
            | SynType.LongIdent(SynLongIdent _) -> 1 + acc
            | SynType.Var(_) -> 1 + acc
            | SynType.SignatureParameter(_) -> 1 + acc
            | SynType.Paren(innerType = inner) -> countArguments inner acc
            | SynType.Tuple(_, types, _) -> types.Length / 2 + 1 + acc
            //| SynType.Var _ -> acc
            | _ -> acc

        countArguments d 0

    | _ -> 0

let parseMemberName (m: SynMemberDefn) =
    match m with
    | SynMemberDefn.AbstractSlot(slotSig = (SynValSig(ident = SynIdent(id, _)))) -> id.idText
    | _ -> ""

let effectMemberCfg (context: GeneratorContext) (SynTypeDefn(typeInfo: SynComponentInfo, objModel, _, _, _, _)) =
    match objModel with
    | SynTypeDefnRepr.ObjectModel(_, members, _) ->
        let members = [
            for m in members do
                match m with
                | SynMemberDefn.AbstractSlot(slotSig = (SynValSig(synType = d))) -> {
                    argumentCount = parseInParamCount m
                    memberName = parseMemberName m
                    isTuple = isTuple d
                    isUnit = isUnit d
                  }
                | _ -> ()
        ]

        let effectName = parseTypeName typeInfo

        let providerName =
            context.ConfigGetter effectName
            |> Seq.tryPick (fun (key, value) ->
                if key = "ProviderName" then
                    Some(value :?> string)
                else
                    None)
            |> Option.defaultValue $"%s{effectName}Provider"

        let effectMember =
            context.ConfigGetter effectName
            |> Seq.tryPick (fun (key, value) ->
                if key = "ProviderPropertyName" then
                    Some(value :?> string)
                else
                    None)
            |> Option.defaultValue "Effect"

        Some(
            {
                providerPropertyName = effectMember
                effectName = effectName
                providerName = providerName
                members = members
            }
        )
    | _ -> None

/// Matches `name = true` among an attribute's arguments.
let rec private isNamedArgumentTrue (name: string) (expr: SynExpr) =
    match expr with
    | SynExpr.Paren(inner, _, _, _) -> isNamedArgumentTrue name inner
    | SynExpr.Tuple(_, exprs, _, _) -> exprs |> List.exists (isNamedArgumentTrue name)
    | SynExpr.App(_,
                  false,
                  SynExpr.App(_, true, SynExpr.LongIdent(_, SynLongIdent([ op ], _, _), _, _), SynExpr.Ident arg, _),
                  SynExpr.Const(SynConst.Bool true, _),
                  _) -> op.idText = "op_Equality" && arg.idText = name
    | _ -> false

/// Whether the type's attribute, e.g. [<GenEnvironment(Inline = true)>], asks for code to append to its file.
let isInlineRequested (attributeName: string) (SynTypeDefn(typeInfo = SynComponentInfo(attributes = attributes))) =
    attributes
    |> List.collect _.Attributes
    |> List.exists (fun attribute ->
        let name = (List.last attribute.TypeName.LongIdent).idText

        (name = attributeName || name = attributeName + "Attribute")
        && isNamedArgumentTrue "Inline" attribute.ArgExpr)

/// Code is appended to the input file when requested with Inline = true. Otherwise it goes in its
/// own file, which for `module A.B` means `namespace A` opening `A.B`.
let placement (isInline: bool) (kind: SynModuleOrNamespaceKind) (id: LongIdent) =
    let names = id |> List.map _.idText

    if isInline then
        Appended
    else
        match kind with
        | SynModuleOrNamespaceKind.NamedModule -> Namespace(List.truncate (names.Length - 1) names, [ toString id ])
        | _ -> Namespace(names, [])

let private attributedTypes (hasAttribute: SynTypeDefn -> bool) (decls: SynModuleDecl list) = [
    for decl in decls do
        match decl with
        | SynModuleDecl.Types(types, _) -> yield! List.filter hasAttribute types
        | _ -> ()
]

/// Appended code lands in the file's last namespace or module, so it can only be requested there.
let private ensureAppendable (attributeName: string) (isLast: bool) (id: LongIdent) =
    if not isLast then
        failwith
            $"%s{attributeName}: Inline = true is only supported in the last namespace or module of a file, since the code is appended to the end of the file, not to '%s{toString id}'."

let parseEffects (context: GeneratorContext) (ast: ParsedInput) : ContextEffectScope list = [
    match ast with
    | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->
        for i, SynModuleOrNamespace(longId = id; kind = kind; decls = decls) in List.indexed modules do
            let types = attributedTypes Ast.hasAttribute<GenEffectsAttribute> decls
            let isInline = types |> List.exists (isInlineRequested "GenEffects")

            if isInline then
                ensureAppendable "GenEffects" (i = modules.Length - 1) id

            { placement = placement isInline kind id; effects = types |> List.choose (effectMemberCfg context) }
    | _ -> ()
]

/// Orsak's own providers, which do not follow the `IFooProvider` -> `Effect: IFoo` convention.
/// Keyed by the provider's unqualified name; a myriad.toml section of the same name takes precedence.
let knownProviders =
    Map [
        "IGuidGenProvider", ([ "Orsak"; "IGuidGenerator" ], "GuidGenerator")
        "ITimeProvider", ([ "System"; "TimeProvider" ], "Clock")
        "ICacheProvider", ([ "Microsoft"; "Extensions"; "Caching"; "Memory"; "IMemoryCache" ], "Cache")
        "ICancellationProvider", ([ "System"; "Threading"; "CancellationTokenSource" ], "Source")
        "IRandomProvider", ([ "Orsak"; "IRandomGenerator" ], "Effect")
    ]

let private tryConfig (context: GeneratorContext) (section: string) (key: string) =
    context.ConfigGetter section
    |> Seq.tryPick (fun (k, value) -> if k = key then Some(value :?> string) else None)

/// Resolves `inherit A.IFooProvider` to the effect type `A.IFoo` and property `Effect`,
/// unless overridden by a `[IFooProvider]` section with `EffectType` / `ProviderPropertyName`.
let environmentProviderCfg (context: GeneratorContext) (environmentName: string) (providerType: string list) =
    let providerName = List.last providerType
    let known = knownProviders |> Map.tryFind providerName
    let suffix = "Provider"

    let effectType =
        match tryConfig context providerName "EffectType", known with
        | Some configured, _ -> configured.Split('.') |> List.ofArray
        | None, Some(effectType, _) -> effectType
        | None, None when providerName.EndsWith suffix && providerName.Length > suffix.Length ->
            List.truncate (providerType.Length - 1) providerType
            @ [ providerName.Substring(0, providerName.Length - suffix.Length) ]
        | None, None ->
            failwith
                $"GenEnvironment: cannot infer the effect type of '%s{providerName}' inherited by '%s{environmentName}'. Name it '...Provider', or add a [%s{providerName}] section with EffectType to myriad.toml."

    let propertyName =
        tryConfig context providerName "ProviderPropertyName"
        |> Option.orElse (known |> Option.map snd)
        |> Option.defaultValue "Effect"

    {
        providerType = providerType
        effectType = effectType
        propertyName = propertyName
        fieldName = Writer.trimI (List.last effectType)
    }

let environmentCfg (context: GeneratorContext) (SynTypeDefn(typeInfo, repr, _, _, _, _)) =
    let name = parseTypeName typeInfo
    let fail reason = failwith $"GenEnvironment: '%s{name}' %s{reason}"

    let members =
        match repr with
        | SynTypeDefnRepr.ObjectModel(_, members, _) -> members
        | _ -> fail "must be an interface."

    let providers = [
        for m in members do
            match m with
            | SynMemberDefn.Inherit(SynType.LongIdent(SynLongIdent(ids, _, _)), _, _) ->
                environmentProviderCfg context name (ids |> List.map _.idText)
            | SynMemberDefn.Inherit _ -> fail "can only inherit non-generic provider interfaces."
            | _ -> fail "can only inherit provider interfaces, and cannot declare members of its own."
    ]

    if providers.IsEmpty then
        fail "must inherit at least one provider interface."

    match providers |> List.countBy _.fieldName |> List.tryFind (fun (_, count) -> count > 1) with
    | Some(field, _) -> fail $"inherits more than one provider whose effect is named '%s{field}'."
    | None -> ()

    { name = name; providers = providers }

let private opensOf (decls: SynModuleDecl list) = [
    for decl in decls do
        match decl with
        | SynModuleDecl.Open(SynOpenDeclTarget.ModuleOrNamespace(SynLongIdent(target, _, _), _), _) -> toString target
        | _ -> ()
]

let parseEnvironments (context: GeneratorContext) (ast: ParsedInput) : ContextEnvironmentScope list = [
    match ast with
    | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->
        for i, SynModuleOrNamespace(longId = id; kind = kind; decls = decls) in List.indexed modules do
            let types = attributedTypes Ast.hasAttribute<GenEnvironmentAttribute> decls
            let isInline = types |> List.exists (isInlineRequested "GenEnvironment")

            if isInline then
                ensureAppendable "GenEnvironment" (i = modules.Length - 1) id

            let scope = {
                placement = placement isInline kind id
                openStatements = opensOf decls
                environments = types |> List.map (environmentCfg context)
            }

            // In a separate file the generated module sits next to the input module, so it cannot share its name.
            match scope.placement with
            | Namespace _ when kind = SynModuleOrNamespaceKind.NamedModule ->
                for e in scope.environments do
                    if Writer.trimI e.name = (List.last id).idText then
                        failwith
                            $"GenEnvironment: the module generated for '%s{e.name}' would have the same name as the module '%s{toString id}' that declares it."
            | _ -> ()

            scope
    | _ -> ()
]
