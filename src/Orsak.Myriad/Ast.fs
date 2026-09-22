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

let rec createEffectModule (context: GeneratorContext) (decls: SynModuleDecl list) (agg: ContextEffectScope) =
    match decls with
    | [] -> agg
    | x :: xs ->
        match x with
        | SynModuleDecl.Types(types, _) ->
            let providerTypes =
                types
                |> List.filter Ast.hasAttribute<GenEffectsAttribute>
                |> List.choose (effectMemberCfg context)

            createEffectModule context xs { agg with effects = providerTypes @ agg.effects }
        | _ -> createEffectModule context xs agg

let parseEffects (context: GeneratorContext) (ast: ParsedInput) : ContextEffectScope list = [

    match ast with
    | ParsedInput.ImplFile(ParsedImplFileInput(_name, _, _, _, _, modules, _, _, _)) ->
        for SynModuleOrNamespace(namespaceId, _, _, moduleDecls, _, _, _, _, _) in modules do
            { ContextEffectScope.effects = []; openStatements = []; ns = toString namespaceId }
            |> createEffectModule context moduleDecls
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

let rec private collectEnvironments (context: GeneratorContext) (decls: SynModuleDecl list) (acc: ContextEnvironmentScope) =
    match decls with
    | [] -> acc
    | SynModuleDecl.Types(types, _) :: rest ->
        let environments =
            types
            |> List.filter Ast.hasAttribute<GenEnvironmentAttribute>
            |> List.map (environmentCfg context)

        collectEnvironments context rest { acc with environments = acc.environments @ environments }
    | SynModuleDecl.Open(SynOpenDeclTarget.ModuleOrNamespace(SynLongIdent(target, _, _), _), _) :: rest ->
        collectEnvironments context rest { acc with openStatements = acc.openStatements @ [ toString target ] }
    | _ :: rest -> collectEnvironments context rest acc

let parseEnvironments (context: GeneratorContext) (ast: ParsedInput) : ContextEnvironmentScope list = [
    match ast with
    | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->
        for SynModuleOrNamespace(longId = id; kind = kind; decls = decls) in modules do
            let names = id |> List.map _.idText

            let scope =
                match kind with
                | SynModuleOrNamespaceKind.NamedModule ->
                    { ns = List.truncate (names.Length - 1) names; openStatements = [ toString id ]; environments = [] }
                | _ -> { ns = names; openStatements = []; environments = [] }

            let scope = collectEnvironments context decls scope

            // The generated module sits next to the input module, so it cannot share its name.
            if kind = SynModuleOrNamespaceKind.NamedModule then
                for e in scope.environments do
                    if Writer.trimI e.name = List.last names then
                        failwith
                            $"GenEnvironment: the module generated for '%s{e.name}' would have the same name as the module '%s{toString id}' that declares it."

            scope
    | _ -> ()
]
