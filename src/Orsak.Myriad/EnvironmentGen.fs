namespace Orsak.Myriad

open Myriad.Core
open Myriad.Core.AstExtensions
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Xml
open Fantomas.FCS.Text.Range
open Orsak.Myriad.Syntax

/// [omit]
/// Builds the syntax tree for [<GenEnvironment>] interfaces:
///
/// module Environment =
///     let create (effects: {| Foo: IFoo; Bar: IBar |}) : IEnvironment = {
///         new IEnvironment
///         interface IFooProvider with
///             member _.Effect = effects.Foo
///         interface IBarProvider with
///             member _.Effect = effects.Bar
///     }
///
///     let run effects = Effect.run (create effects)
///
///     let runOrFail effects = Effect.runOrFail (create effects)
///
/// Every provider is implemented explicitly, so it does not matter that they share member names. Like
/// EffectGen's Effect.Create, run and runOrFail need the input to open Orsak: fully qualified,
/// Orsak.Effect.run would resolve to the type Effect<'r, 'a, 'e> rather than the Effect module.
module EnvironmentSyntax =
    let private parameterName = "effects"
    let private outerName = "outer"
    let private singleEffectName = "effect"

    /// `Name<A, B>`
    let private genericType (name: string list) (args: string list list) =
        SynType.App(
            longType name,
            Some range0,
            args |> List.map longType,
            List.replicate (args.Length - 1) range0,
            Some range0,
            false,
            range0
        )

    let private serviceType =
        function
        | BeginsScope(interfaceType, scope, _)
        | NestsCancellationScope(interfaceType, scope) -> genericType interfaceType [ scope ]
        | GrowsInto(interfaceType, scope, inner, _) -> genericType interfaceType [ scope; [ inner ] ]

    /// The record of effects: a field for each provider, and for each service implemented by one.
    let private effectsRecordType (providers: EnvironmentProviderCfg list) (services: EnvironmentServiceCfg list) =
        let fields = [
            for p in providers -> ident p.fieldName, longType p.effectType
            for s in services do
                match s with
                | BeginsScope(fieldName = field) -> yield ident field, serviceType s
                | GrowsInto _
                | NestsCancellationScope _ -> ()
        ]

        SynType.AnonRecd(false, fields, range0)

    /// `member self.Name = body`, or with arguments, `member self.Name args = body`, implementing an interface
    /// member, or not.
    let private memberOf (implements: bool) (self: string) (name: string) (args: SynPat list) (body: SynExpr) =
        let flags: SynMemberFlags = {
            IsInstance = true
            IsDispatchSlot = false
            IsOverrideOrExplicitImpl = implements
            IsFinal = false
            GetterOrSetterIsCompilerGenerated = false
            MemberKind = SynMemberKind.Member
        }

        let argInfos = [
            [ SynArgInfo([], false, None) ]
            [ for _ in args -> SynArgInfo([], false, None) ]
        ]

        let binding =
            SynBinding(
                None,
                SynBindingKind.Normal,
                false,
                false,
                [],
                PreXmlDoc.Empty,
                SynValData(Some flags, SynValInfo(argInfos, SynArgInfo([], false, None)), None),
                SynPat.LongIdent(SynLongIdent.Create [ self; name ], None, None, SynArgPats.Pats args, None, range0),
                None,
                body,
                range0,
                DebugPointAtBinding.NoneAtInvisible,
                { LeadingKeyword = SynLeadingKeyword.Member range0; InlineKeyword = None; EqualsRange = Some range0 }
            )

        SynMemberDefn.Member(binding, range0)

    let private memberDefn = memberOf true

    /// `(outer :> Interface)`, as providers and services often share member names.
    let private outerAs (interfaceType: SynType) =
        paren (SynExpr.Upcast(longExpr [ outerName ], interfaceType, range0))

    let private dotGet (expr: SynExpr) (name: string) =
        SynExpr.DotGet(expr, range0, SynLongIdent.Create [ name ], range0)

    /// Where the environment gets its services: the record of effects, or the environment it extends.
    type private Source =
        | FromEffects
        | FromOuter

    /// `effects.Foo`
    let private fromEffects (provider: EnvironmentProviderCfg) =
        longExpr [ parameterName; provider.fieldName ]

    /// `(outer :> IFooProvider).Effect`
    let private fromOuter (provider: EnvironmentProviderCfg) =
        dotGet (outerAs (longType provider.providerType)) provider.propertyName

    let private providerImpl (effectOf: EnvironmentProviderCfg -> SynExpr) (provider: EnvironmentProviderCfg) =
        SynInterfaceImpl(
            longType provider.providerType,
            Some range0,
            [],
            [ memberDefn "_" provider.propertyName [] (effectOf provider) ],
            range0
        )

    /// `cancellationSource` is the environment's cancellation source, when it provides one.
    let private serviceImpl (source: Source) (cancellationSource: SynExpr option) (service: EnvironmentServiceCfg) =
        // `effects.Field.Name`, or `(outer :> Interface).Name`
        let memberOf field name =
            match source with
            | FromEffects -> longExpr [ parameterName; field; name ]
            | FromOuter -> dotGet (outerAs (serviceType service)) name

        let memberImpl =
            match service with
            | BeginsScope(fieldName = field) ->
                memberDefn "_" "BeginScope" [ unitPat ] (appAtomic (memberOf field "BeginScope") unitExpr)
            | NestsCancellationScope(scopeType = scope) ->
                // `CancellationScope.NestedIn(source)`
                let nestedIn = longExpr (scope @ [ "NestedIn" ])
                memberDefn "_" "BeginScope" [ unitPat ] (appAtomic nestedIn (paren cancellationSource.Value))
            | GrowsInto(inner = inner; innerBase = innerBase) ->
                match source with
                | FromEffects ->
                    // `this` is typed as the interface being implemented, so it is cast to the environment extended.
                    // 'Inner adds only the provider of the scope, so its extend takes the scope itself.
                    let self = paren (SynExpr.Downcast(longExpr [ "this" ], longType innerBase, range0))
                    let extend = app (longExpr [ Writer.trimI inner; "extend" ]) self
                    memberDefn "this" "Grow" [ namedPat "scope" ] (app extend (longExpr [ "scope" ]))
                | FromOuter ->
                    memberDefn "_" "Grow" [ namedPat "scope" ] (app (dotGet (outerAs (serviceType service)) "Grow") (longExpr [ "scope" ]))

        SynInterfaceImpl(serviceType service, Some range0, [], [ memberImpl ], range0)

    /// Implements the environment, taking each provider's effect from `effectOf`, and the services from `serviceSource`.
    let private objectExpression
        (environment: EnvironmentCfg)
        (effectOf: EnvironmentProviderCfg -> SynExpr)
        (serviceSource: Source)
        =
        let cancellationSource =
            environment.providers
            |> List.tryFind (fun p -> List.last p.providerType = "ICancellationProvider")
            |> Option.map effectOf

        SynExpr.ObjExpr(
            longType [ environment.name ],
            None,
            None,
            [],
            [],
            [
                for provider in environment.providers -> providerImpl effectOf provider
                for service in environment.services -> serviceImpl serviceSource cancellationSource service
            ],
            range0,
            range0
        )

    /// `let run effects = Effect.run (create effects)`
    let private runner (name: string) =
        let body =
            app (longExpr [ "Effect"; name ]) (paren (app (longExpr [ "create" ]) (longExpr [ parameterName ])))

        letDecl name [ namedPat parameterName, Some parameterName ] None body

    /// `(effect: IFoo)` when the environment adds a single provider, otherwise `(effects: {| Foo: IFoo; ... |})`.
    let private ownParameter (extends: EnvironmentBaseCfg) =
        let name, typ =
            match extends.own with
            | [ provider ] -> singleEffectName, longType provider.effectType
            | own -> parameterName, effectsRecordType own []

        SynPat.Paren(SynPat.Typed(namedPat name, typ, range0), range0)

    let private ownParameterName (extends: EnvironmentBaseCfg) =
        if extends.own.Length = 1 then singleEffectName else parameterName

    /// `let extend (outer: IBase) (effects: {| ... |}) : IEnvironment`, forwarding the providers and services of
    /// `outer`, and taking the environment's own providers from the record of effects. When the environment adds
    /// a single provider, it takes that effect instead: `let extend (outer: IBase) (effect: IFoo) : IEnvironment`.
    let private extend (environment: EnvironmentCfg) (extends: EnvironmentBaseCfg) =
        let outer =
            SynPat.Paren(SynPat.Typed(namedPat outerName, longType extends.baseType, range0), range0)

        let effectOf (provider: EnvironmentProviderCfg) =
            if extends.inherited |> List.exists (fun i -> i.providerType = provider.providerType) then
                fromOuter provider
            elif extends.own.Length = 1 then
                longExpr [ singleEffectName ]
            else
                fromEffects provider

        let body = objectExpression environment effectOf FromOuter
        let ownName = ownParameterName extends

        letDecl "extend" [ outer, Some outerName; ownParameter extends, Some ownName ] (Some(longType [ environment.name ])) body

    /// The [<AutoOpen>] module `name`.
    let private autoOpenModule (name: string) (decls: SynModuleDecl list) =
        let autoOpen: SynAttribute = {
            TypeName = SynLongIdent.Create [ "AutoOpen" ]
            ArgExpr = unitExpr
            Target = None
            AppliesToGetterAndSetter = false
            Range = range0
        }

        let info =
            SynComponentInfo([ { Attributes = [ autoOpen ]; Range = range0 } ], None, [], [ ident name ], PreXmlDoc.Empty, false, None, range0)

        SynModuleDecl.NestedModule(info, false, decls, false, range0, { ModuleKeyword = Some range0; EqualsRange = Some range0 })

    /// `member this.Extend(effect: IFoo) = Environment.extend this effect`, or with a record of effects.
    let private extendMember (environment: EnvironmentCfg) (extends: EnvironmentBaseCfg) =
        let extend = app (longExpr [ Writer.trimI environment.name; "extend" ]) (longExpr [ "this" ])
        let call = app extend (longExpr [ ownParameterName extends ])
        memberOf false "this" "Extend" [ ownParameter extends ] call

    /// Identifies the type of the parameter of extend, which the Extend members of an environment overload on.
    let private ownKey (extends: EnvironmentBaseCfg) =
        match extends.own with
        | [ provider ] -> [ provider.effectType ]
        | own -> [ for p in own |> List.sortBy _.fieldName -> p.fieldName :: p.effectType ]

    /// For each environment extended by others in the scope, an [<AutoOpen>] module with a type extension:
    ///
    /// [<AutoOpen>]
    /// module BaseExtensions =
    ///     type IBase with
    ///         member this.Extend(effect: IFoo) = Environment.extend this effect
    ///
    /// The extension is optional, in a module, as namespaces can only hold the intrinsic extensions of the file
    /// that declares the type. Extend is overloaded on its parameter, so environments that extend the same one
    /// with the same effects are left out, as their Extend members could not be told apart.
    let private extensionModules (environments: EnvironmentCfg list) = [
        let extending = environments |> List.choose (fun e -> e.extends |> Option.map (fun x -> e, x))

        for baseType, group in extending |> List.groupBy (fun (_, x) -> x.baseType) do
            let members = [
                for _, overloads in group |> List.groupBy (snd >> ownKey) do
                    match overloads with
                    | [ environment, extends ] -> extendMember environment extends
                    | _ -> ()
            ]

            if not members.IsEmpty then
                let augmentation =
                    SynTypeDefn(
                        SynComponentInfo.Create(baseType |> List.map ident),
                        SynTypeDefnRepr.ObjectModel(SynTypeDefnKind.Augmentation range0, [], range0),
                        members,
                        None,
                        range0,
                        { LeadingKeyword = SynTypeDefnLeadingKeyword.Type range0; EqualsRange = None; WithKeyword = Some range0 }
                    )

                autoOpenModule (Writer.trimI (List.last baseType) + "Extensions") [ SynModuleDecl.Types([ augmentation ], range0) ]
    ]

    /// extend comes first, as an environment growing into itself uses it in create.
    let private environmentModule (environment: EnvironmentCfg) =
        let parameter =
            SynPat.Paren(
                SynPat.Typed(namedPat parameterName, effectsRecordType environment.providers environment.services, range0),
                range0
            )

        let create =
            objectExpression environment fromEffects FromEffects

        moduleDecl (Writer.trimI environment.name) [
            match environment.extends with
            | Some extends -> extend environment extends
            | None -> ()
            letDecl "create" [ parameter, Some parameterName ] (Some(longType [ environment.name ])) create
            runner "run"
            runner "runOrFail"
        ]

    /// Output for each input namespace or module that has [<GenEnvironment>] interfaces,
    /// repeating the input's opens when it gets a namespace of its own.
    let create (scopes: ContextEnvironmentScope list) : SynModuleOrNamespace list = [
        for scope in scopes do
            if not scope.environments.IsEmpty then
                place scope.placement scope.openStatements [
                    for e in scope.environments -> environmentModule e
                    yield! extensionModules scope.environments
                ]
    ]


/// <exclude/>
[<MyriadGenerator("EnvironmentGen")>]
type EnvironmentGen() =
    interface IMyriadGenerator with
        member _.ValidInputExtensions = seq { ".fs" }

        member _.Generate(context: GeneratorContext) =
            let ast, _ =
                Ast.fromFilename context.InputFilename |> Async.RunSynchronously |> Array.head

            Ast.parseEnvironments context ast |> EnvironmentSyntax.create |> Output.Ast
