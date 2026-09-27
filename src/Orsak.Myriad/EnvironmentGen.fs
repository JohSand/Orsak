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
        | BeginsScope(interfaceType, scope, _) -> genericType interfaceType [ scope ]
        | HandlesExceptions(interfaceType, error, _) -> genericType interfaceType [ error ]
        | GrowsInto(interfaceType, scope, inner, _, _) -> genericType interfaceType [ scope; [ inner ] ]

    /// The record of effects: a field for each provider, and for each service implemented by one.
    let private effectsRecordType (providers: EnvironmentProviderCfg list) (services: EnvironmentServiceCfg list) =
        let fields = [
            for p in providers -> ident p.fieldName, longType p.effectType
            for s in services do
                match s with
                | BeginsScope(fieldName = field)
                | HandlesExceptions(fieldName = field) -> yield ident field, serviceType s
                | GrowsInto _ -> ()
        ]

        SynType.AnonRecd(false, fields, range0)

    /// `member self.Name = body`, or with arguments, `member self.Name args = body`.
    let private memberDefn (self: string) (name: string) (args: SynPat list) (body: SynExpr) =
        let flags: SynMemberFlags = {
            IsInstance = true
            IsDispatchSlot = false
            IsOverrideOrExplicitImpl = true
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

    /// `(outer :> Interface)`, as providers and services often share member names.
    let private outerAs (interfaceType: SynType) =
        paren (SynExpr.Upcast(longExpr [ outerName ], interfaceType, range0))

    let private dotGet (expr: SynExpr) (name: string) =
        SynExpr.DotGet(expr, range0, SynLongIdent.Create [ name ], range0)

    /// Where the environment gets what it implements: the record of effects, or the environment it extends.
    type private Source =
        | FromEffects
        | FromOuter

    let private providerImpl (source: Source) (provider: EnvironmentProviderCfg) =
        let effect =
            match source with
            | FromEffects -> longExpr [ parameterName; provider.fieldName ]
            | FromOuter -> dotGet (outerAs (longType provider.providerType)) provider.propertyName

        SynInterfaceImpl(longType provider.providerType, Some range0, [], [ memberDefn "_" provider.propertyName [] effect ], range0)

    let private serviceImpl (source: Source) (service: EnvironmentServiceCfg) =
        // `effects.Field.Name`, or `(outer :> Interface).Name`
        let memberOf field name =
            match source with
            | FromEffects -> longExpr [ parameterName; field; name ]
            | FromOuter -> dotGet (outerAs (serviceType service)) name

        let memberImpl =
            match service with
            | BeginsScope(fieldName = field) ->
                memberDefn "_" "BeginScope" [ unitPat ] (appAtomic (memberOf field "BeginScope") unitExpr)
            | HandlesExceptions(fieldName = field) ->
                memberDefn "_" "Handle" [ namedPat "e" ] (app (memberOf field "Handle") (longExpr [ "e" ]))
            | GrowsInto(inner = inner; innerBase = innerBase; innerField = innerField) ->
                match source with
                | FromEffects ->
                    // `this` is typed as the interface being implemented, so it is cast to the environment extended
                    let self = paren (SynExpr.Downcast(longExpr [ "this" ], longType innerBase, range0))

                    let scope =
                        SynExpr.AnonRecd(
                            false,
                            None,
                            [ SynLongIdent.Create [ innerField ], Some range0, longExpr [ "scope" ] ],
                            range0,
                            { OpeningBraceRange = range0 }
                        )

                    memberDefn "this" "Grow" [ namedPat "scope" ] (app (app (longExpr [ Writer.trimI inner; "extend" ]) self) scope)
                | FromOuter ->
                    memberDefn "_" "Grow" [ namedPat "scope" ] (app (dotGet (outerAs (serviceType service)) "Grow") (longExpr [ "scope" ]))

        SynInterfaceImpl(serviceType service, Some range0, [], [ memberImpl ], range0)

    /// Implements the environment, taking each provider and service from `sourceOf`.
    let private objectExpression
        (environment: EnvironmentCfg)
        (providerSource: EnvironmentProviderCfg -> Source)
        (serviceSource: Source)
        =
        SynExpr.ObjExpr(
            longType [ environment.name ],
            None,
            None,
            [],
            [],
            [
                for provider in environment.providers -> providerImpl (providerSource provider) provider
                for service in environment.services -> serviceImpl serviceSource service
            ],
            range0,
            range0
        )

    /// `let run effects = Effect.run (create effects)`
    let private runner (name: string) =
        let body =
            app (longExpr [ "Effect"; name ]) (paren (app (longExpr [ "create" ]) (longExpr [ parameterName ])))

        letDecl name [ namedPat parameterName, Some parameterName ] None body

    /// `let extend (outer: IBase) (effects: {| ... |}) : IEnvironment`, forwarding the providers and services of
    /// `outer`, and taking the environment's own providers from the record of effects.
    let private extend (environment: EnvironmentCfg) (extends: EnvironmentBaseCfg) =
        let outer =
            SynPat.Paren(SynPat.Typed(namedPat outerName, longType extends.baseType, range0), range0)

        let effects =
            SynPat.Paren(SynPat.Typed(namedPat parameterName, effectsRecordType extends.own [], range0), range0)

        let providerSource (provider: EnvironmentProviderCfg) =
            if extends.inherited |> List.exists (fun i -> i.providerType = provider.providerType) then
                FromOuter
            else
                FromEffects

        let body = objectExpression environment providerSource FromOuter
        letDecl "extend" [ outer, Some outerName; effects, Some parameterName ] (Some(longType [ environment.name ])) body

    /// extend comes first, as an environment growing into itself uses it in create.
    let private environmentModule (environment: EnvironmentCfg) =
        let parameter =
            SynPat.Paren(
                SynPat.Typed(namedPat parameterName, effectsRecordType environment.providers environment.services, range0),
                range0
            )

        let create =
            objectExpression environment (fun _ -> FromEffects) FromEffects

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
                place scope.placement scope.openStatements [ for e in scope.environments -> environmentModule e ]
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
