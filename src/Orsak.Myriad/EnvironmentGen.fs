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

    let private effectsRecordType (providers: EnvironmentProviderCfg list) =
        SynType.AnonRecd(false, [ for p in providers -> ident p.fieldName, longType p.effectType ], range0)

    /// `member _.Effect = effects.Foo`
    let private outerName = "outer"

    /// The provider's effect, from the record of effects.
    let private fromEffects (provider: EnvironmentProviderCfg) =
        longExpr [ parameterName; provider.fieldName ]

    /// The provider's effect, from the environment being extended: `(outer :> IFooProvider).Effect`. The upcast is
    /// needed, as providers often share a property name.
    let private fromOuter (provider: EnvironmentProviderCfg) =
        SynExpr.DotGet(
            paren (SynExpr.Upcast(longExpr [ outerName ], longType provider.providerType, range0)),
            range0,
            SynLongIdent.Create [ provider.propertyName ],
            range0
        )

    let private providerMember (provider: EnvironmentProviderCfg) (effect: SynExpr) =
        let flags: SynMemberFlags = {
            IsInstance = true
            IsDispatchSlot = false
            IsOverrideOrExplicitImpl = true
            IsFinal = false
            GetterOrSetterIsCompilerGenerated = false
            MemberKind = SynMemberKind.Member
        }

        let binding =
            SynBinding(
                None,
                SynBindingKind.Normal,
                false,
                false,
                [],
                PreXmlDoc.Empty,
                SynValData(Some flags, SynValInfo([ [ SynArgInfo([], false, None) ]; [] ], SynArgInfo([], false, None)), None),
                SynPat.LongIdent(SynLongIdent.Create [ "_"; provider.propertyName ], None, None, SynArgPats.Pats [], None, range0),
                None,
                effect,
                range0,
                DebugPointAtBinding.NoneAtInvisible,
                { LeadingKeyword = SynLeadingKeyword.Member range0; InlineKeyword = None; EqualsRange = Some range0 }
            )

        SynMemberDefn.Member(binding, range0)

    let private providerImpl (provider: EnvironmentProviderCfg, effect: SynExpr) =
        SynInterfaceImpl(longType provider.providerType, Some range0, [], [ providerMember provider effect ], range0)

    /// Implements the environment, taking each provider's effect from `effectOf`.
    let private objectExpression (environment: EnvironmentCfg) (effectOf: EnvironmentProviderCfg -> SynExpr) =
        SynExpr.ObjExpr(
            longType [ environment.name ],
            None,
            None,
            [],
            [],
            [ for provider in environment.providers -> providerImpl (provider, effectOf provider) ],
            range0,
            range0
        )

    /// `let run effects = Effect.run (create effects)`
    let private runner (name: string) =
        let body =
            app (longExpr [ "Effect"; name ]) (paren (app (longExpr [ "create" ]) (longExpr [ parameterName ])))

        letDecl name [ namedPat parameterName, Some parameterName ] None body

    /// `let extend (outer: IBase) (effects: {| ... |}) : IEnvironment`, forwarding the providers of `outer`, and taking
    /// the environment's own providers from the record of effects.
    let private extend (environment: EnvironmentCfg) (extends: EnvironmentBaseCfg) =
        let outer =
            SynPat.Paren(SynPat.Typed(namedPat outerName, longType extends.baseType, range0), range0)

        let effects =
            SynPat.Paren(SynPat.Typed(namedPat parameterName, effectsRecordType extends.own, range0), range0)

        let isInherited (provider: EnvironmentProviderCfg) =
            extends.inherited |> List.exists (fun i -> i.providerType = provider.providerType)

        let body =
            objectExpression environment (fun provider ->
                if isInherited provider then fromOuter provider else fromEffects provider)

        letDecl "extend" [ outer, Some outerName; effects, Some parameterName ] (Some(longType [ environment.name ])) body

    let private environmentModule (environment: EnvironmentCfg) =
        let parameter =
            SynPat.Paren(SynPat.Typed(namedPat parameterName, effectsRecordType environment.providers, range0), range0)

        moduleDecl (Writer.trimI environment.name) [
            letDecl "create" [ parameter, Some parameterName ] (Some(longType [ environment.name ])) (objectExpression environment fromEffects)
            runner "run"
            runner "runOrFail"
            match environment.extends with
            | Some extends -> extend environment extends
            | None -> ()
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
