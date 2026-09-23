namespace Orsak.Myriad

open Myriad.Core
open Myriad.Core.AstExtensions
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Xml
open Fantomas.FCS.Text.Range
open Orsak.Myriad.Syntax

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
/// Every provider is implemented explicitly, so it does not matter that they share member names.
module EnvironmentSyntax =
    let private parameterName = "effects"

    let private effectsRecordType (providers: EnvironmentProviderCfg list) =
        SynType.AnonRecd(false, [ for p in providers -> ident p.fieldName, longType p.effectType ], range0)

    /// `member _.Effect = effects.Foo`
    let private providerMember (provider: EnvironmentProviderCfg) =
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
                longExpr [ parameterName; provider.fieldName ],
                range0,
                DebugPointAtBinding.NoneAtInvisible,
                { LeadingKeyword = SynLeadingKeyword.Member range0; InlineKeyword = None; EqualsRange = Some range0 }
            )

        SynMemberDefn.Member(binding, range0)

    let private providerImpl (provider: EnvironmentProviderCfg) =
        SynInterfaceImpl(longType provider.providerType, Some range0, [], [ providerMember provider ], range0)

    let private objectExpression (environment: EnvironmentCfg) =
        SynExpr.ObjExpr(
            longType [ environment.name ],
            None,
            None,
            [],
            [],
            environment.providers |> List.map providerImpl,
            range0,
            range0
        )

    let private environmentModule (environment: EnvironmentCfg) =
        let parameter =
            SynPat.Paren(SynPat.Typed(namedPat parameterName, effectsRecordType environment.providers, range0), range0)

        moduleDecl (Writer.trimI environment.name) [
            letDecl "create" [ parameter, Some parameterName ] (Some(longType [ environment.name ])) (objectExpression environment)
        ]

    /// Output for each input namespace or module that has [<GenEnvironment>] interfaces,
    /// repeating the input's opens when it gets a namespace of its own.
    let create (scopes: ContextEnvironmentScope list) : SynModuleOrNamespace list = [
        for scope in scopes do
            if not scope.environments.IsEmpty then
                place scope.placement scope.openStatements [ for e in scope.environments -> environmentModule e ]
    ]

[<MyriadGenerator("EnvironmentGen")>]
type EnvironmentGen() =
    interface IMyriadGenerator with
        member _.ValidInputExtensions = seq { ".fs" }

        member _.Generate(context: GeneratorContext) =
            let ast, _ =
                Ast.fromFilename context.InputFilename |> Async.RunSynchronously |> Array.head

            Ast.parseEnvironments context ast |> EnvironmentSyntax.create |> Output.Ast
