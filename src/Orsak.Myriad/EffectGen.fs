namespace Orsak.Myriad

open Myriad.Core
open Myriad.Core.AstExtensions
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Xml
open Fantomas.FCS.Text.Range
open Orsak.Myriad.Syntax

/// [omit]
/// Builds the syntax tree for [<GenEffects>] interfaces:
///
/// type IFooProvider =
///     abstract Effect: IFoo
///
/// module Foo =
///     let bar a b =
///         Effect.Create(fun (er: #IFooProvider) -> er.Effect.Bar a b)
module EffectSyntax =
    let private providerParameter = "er"

    /// `abstract Effect: IFoo`
    let private abstractProperty (name: string) (typ: SynType) =
        let flags: SynMemberFlags = {
            IsInstance = true
            IsDispatchSlot = true
            IsOverrideOrExplicitImpl = false
            IsFinal = false
            GetterOrSetterIsCompilerGenerated = false
            MemberKind = SynMemberKind.PropertyGet
        }

        let signature =
            SynValSig(
                [],
                SynIdent(ident name, None),
                SynValTyparDecls(None, true),
                typ,
                SynValInfo([], SynArgInfo([], false, None)),
                false,
                false,
                PreXmlDoc.Empty,
                SynValSigAccess.Single None,
                None,
                range0,
                {
                    LeadingKeyword = SynLeadingKeyword.Abstract range0
                    InlineKeyword = None
                    WithKeyword = None
                    EqualsRange = None
                }
            )

        SynMemberDefn.AbstractSlot(signature, flags, range0, { GetSetKeywords = None })

    /// The provider is declared here, so its configured name must be a plain type name.
    let private providerName (effect: EffectProviderCfg) =
        if effect.providerName.Contains '.' then
            failwith
                $"GenEffects: ProviderName '%s{effect.providerName}' for '%s{effect.effectName}' must be a plain type name, since the provider is generated."

        effect.providerName

    let private providerType (effect: EffectProviderCfg) =
        let info = SynComponentInfo.Create [ ident (providerName effect) ]

        let repr =
            SynTypeDefnRepr.ObjectModel(
                SynTypeDefnKind.Unspecified,
                [ abstractProperty effect.providerPropertyName (longType [ effect.effectName ]) ],
                range0
            )

        let typeDefn =
            SynTypeDefn(
                info,
                repr,
                [],
                None,
                range0,
                { LeadingKeyword = SynTypeDefnLeadingKeyword.Type range0; EqualsRange = Some range0; WithKeyword = None }
            )

        SynModuleDecl.Types([ typeDefn ], range0)

    /// `let bar a b = Effect.Create(fun (er: #IFooProvider) -> er.Effect.Bar a b)`,
    /// passing the arguments the way the member declares them: `()`, tupled or curried.
    let private effectFunction (effect: EffectProviderCfg) (m: EffectMemberCfg) =
        let names = [ for i in 1 .. m.argumentCount -> string (char (96 + i)) ]
        let target = longExpr [ providerParameter; effect.providerPropertyName; m.memberName ]

        let args, call =
            if m.isUnit then
                [ unitPat, None ], appAtomic target unitExpr
            elif m.isTuple then
                [ for n in names -> namedPat n, Some n ], appAtomic target (paren (tupleExpr [ for n in names -> longExpr [ n ] ]))
            else
                [ for n in names -> namedPat n, Some n ], names |> List.fold (fun f n -> app f (longExpr [ n ])) target

        let providerConstraint =
            SynType.HashConstraint(longType [ providerName effect ], range0)

        let body =
            appAtomic (longExpr [ "Effect"; "Create" ]) (paren (lambda providerParameter providerConstraint call))

        letDecl (Writer.toCamelCase m.memberName) args None body

    let private effectModule (effect: EffectProviderCfg) =
        moduleDecl (Writer.trimI effect.effectName) [ for m in effect.members -> effectFunction effect m ]

    /// Output for each input namespace or module that has [<GenEffects>] interfaces. Some Effect.Create
    /// overloads are extensions that only `open Orsak` brings into scope, which appended code relies on the file for.
    let create (scopes: ContextEffectScope list) : SynModuleOrNamespace list = [
        for scope in scopes do
            if not scope.effects.IsEmpty then
                place scope.placement [ "Orsak" ] [
                    for e in scope.effects do
                        providerType e
                        effectModule e
                ]
    ]

/// <exclude/>
[<MyriadGenerator("EffectGen")>]
type EffectGen() =
    interface IMyriadGenerator with
        member _.ValidInputExtensions = seq { ".fs" }

        member _.Generate(context: GeneratorContext) =
            let ast, _ =
                Ast.fromFilename context.InputFilename |> Async.RunSynchronously |> Array.head

            Ast.parseEffects context ast |> EffectSyntax.create |> Output.Ast
