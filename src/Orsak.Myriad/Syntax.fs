/// Small constructors for the Fantomas syntax nodes the generators emit,
/// filling in the ranges and trivia that do not matter for generated code.
module Orsak.Myriad.Syntax

open Myriad.Core.AstExtensions
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Xml
open Fantomas.FCS.Text.Range

let ident (name: string) = Ident(name, range0)

let longType (parts: string list) =
    SynType.LongIdent(SynLongIdent.Create parts)

/// `a.b.c` as an expression.
let longExpr (parts: string list) =
    SynExpr.LongIdent(false, SynLongIdent.Create parts, None, range0)

let paren (expr: SynExpr) = SynExpr.Paren(expr, range0, Some range0, range0)

/// `f()` / `f(a, b)` style application, without a space.
let appAtomic (funcExpr: SynExpr) (argExpr: SynExpr) =
    SynExpr.App(ExprAtomicFlag.Atomic, false, funcExpr, argExpr, range0)

/// `f a` style application.
let app (funcExpr: SynExpr) (argExpr: SynExpr) =
    SynExpr.App(ExprAtomicFlag.NonAtomic, false, funcExpr, argExpr, range0)

let unitExpr = SynExpr.Const(SynConst.Unit, range0)

let tupleExpr (exprs: SynExpr list) =
    SynExpr.Tuple(false, exprs, List.replicate (exprs.Length - 1) range0, range0)

let namedPat (name: string) =
    SynPat.Named(SynIdent(ident name, None), false, None, range0)

let unitPat = SynPat.Paren(SynPat.Const(SynConst.Unit, range0), range0)

/// `fun (name: type) -> body`
let lambda (name: string) (typ: SynType) (body: SynExpr) =
    let simplePats =
        SynSimplePats.SimplePats(
            [ SynSimplePat.Typed(SynSimplePat.Id(ident name, None, false, false, false, range0), typ, range0) ],
            [],
            range0
        )

    let pats = [ SynPat.Paren(SynPat.Typed(namedPat name, typ, range0), range0) ]
    SynExpr.Lambda(false, false, simplePats, body, Some(pats, body), range0, { ArrowRange = Some range0 })

/// `let name args : returnType = body`, where `args` are the parameter patterns with the names
/// used for their argument info. A binding without args is a value.
let letDecl (name: string) (args: (SynPat * string option) list) (returnType: SynType option) (body: SynExpr) =
    let argInfos = [
        for _, argName in args -> [
            match argName with
            | Some n -> SynArgInfo([], false, Some(ident n))
            | None -> ()
        ]
    ]

    let headPat =
        if args.IsEmpty then
            namedPat name
        else
            SynPat.LongIdent(SynLongIdent.Create [ name ], None, None, SynArgPats.Pats(List.map fst args), None, range0)

    let binding =
        SynBinding(
            None,
            SynBindingKind.Normal,
            false,
            false,
            [],
            PreXmlDoc.Empty,
            SynValData(None, SynValInfo(argInfos, SynArgInfo([], false, None)), None),
            headPat,
            returnType |> Option.map (fun t -> SynBindingReturnInfo(t, range0, [], { ColonRange = Some range0 })),
            body,
            range0,
            DebugPointAtBinding.NoneAtLet,
            { LeadingKeyword = SynLeadingKeyword.Let range0; InlineKeyword = None; EqualsRange = Some range0 }
        )

    SynModuleDecl.Let(false, [ binding ], range0)

let moduleDecl (name: string) (decls: SynModuleDecl list) =
    SynModuleDecl.NestedModule(
        SynComponentInfo.Create [ ident name ],
        false,
        decls,
        false,
        range0,
        { ModuleKeyword = Some range0; EqualsRange = Some range0 }
    )

/// `namespace a.b` followed by decls; an empty name is the global namespace.
let namespaceDecl (ns: string list) (decls: SynModuleDecl list) =
    let kind =
        if ns.IsEmpty then
            SynModuleOrNamespaceKind.GlobalNamespace
        else
            SynModuleOrNamespaceKind.DeclaredNamespace

    SynModuleOrNamespace(
        List.map ident ns,
        false,
        kind,
        decls,
        PreXmlDoc.Empty,
        [],
        None,
        range0,
        { LeadingKeyword = SynModuleOrNamespaceLeadingKeyword.Namespace range0 }
    )
