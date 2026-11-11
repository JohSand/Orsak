namespace Orsak.Myriad

open Myriad.Core
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.FCS.Xml
open Fantomas.FCS.Text.Range
open Orsak.Myriad.Syntax

/// [omit]
/// The call a proxy makes for a member, from what the member returns.
type RemoteCall =
    | TaskOf
    | UnitTask
    | AsyncOf
    | ValueTaskOf

/// [omit]
/// How a member takes its arguments, which its implementation must follow.
type RemoteArguments =
    | NoArguments
    | Single
    | Tupled
    | Curried

/// [omit]
type RemoteMemberCfg = {
    name: string
    arguments: RemoteArguments
    /// The types of all the member's arguments, its CancellationToken's too.
    argumentTypes: SynType list
    /// The position of the member's CancellationToken among its arguments, which isn't sent but cancels the call.
    token: int option
    resultType: SynType
    call: RemoteCall
}

/// [omit]
type RemoteCfg = { interfaceName: string; members: RemoteMemberCfg list }

/// [omit]
type ContextRemoteScope = { placement: Placement; remotes: RemoteCfg list }

/// [omit]
/// Reads [<GenRemote>] interfaces: each member's name, arguments and result, from its signature.
module RemoteAst =
    let private attributeName = "GenRemote"

    /// By name, as the attribute is in Orsak.Remoting, which Orsak.Myriad doesn't reference.
    let private hasGenRemote (SynTypeDefn(typeInfo = SynComponentInfo(attributes = attributes))) =
        attributes
        |> List.collect _.Attributes
        |> List.exists (fun a ->
            let name = (List.last a.TypeName.LongIdent).idText
            name = attributeName || name = attributeName + "Attribute")

    /// `a: int` in a signature is the type `int`.
    let rec private unnamed (t: SynType) =
        match t with
        | SynType.SignatureParameter(usedType = inner) -> unnamed inner
        | SynType.Paren(inner, _) -> unnamed inner
        | t -> t

    let private isUnitType (t: SynType) =
        match unnamed t with
        | SynType.LongIdent(SynLongIdent([ id ], _, _)) -> id.idText = "unit"
        | _ -> false

    let private lastName (t: SynType) =
        match unnamed t with
        | SynType.LongIdent(SynLongIdent(ids, _, _)) -> Some (List.last ids).idText
        | SynType.App(SynType.LongIdent(SynLongIdent(ids, _, _)), _, _, _, _, _, _) -> Some (List.last ids).idText
        | _ -> None

    /// The call and result type of what a member returns: a Task, ValueTask or Async of a value, or a Task of none.
    let private result (interfaceName: string) (memberName: string) (returnType: SynType) =
        match unnamed returnType with
        | SynType.App(typeName = name; typeArgs = [ resultType ]) ->
            match lastName name with
            | Some "Task" -> TaskOf, resultType
            | Some "ValueTask" -> ValueTaskOf, resultType
            | Some "Async" -> AsyncOf, resultType
            | _ -> failwith $"GenRemote: '%s{interfaceName}.%s{memberName}' must return a Task, a ValueTask or an Async."
        | t when lastName t = Some "Task" -> UnitTask, longType [ "unit" ]
        | _ -> failwith $"GenRemote: '%s{interfaceName}.%s{memberName}' must return a Task, a ValueTask or an Async."

    /// The argument types of a curried signature, `a -> b -> Task<c>`, and what it returns.
    let rec private curried (t: SynType) =
        match t with
        | SynType.Fun(argType, rest, _, _) ->
            let args, returns = curried rest
            argType :: args, returns
        | returns -> [], returns

    /// A member's CancellationToken, found by name, as a generator only sees syntax.
    let private token (interfaceName: string) (memberName: string) (call: RemoteCall) (types: SynType list) =
        match types |> List.indexed |> List.filter (fun (_, t) -> lastName t = Some "CancellationToken") with
        | [] -> None
        | [ _ ] when call = AsyncOf ->
            failwith
                $"GenRemote: '%s{interfaceName}.%s{memberName}' returns an Async, which is cancelled by its own token, so it can't take a CancellationToken."
        | [ i, _ ] -> Some i
        | _ -> failwith $"GenRemote: '%s{interfaceName}.%s{memberName}' takes more than one CancellationToken."

    let private memberCfg (interfaceName: string) (m: SynMemberDefn) =
        match m with
        | SynMemberDefn.AbstractSlot(slotSig = SynValSig(ident = SynIdent(id, _); synType = signature)) ->
            let name = id.idText

            let cfg arguments types returns =
                let call, resultType = result interfaceName name returns

                Some {
                    name = name
                    arguments = arguments
                    argumentTypes = types
                    token = token interfaceName name call types
                    resultType = resultType
                    call = call
                }

            match curried signature with
            | [ arg ], returns when isUnitType arg -> cfg NoArguments [] returns
            | [ arg ], returns ->
                match unnamed arg with
                | SynType.Tuple(path = segments) ->
                    cfg Tupled [ for s in segments do
                                    match s with
                                    | SynTupleTypeSegment.Type t -> unnamed t
                                    | _ -> () ] returns
                | t -> cfg Single [ t ] returns
            | args, returns when args.Length > 1 -> cfg Curried (List.map unnamed args) returns
            | _ -> failwith $"GenRemote: '%s{interfaceName}.%s{name}' must be a method, of one or more arguments."
        | _ -> None

    let private remoteCfg (SynTypeDefn(typeInfo, repr, _, _, _, _)) =
        let interfaceName = Ast.parseTypeName typeInfo

        match repr with
        | SynTypeDefnRepr.ObjectModel(_, members, _) -> {
            interfaceName = interfaceName
            members = members |> List.choose (memberCfg interfaceName)
          }
        | _ -> failwith $"GenRemote: '%s{interfaceName}' must be an interface."

    let parse (context: GeneratorContext) (ast: ParsedInput) : ContextRemoteScope list = [
        match ast with
        | ParsedInput.ImplFile(ParsedImplFileInput(contents = modules)) ->
            for i, SynModuleOrNamespace(longId = id; kind = kind; decls = decls) in List.indexed modules do
                let types = [
                    for decl in decls do
                        match decl with
                        | SynModuleDecl.Types(types, _) -> yield! List.filter hasGenRemote types
                        | _ -> ()
                ]

                let isInline = Ast.isInlineGeneration context attributeName types

                if isInline && not types.IsEmpty && i <> modules.Length - 1 then
                    failwith
                        $"GenRemote: inline generation only supports types in the last namespace or module of a file, since the code is appended to the end of the file, not to '%s{Ast.toString id}'."

                { placement = Ast.placement isInline kind id; remotes = List.map remoteCfg types }
        | _ -> ()
    ]

/// [omit]
/// Builds the syntax tree for [<GenRemote>] interfaces:
///
/// module OrderApiProxy =
///     let create (client: Orsak.Remoting.RemoteClient) : IOrderApi =
///         let place = client.Member("IOrderApi", "Place", [| typeof<Order> |], typeof<Result<OrderId, PlaceError>>)
///         { new IOrderApi with
///             member _.Place a = place.Task [| box a |] }
module RemoteSyntax =
    let private clientName = "client"

    let private stringExpr (s: string) =
        SynExpr.Const(SynConst.String(s, SynStringKind.Regular, range0), range0)

    let private arrayExpr (items: SynExpr list) = SynExpr.ArrayOrList(true, items, range0)

    let private typeofExpr (t: SynType) =
        SynExpr.TypeApp(longExpr [ "typeof" ], range0, [ t ], [], Some range0, range0, range0)

    /// The member's codecs, made once: `let place = client.Member("IOrderApi", "Place", [| typeof<Order> |], typeof<...>)`
    let private memberCodecs (interfaceName: string) (m: RemoteMemberCfg) =
        let call =
            appAtomic
                (longExpr [ clientName; "Member" ])
                (paren (
                    tupleExpr [
                        stringExpr interfaceName
                        stringExpr m.name
                        arrayExpr [
                            for i, t in List.indexed m.argumentTypes do
                                if Some i <> m.token then
                                    typeofExpr t
                        ]
                        typeofExpr m.resultType
                    ]
                ))

        Writer.toCamelCase m.name, call

    /// `member _.Place a = place.Task [| box a |]`, taking the arguments the way the member declares them.
    let private memberImpl (m: RemoteMemberCfg) =
        let names = [ for i in 1 .. m.argumentTypes.Length -> string (char (96 + i)) ]

        let args =
            match m.arguments with
            | NoArguments -> [ unitPat ]
            | Single -> [ namedPat names[0] ]
            | Tupled -> [ SynPat.Paren(SynPat.Tuple(false, [ for n in names -> namedPat n ], [ for _ in names.Tail -> range0 ], range0), range0) ]
            | Curried -> [ for n in names -> namedPat n ]

        // the arguments sent: all but the CancellationToken, which cancels the call
        let boxed =
            match m.arguments with
            | NoArguments -> []
            | _ -> [
                for i, n in List.indexed names do
                    if Some i <> m.token then
                        app (longExpr [ "box" ]) (longExpr [ n ])
              ]

        let callName =
            match m.call with
            | TaskOf -> "Task"
            | UnitTask -> "UnitTask"
            | AsyncOf -> "Async"
            | ValueTaskOf -> "ValueTask"

        let callee = longExpr [ Writer.toCamelCase m.name; callName ]

        let body =
            match m.token with
            | Some i -> appAtomic callee (paren (tupleExpr [ arrayExpr boxed; longExpr [ names[i] ] ]))
            | None -> app callee (arrayExpr boxed)

        let flags: SynMemberFlags = {
            IsInstance = true
            IsDispatchSlot = false
            IsOverrideOrExplicitImpl = true
            IsFinal = false
            GetterOrSetterIsCompilerGenerated = false
            MemberKind = SynMemberKind.Member
        }

        let argInfos = [ [ SynArgInfo([], false, None) ]; [ for _ in args -> SynArgInfo([], false, None) ] ]

        let binding =
            SynBinding(
                None,
                SynBindingKind.Normal,
                false,
                false,
                [],
                PreXmlDoc.Empty,
                SynValData(Some flags, SynValInfo(argInfos, SynArgInfo([], false, None)), None),
                SynPat.LongIdent(SynLongIdent.Create [ "_"; m.name ], None, None, SynArgPats.Pats args, None, range0),
                None,
                body,
                range0,
                DebugPointAtBinding.NoneAtInvisible,
                { LeadingKeyword = SynLeadingKeyword.Member range0; InlineKeyword = None; EqualsRange = Some range0 }
            )

        SynMemberDefn.Member(binding, range0)

    let private letIn (name: string) (value: SynExpr) (body: SynExpr) =
        let binding =
            SynBinding(
                None,
                SynBindingKind.Normal,
                false,
                false,
                [],
                PreXmlDoc.Empty,
                SynValData(None, SynValInfo([], SynArgInfo([], false, None)), None),
                namedPat name,
                None,
                value,
                range0,
                DebugPointAtBinding.NoneAtLet,
                { LeadingKeyword = SynLeadingKeyword.Let range0; InlineKeyword = None; EqualsRange = Some range0 }
            )

        SynExpr.LetOrUse(false, false, [ binding ], body, range0, { InKeyword = None })

    /// `let create (client: Orsak.Remoting.RemoteClient) : IOrderApi = ...`
    let private createFunction (remote: RemoteCfg) =
        let interfaceType = longType [ remote.interfaceName ]

        let proxy =
            SynExpr.ObjExpr(interfaceType, None, Some range0, [], [ for m in remote.members -> memberImpl m ], [], range0, range0)

        let body =
            List.foldBack (fun m body -> let name, call = memberCodecs remote.interfaceName m in letIn name call body) remote.members proxy

        let clientPat =
            SynPat.Paren(SynPat.Typed(namedPat clientName, longType [ "Orsak"; "Remoting"; "RemoteClient" ], range0), range0)

        letDecl "create" [ clientPat, Some clientName ] (Some interfaceType) body

    let private proxyModule (remote: RemoteCfg) =
        moduleDecl (Writer.trimI remote.interfaceName + "Proxy") [ createFunction remote ]

    let create (scopes: ContextRemoteScope list) : SynModuleOrNamespace list = [
        for scope in scopes do
            if not scope.remotes.IsEmpty then
                place scope.placement [] [ for r in scope.remotes -> proxyModule r ]
    ]

/// <exclude/>
[<MyriadGenerator("RemoteGen")>]
type RemoteGen() =
    interface IMyriadGenerator with
        member _.ValidInputExtensions = seq { ".fs" }

        member _.Generate(context: GeneratorContext) =
            let ast, _ =
                Ast.fromFilename context.InputFilename |> Async.RunSynchronously |> Array.head

            RemoteAst.parse context ast |> RemoteSyntax.create |> Output.Ast
