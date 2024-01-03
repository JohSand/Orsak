namespace Orsak.AspNetCore


open FSharp.Quotations
open FSharp.Quotations.Patterns
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Routing
open System
open System.Linq.Expressions
open System.Runtime.CompilerServices
open System.ComponentModel
open System.Reflection
open System.Text

type HandlingMethod = HandlingMethod of MethodInfo

type Endpoint =   
    | Endpoint of
        {|
            verb: string
            path: string
            requestDelegate: RequestDelegate
            conventions: IEndpointConventionBuilder -> IEndpointConventionBuilder
        |}

    member inline this.AddConvention([<InlineIfLambda>]f: IEndpointConventionBuilder -> IEndpointConventionBuilder) =
        let (Endpoint this) = this in Endpoint {| this with conventions = fun b -> f (this.conventions b) |}

    member this.RequiresAuthorization() =
        this.AddConvention(_.RequireAuthorization())

    member this.AllowAnonymous() =
        this.AddConvention(_.AllowAnonymous())

    member this.RequireCors(name: string) =
        this.AddConvention(_.RequireCors(name))

    member this.RequireCors(builder: Action<_>) =
        this.AddConvention(_.RequireCors(builder))

    member this.WithName(name) =
        this.AddConvention(_.WithName(name))

    member this.WithMetadata([<ParamArray>] items) =
        this.AddConvention(_.WithMetadata(items))

    member this.WithDisplayName(name: string) =
        this.AddConvention(_.WithDisplayName(name))

    member this.WithDisplayName(f: Func<_,_>) =
        this.AddConvention(_.WithDisplayName(f))

    member this.WithGroupName(name) =
        this.AddConvention(_.WithGroupName(name))

    member this.Add(f) =
        this.AddConvention(fun b -> b.Add(f); b)

[<AutoOpen>]
module Helpers =
    let getConstraint name (ep: RouteEndpoint) =
        let mutable policies = Unchecked.defaultof<_>
        if ep.RoutePattern.ParameterPolicies.TryGetValue(name, &policies) then
            policies[0].Content
        else
            ""

    let parseRouteValue (name: string, ctx: HttpContext) =
        let unEscape (s: string) = s.Replace("%2F", "/").Replace("%2f", "/")

        match ctx.GetEndpoint() :?> RouteEndpoint |> getConstraint name with
        | ""            -> (ctx.GetRouteValue(name) :?> string |> unEscape |> box)
        | "int"         -> (ctx.GetRouteValue(name) :?> string |> int |> box)
        | "bool"        -> (ctx.GetRouteValue(name) :?> string |> bool.Parse |> box)
        | "length(1)"   -> (ctx.GetRouteValue(name) :?> string |> char |> box)
        | "long"        -> (ctx.GetRouteValue(name) :?> string |> int64 |> box)
        | "double"      -> (ctx.GetRouteValue(name) :?> string |> float |> box)
        | "guid"        -> (ctx.GetRouteValue(name) :?> string |> Guid |> box)
        | _ -> ctx.GetRouteValue(name)


    type StringBuilder with
        member sb.AppendParameter(c, name) =
            match c with
            | 'b' -> sb.Append($"{{%s{name}:bool}}")
            | 'c' -> sb.Append($"{{%s{name}:length(1)}}")
            | 's' -> sb.Append($"{{%s{name}}}")
            | 'i' -> sb.Append($"{{%s{name}:int}}")
            | 'd' -> sb.Append($"{{%s{name}:long}}")
            | 'f' -> sb.Append($"{{%s{name}:double}}")
            | 'O' -> sb.Append($"{{%s{name}:guid}}")
            | _ -> failwith $"%c{c} is not a supported route format character."

        [<TailCall>]
        member sb.AppendPath(chars: char ReadOnlySpan, names: string ReadOnlySpan) =
            let paramIndex = chars.IndexOf('%')
            if paramIndex = -1 then
                sb.Append(chars).ToString()
            elif chars[paramIndex + 1] = '%' then
                sb
                  .Append(chars.Slice(0, paramIndex))
                  .Append('%')
                  .AppendPath(chars.Slice(paramIndex + 2), names)
            else
                sb
                  .Append(chars.Slice(0, paramIndex))
                  .AppendParameter(chars[paramIndex + 1], names[0])
                  .AppendPath(chars.Slice(paramIndex + 2), names.Slice(1))

    [<return: Struct>]
    let (|TupledArg|_|) (var: Var) =
        if var.Name = "tupledArg" then ValueSome() else ValueNone

    let getNames (q: Expr) =
        match q with
        | Lambda(TupledArg, Let(var, _, Let(var2, _, Let(var3, _, Let(var4, _, Let(var5, _, _)))))) 
        | Lambda(TupledArg, Let(var, _, Let(var2, _, Let(var3, _, Let(var4, _, Lambda(var5, _)))))) ->
            [| var.Name; var2.Name; var3.Name; var4.Name; var5.Name; |]
        | Lambda(TupledArg, Let(var, _, Let(var2, _, Let(var3, _, Let(var4, _, _))))) 
        | Lambda(TupledArg, Let(var, _, Let(var2, _, Let(var3, _, Lambda(var4, _))))) ->
            [| var.Name; var2.Name; var3.Name; var4.Name |]
        | Lambda(TupledArg, Let(var, _, Let(var2, _, Let(var3, _, _))))
        | Lambda(TupledArg, Let(var, _, Let(var2, _, Lambda(var3, _)))) ->
            [| var.Name; var2.Name; var3.Name |]
        | Lambda(TupledArg, Let(var, _, Let(var2, _, _)))
        | Lambda(var, Lambda(var2, _)) ->
            [| var.Name; var2.Name |]
        | Lambda(var, _) -> [| var.Name |]
        | _ -> [||]

    let getMethodInfo (q: Expr) =
        match q with
        | Lambda(TupledArg, Let(_, _, Let(_, _, Let(_, _, Let(_, _, Let(_, _, Call(_, mi, _)))))))
        | Lambda(TupledArg, Let(_, _, Let(_, _, Let(_, _, Let(_, _, Lambda(_, Call(_, mi, _)))))))
        | Lambda(TupledArg, Let(_, _, Let(_, _, Let(_, _, Let(_, _, Call(_, mi, _))))))
        | Lambda(TupledArg, Let(_, _, Let(_, _, Let(_, _, Lambda(_, Call(_, mi, _))))))
        | Lambda(TupledArg, Let(_, _, Let(_, _, Let(_, _, Call(_, mi, _)))))
        | Lambda(TupledArg, Let(_, _, Let(_, _, Lambda(_, Call(_, mi, _)))))
        | Lambda(TupledArg, Let(_, _, Let(_, _, Call(_, mi, _))))
        | Lambda(_, Lambda(_, Call(_, mi, _)))
        | Lambda(_, Call(_, mi, _)) -> mi
        | _ -> Unchecked.defaultof<_>

    let createCtorFunc<'T> () =
        let ctorInfo = typeof<'T>.GetConstructors()[0]
        let args = Expression.Parameter(typeof<obj array>, "args")

        let (ctorArgs: Expression array) =
            ctorInfo.GetParameters()
            |> Array.mapi (fun i pinfo ->
                Expression.Convert(Expression.ArrayIndex(args, Expression.Constant(i)), pinfo.ParameterType))

        Expression
            .Lambda(typeof<Func<obj array, 'T>>, Expression.New(ctorInfo, ctorArgs), args)
            .Compile()
        :?> Func<obj array, 'T>

    let inline createEndpointDelegate (eff: 'T -> 'A) (names: string []) this =
        //type tests for all primitives we support
        if
            typeof<'T> = typeof<int>
            || typeof<'T> = typeof<bool>
            || typeof<'T> = typeof<char>
            || typeof<'T> = typeof<string>
            || typeof<'T> = typeof<int64>
            || typeof<'T> = typeof<float>
            || typeof<'T> = typeof<Guid>
        then
            RequestDelegate(fun ctx ->
                let arg = parseRouteValue (names[0], ctx) :?> 'T
                let (a: RequestDelegate) = eff arg *>> this in a.Invoke(ctx))
        //if not a single value, it is a tuple
        else
            //tupled types
            let activator = createCtorFunc<'T> ()
            //we avoid paying the cost by creating this outside the request delegate
            RequestDelegate(fun ctx ->
                let argArray = Array.zeroCreate names.Length
                for i = 0 to names.Length - 1 do
                    argArray[i] <- parseRouteValue (names[i], ctx)

                eff (activator.Invoke argArray) *>> this |> _.Invoke(ctx))

    type IEndpointRouteBuilder with
        member builder.MapEffectEndpoints(endpoints: Endpoint list) =
            endpoints
            |> List.iter (fun (Endpoint e) ->
                let convBuilder = builder.MapMethods(e.path, [| e.verb |], e.requestDelegate)
                e.conventions convBuilder |> ignore
            )

[<Extension>]
type EffectRunnerExtensions =
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline CreateEndpoint
        (
            this,
            path: PrintfFormat<_, _, _, _, 'T>,
            verb: string,
            handler: (Expr<'T -> 'A>)
        ) =
        match handler with
        | WithValue(value, ``type``, expr) ->
            let eff = value :?> 'T -> 'A

            if typeof<'T> = typeof<unit> then
                Endpoint {|
                    verb = verb
                    path = path.ToString().Replace("%%", "%")
                    requestDelegate = eff (Unchecked.defaultof<'T>) *>> this
                    conventions = id
                |}   
            else
                let names = getNames expr
                Endpoint {|
                    verb = verb
                    path = StringBuilder().AppendPath(path.Value, names)
                    requestDelegate = createEndpointDelegate eff names this    
                    conventions = id
                |}
            |> _.WithMetadata(HandlingMethod (getMethodInfo expr))

        | _ -> failwith "This expression is expected to be constructed with ReflectedDefinition(includeValue = true)."

    [<Extension>]
    static member inline RouteGet(this, path, [<ReflectedDefinition(includeValue = true)>] routeHandler) =
        EffectRunnerExtensions.CreateEndpoint(this, path, HttpMethods.Get, routeHandler)

    [<Extension>]
    static member inline RoutePost(this, path, [<ReflectedDefinition(includeValue = true)>] routeHandler) =
        EffectRunnerExtensions.CreateEndpoint(this, path, HttpMethods.Post, routeHandler)

    [<Extension>]
    static member inline RoutePut(this, path, [<ReflectedDefinition(includeValue = true)>] routeHandler) =
        EffectRunnerExtensions.CreateEndpoint(this, path, HttpMethods.Put, routeHandler)

    [<Extension>]
    static member inline RoutePatch(this, path, [<ReflectedDefinition(includeValue = true)>] routeHandler) =
        EffectRunnerExtensions.CreateEndpoint(this, path, HttpMethods.Patch, routeHandler)

    [<Extension>]
    static member inline RouteDelete(this, path, [<ReflectedDefinition(includeValue = true)>] routeHandler) =
        EffectRunnerExtensions.CreateEndpoint(this, path, HttpMethods.Delete, routeHandler)

    [<Extension>]
    static member inline RouteHead(this, path, [<ReflectedDefinition(includeValue = true)>] routeHandler) =
        EffectRunnerExtensions.CreateEndpoint(this, path, HttpMethods.Head, routeHandler)

    [<Extension>]
    static member inline RouteTrace(this, path, [<ReflectedDefinition(includeValue = true)>] routeHandler) =
        EffectRunnerExtensions.CreateEndpoint(this, path, HttpMethods.Trace, routeHandler)

    [<Extension>]
    static member inline RouteConnect(this, path, [<ReflectedDefinition(includeValue = true)>] routeHandler) =
        EffectRunnerExtensions.CreateEndpoint(this, path, HttpMethods.Connect, routeHandler)

    [<Extension>]
    static member inline RouteOptions(this, path, [<ReflectedDefinition(includeValue = true)>] routeHandler) =
        EffectRunnerExtensions.CreateEndpoint(this, path, HttpMethods.Options, routeHandler)
