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

type Endpoint = { verb: string; path: string; requestDelegate: RequestDelegate }

[<AutoOpen>]
module Helpers =   
    let getConstraint (c: char) =
        match c with
        | 'b' -> sprintf "{%s:bool}" // bool
        | 'c' -> sprintf "{%s:length(1)}" // char
        | 's' -> sprintf "{%s}" // string
        | 'i' -> sprintf "{%s:int}" // int
        | 'd' -> sprintf "{%s:long}" // int64
        | 'f' -> sprintf "{%s:double}" // float
        | 'O' ->
            sprintf
                "{%s:regex([0-9A-Fa-f]{{8}}-[0-9A-Fa-f]{{4}}-[0-9A-Fa-f]{{4}}-[0-9A-Fa-f]{{4}}-[0-9A-Fa-f]{{12}}|[0-9A-Fa-f]{{32}}|[-_0-9A-Za-z]{{22}})}" // Guid
        | _ -> failwith $"%c{c} is not a supported route format character."

    let getParser (c: char) =
        let decodeSlashes (s: string) =
            s.Replace("%2F", "/").Replace("%2f", "/")

        match c with
        | 's' -> (decodeSlashes >> box)
        | 'i' -> (int >> box)
        | 'b' -> (bool.Parse >> box)
        | 'c' -> (char >> box)
        | 'd' -> (int64 >> box)
        | 'f' -> (float >> box)
        | 'O' -> (Guid >> box)
        //| 'u' -> Some (ShortId.toUInt64 >> box)
        | _ -> box

    let inline parseRouteValue (rd: RouteData) key formatChar =
        getParser formatChar (rd.Values[key].ToString())

    let convertToRouteTemplate (path: PrintfFormat<_, _, _, _, 'T>) (names: string array) =
        let rec convert (index: int) (chars: char list) =
            match chars with
            //escape % case
            | '%' :: '%' :: tail ->
                let template, mappings = convert index tail
                "%" + template, mappings
            //some parameter
            | '%' :: c :: tail ->
                let template, mappings = convert (index + 1) tail
                getConstraint c names[index] + template, (names[index], c) :: mappings
            //regular string
            | c :: tail ->
                let template, mappings = convert index tail
                c.ToString() + template, mappings
            | [] -> "", []

        let path, mappings = path.Value |> List.ofSeq |> convert 0
        path, Array.ofList mappings

    let getNames (q: Expr) =
        //single
        match q with
        | Lambda(var, Lambda(var2, body)) -> [| var.Name |]
        | Lambda(var, Call(_, _, _)) -> [| var.Name |]
        //tuple 2
        | Lambda(_, Let(var, _, Let(var2, _, Lambda(_)))) -> [| var.Name; var2.Name |]
        | Lambda(_, Let(var, _, Let(var2, _, Call _))) -> [| var.Name; var2.Name |]
        | asd -> [||]

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

    let inline invoke (ctx) (d: RequestDelegate) = d.Invoke ctx

    type IEndpointRouteBuilder with

        member builder.MapEffectEndpoints(endpoints: Endpoint list) =
            endpoints
            |> List.iter (fun e ->                
                    builder.MapMethods(e.path, [| e.verb |], e.requestDelegate).RequireAuthorization() |> ignore
                )

[<Extension>]
type EffectRunnerExtensions =

    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline CreateEndpoint(this, path: PrintfFormat<_, _, _, _, 'T>, verb: string, handler: (Expr<'T -> 'A>)) =
        match handler with
        | WithValue(value, ``type``, expr) ->
            let eff = value :?> 'T -> 'A

            let path, mappings =
                if typeof<'T> = typeof<unit> then
                    path.ToString(), [||]
                else
                    getNames expr |> convertToRouteTemplate path

            let requestDelegate =
                if typeof<'T> = typeof<unit> then
                    eff (Unchecked.defaultof<'T>) *>> this

                //type tests for all primitives we support
                elif
                    typeof<'T> = typeof<int>
                    || typeof<'T> = typeof<bool>
                    || typeof<'T> = typeof<char>
                    || typeof<'T> = typeof<string>
                    || typeof<'T> = typeof<int64>
                    || typeof<'T> = typeof<float>
                    || typeof<'T> = typeof<Guid>
                then
                    let key, v = mappings[0]
                    let parser = getParser v
                    RequestDelegate(fun ctx ->
                        let rv = ctx.GetRouteData().Values[key].ToString()
                        eff (parser rv :?> 'T) *>> this
                        |> invoke ctx)
                else
                    //if not a single value, it is a tuple
                    let activator = createCtorFunc<'T> ()
                    //we avoid paying the cost by creating this outside the request delegate
                    //tupled types
                    RequestDelegate(fun ctx ->
                        let rd = ctx.GetRouteData()

                        let o = mappings |> Array.map ((<||) (parseRouteValue rd)) |> activator.Invoke

                        eff o *>> this |> invoke ctx)

            { verb = verb; path = path; requestDelegate = requestDelegate }

        | _ -> failwith ""

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
