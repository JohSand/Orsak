namespace Orsak.AspNetCore.Alt

open System
open System.Reflection
open System.Runtime.CompilerServices
open System.Text
open System.Text.Json
open Microsoft.AspNetCore.Http
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Orsak.AspNetCore
open Orsak.AspNetCore.Helpers

#nowarn "3535"

type A<'t, 'e> =
    static abstract Deserialize: JsonDocument -> Result<'t, 'e>

type IHandleEffect<'Eff, 'Err> =
    abstract AsDelegate: 'Eff -> RequestDelegate
    abstract HandleError: 'Err -> RequestDelegate

type HandlingMethod = HandlingMethod of MethodInfo


type EndpointBinder<'F, 'Eff, 'Err, 'T> = {
    format: PrintfFormat<'F, unit, unit, 'Eff, 'T>
    runner: IHandleEffect<'Eff, 'Err>
    verb: string
}

type EffectEndpoint = { verb: string; path: string; requestDelegate: RequestDelegate }

module Reflection =

    open FastExpressionCompiler.LightExpression

    let deserialize<'B, 'Err when 'B :> A<'B, 'Err>> (doc: JsonDocument) = 'B.Deserialize(doc)

    let tupleConstructor<'T> () =
        let ctorInfo = typeof<'T>.GetConstructors()[0]
        let args = Expression.Parameter(typeof<obj array>, "args")

        let (ctorArgs: Expression array) =
            ctorInfo.GetParameters()
            |> Array.mapi (fun i pinfo ->
                Expression.Convert(Expression.ArrayIndex(args, Expression.Constant(i)), pinfo.ParameterType))

        Expression.Lambda(typeof<Func<obj array, 'T>>, Expression.New(ctorInfo, ctorArgs), args).CompileFast()
        :?> Func<obj array, 'T>

    let mkRequestDelegate (binder: EndpointBinder<'F, 'Eff, 'Err, 'T>) (expr: Expr) (adapted: Func<'T, 'Eff>) =
        let names = getNames expr
        let activator = tupleConstructor<'T> ()

        {
            verb = binder.verb
            path = StringBuilder().AppendPath(binder.format.Value, names)
            requestDelegate =
                RequestDelegate(fun (ctx: HttpContext) -> task {
                    let argArray = Array.zeroCreate names.Length

                    for i = 0 to names.Length - 1 do
                        argArray[i] <- parseRouteValue (names[i], ctx)

                    let x = activator.Invoke argArray
                    let effect = adapted.Invoke(x)
                    return! binder.runner.AsDelegate(effect).Invoke(ctx)
                })
        }

    let mkEndpoint<'Eff, 'Err, 'T, 'B when 'B :> A<'B, 'Err>>
        (runner: IHandleEffect<'Eff, 'Err>)
        (path: string)
        (verb: string)
        (expr: Expr)
        (adapted: Func<'T, 'Eff>)
        =
        let names = getNames expr
        let activator = tupleConstructor<'T> ()

        {
            verb = verb
            path = StringBuilder().AppendPath(path, names[1..])
            requestDelegate =
                RequestDelegate(fun (ctx: HttpContext) -> task {
                    let! doc = JsonDocument.ParseAsync(ctx.Request.Body)

                    match deserialize<'B, 'Err> doc with
                    | Ok(j: 'B) ->
                        let argArray = Array.zeroCreate names.Length
                        argArray[0] <- box j

                        for i = 1 to names.Length - 1 do
                            argArray[i] <- parseRouteValue (names[i], ctx)

                        let effect = adapted.Invoke(activator.Invoke argArray)
                        return! runner.AsDelegate(effect).Invoke(ctx)
                    | Error(err: 'Err) -> return! runner.HandleError(err).Invoke(ctx)
                })
        }

type Router =
    [<Extension>]
    static member Get(i: IHandleEffect<'Eff, 'Err>, route: PrintfFormat<'F, unit, unit, 'Eff, 'T>) = {
        format = route
        runner = i
        verb = "GET"
    }

type Invoker =
    [<Extension>]
    static member Invoke
        (
            binder: EndpointBinder<'Eff, 'Eff, 'Err, unit>,
            [<ReflectedDefinition(includeValue = true)>] expr: Expr<unit -> 'Eff>
        ) =
        match expr with
        | WithValue(:? (unit -> 'Eff) as f, _type, _) -> {
            verb = binder.verb
            path = binder.format.ToString().Replace("%%", "%")
            requestDelegate =
                RequestDelegate(fun (ctx: HttpContext) -> task {

                    let effect = f ()
                    return! binder.runner.AsDelegate(effect).Invoke(ctx)
                })
          }
        | _ -> Throwhelpers.invalidCreation ()

    [<Extension>]
    static member Invoke
        (
            binder: EndpointBinder<'T1 -> 'Eff, 'Eff, 'Err, 'T1>,
            [<ReflectedDefinition(includeValue = true)>] expr: Expr<'T1 -> 'Eff>
        ) =
        match expr with
        | WithValue(:? ('T1 -> 'Eff) as f, _type, expr) -> Reflection.mkRequestDelegate binder expr (Func<'T1, 'Eff>(f))
        | _ -> Throwhelpers.invalidCreation ()

    [<Extension>]
    static member Invoke
        (
            binder: EndpointBinder<'T1 -> 'T2 -> 'Eff, 'Eff, 'Err, 'T1 * 'T2>,
            [<ReflectedDefinition(includeValue = true)>] expr: Expr<'T1 -> 'T2 -> 'Eff>
        ) =
        match expr with
        | WithValue(:? ('T1 -> 'T2 -> 'Eff) as f, _type, expr) ->
            let adapted = OptimizedClosures.FSharpFunc<'T1, 'T2, 'Eff>.Adapt(f)
            Reflection.mkRequestDelegate binder expr (Func<_, _>(adapted.Invoke))
        | _ -> Throwhelpers.invalidCreation ()



    [<Extension>]
    static member Invoke<'T1, 'T2, 'Eff, 'Err, 'B when 'B :> A<'B, 'Err>>
        (
            b: EndpointBinder<'T1 -> 'T2 -> 'Eff, 'Eff, 'Err, 'T1 * 'T2>,
            [<ReflectedDefinition(includeValue = true)>] expr: Expr<'B -> 'T1 -> 'T2 -> 'Eff>
        ) =
        match expr with
        | WithValue(:? ('B -> 'T1 -> 'T2 -> 'Eff) as f, _type, expr) ->
            let fsf = OptimizedClosures.FSharpFunc<'B, 'T1, 'T2, 'Eff>.Adapt(f)
            let func = Func<'B * 'T1 * 'T2, 'Eff>(fsf.Invoke)

            Reflection.mkEndpoint<_, _, _, 'B> b.runner b.format.Value b.verb expr func
        | _ -> Throwhelpers.invalidCreation ()

    [<Extension>]
    static member Invoke
        (
            binder: EndpointBinder<'T1 -> 'T2 -> 'T3 -> 'Eff, 'Eff, 'Err, 'T1 * 'T2 * 'T3>,
            [<ReflectedDefinition(includeValue = true)>] expr: Expr<'T1 -> 'T2 -> 'T3 -> 'Eff>
        ) =
        match expr with
        | WithValue(:? ('T1 -> 'T2 -> 'T3 -> 'Eff) as f, _type, expr) ->
            let adapted = OptimizedClosures.FSharpFunc<'T1, 'T2, 'T3, 'Eff>.Adapt(f)
            Reflection.mkRequestDelegate binder expr (Func<_, _>(adapted.Invoke))
        | _ -> Throwhelpers.invalidCreation ()

open Orsak

type EffectHandler =
    | EffectHandler

    member this.HandleError(_var0: string) : RequestDelegate = failwith "todo"

    interface IHandleEffect<Effect<HttpContext, int, string>, string> with
        override this.AsDelegate(e: Effect<HttpContext, int, string>) =
            RequestDelegate(fun (ctx: HttpContext) -> task {
                match! e |> Effect.run ctx with
                | Ok _ -> ()
                | Error e -> return! this.HandleError(e).Invoke(ctx)
            })

        member this.HandleError(e) = this.HandleError(e)

type A<'t> = A<'t, string>

type B() =
    interface A<B> with
        static member Deserialize(_: JsonDocument) = Ok(B())

module Test =
    let test () =
        EffectHandler.Get("%i%s").Invoke(fun (_: int) _ -> eff { return 1 })
