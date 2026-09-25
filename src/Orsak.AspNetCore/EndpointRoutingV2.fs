/// Endpoint routing with curried route handlers, checked against the format string's route values at compile
/// time: `string -> int -> 'Eff` for "/pong/%s/%i", or `unit -> 'Eff` for a route without values.
namespace Orsak.AspNetCore.V2

open FSharp.Quotations
open FSharp.Quotations.Patterns
open Microsoft.AspNetCore.Http
open System.ComponentModel
open System.Runtime.CompilerServices
open System.Text
open Orsak.AspNetCore
open Orsak.AspNetCore.Helpers

/// Applies a curried route handler to the route values a format string produces: `int -> string -> 'Eff`
/// for the 'Tuple type `int * string`, `int -> 'Eff` for `int`, and `unit -> 'Eff` for `unit`, a route
/// without values. The tuple only picks the overload, at the call site, where the handler's and the
/// format's types are known; the values are unboxed from the parsed route values, without constructing it.
/// A single value, or unit, is passed as the 'Tuple itself, without an array.
/// A witness type rather than extension members on tuple types, since those don't solve the constraints of
/// inline functions from another assembly.
type RouteHandler =
    static member inline Apply(f: 'a -> 'r, a: 'a, _: obj array, _: RouteHandler) = f a

    static member inline Apply(f: 'a -> 'b -> 'r, _: 'a * 'b, v: obj array, _: RouteHandler) =
        f (unbox<'a> v[0]) (unbox<'b> v[1])

    static member inline Apply(f: 'a -> 'b -> 'c -> 'r, _: 'a * 'b * 'c, v: obj array, _: RouteHandler) =
        f (unbox<'a> v[0]) (unbox<'b> v[1]) (unbox<'c> v[2])

    static member inline Apply
        (f: 'a -> 'b -> 'c -> 'd -> 'r, _: 'a * 'b * 'c * 'd, v: obj array, _: RouteHandler)
        =
        f (unbox<'a> v[0]) (unbox<'b> v[1]) (unbox<'c> v[2]) (unbox<'d> v[3])

    static member inline Apply
        (f: 'a -> 'b -> 'c -> 'd -> 'e -> 'r, _: 'a * 'b * 'c * 'd * 'e, v: obj array, _: RouteHandler)
        =
        f (unbox<'a> v[0]) (unbox<'b> v[1]) (unbox<'c> v[2]) (unbox<'d> v[3]) (unbox<'e> v[4])

    static member inline Apply
        (f: 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'r, _: 'a * 'b * 'c * 'd * 'e * 'f, v: obj array, _: RouteHandler)
        =
        f (unbox<'a> v[0]) (unbox<'b> v[1]) (unbox<'c> v[2]) (unbox<'d> v[3]) (unbox<'e> v[4]) (unbox<'f> v[5])

    static member inline Apply
        (
            f: 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'g -> 'r,
            _: 'a * 'b * 'c * 'd * 'e * 'f * 'g,
            v: obj array,
            _: RouteHandler
        ) =
        f
            (unbox<'a> v[0])
            (unbox<'b> v[1])
            (unbox<'c> v[2])
            (unbox<'d> v[3])
            (unbox<'e> v[4])
            (unbox<'f> v[5])
            (unbox<'g> v[6])

    /// Called with a 'Tuple value, whose type picks the overload: the value itself for a single value or unit,
    /// or a default tuple next to the values; and an instance of RouteHandler as the witness, so the
    /// constraint has a type to look in next to the tuple.
    static member inline Invoke(handler: ^handler, tuple: ^tuple, values: obj array, witness: ^w) : ^r =
        ((^w or ^tuple): (static member Apply: ^handler * ^tuple * obj array * ^w -> ^r) (handler, tuple, values, witness))

[<Extension>]
type EffectRunnerExtensions =
    /// Like CreateEndpoint, for a curried handler matching the format's route values: `string -> int -> 'Eff`
    /// for "/pong/%s/%i", or `unit -> 'Eff` for a route without values. RouteHandler.Apply checks the handler
    /// against the format's tuple of values at the call site, and calls it with the parsed values.
    [<EditorBrowsable(EditorBrowsableState.Never)>]
    static member inline CreateEndpoint2
        (this: 'H, path: PrintfFormat<'Printer, unit, unit, ^Eff, ^T>, verb: string, handler: Expr< ^Handler>)
        =
        match handler with
        | WithValue(:? ^Handler as routeHandler, _type, expr) ->
            let parsers = routeValueParsers path.Value
            let names = if parsers.Length = 0 then [||] else getNames expr

            if names.Length <> parsers.Length then
                Throwhelpers.argumentException
                    $"The route '%s{path.Value}' has %i{parsers.Length} values, but %i{names.Length} parameter names could be read from its handler. Pass a function or lambda directly, so that its parameter names can name the route values."

            let requestDelegate =
                if parsers.Length = 0 then
                    // no route values: the unit -> 'Eff handler is called for each request
                    RequestDelegate(fun ctx ->
                        let (run: RequestDelegate) =
                            (RouteHandler.Invoke(routeHandler, Unchecked.defaultof< ^T>, null, Unchecked.defaultof<RouteHandler>)
                            : ^Eff)
                            *>> this

                        run.Invoke(ctx))
                elif parsers.Length = 1 then
                    // one route value: 'T is the value's type, so it is passed as is, without an array. Not bound
                    // with let, which would resolve RouteHandler.Invoke here, before 'T is known.
                    let parser, name = parsers[0], names[0]

                    RequestDelegate(fun ctx ->
                        let (run: RequestDelegate) =
                            (RouteHandler.Invoke(
                                routeHandler,
                                (readRouteValue parser name ctx :?> ^T),
                                null,
                                Unchecked.defaultof<RouteHandler>
                            )
                            : ^Eff)
                            *>> this

                        run.Invoke(ctx))
                else
                    RequestDelegate(fun ctx ->
                        let values = Array.zeroCreate parsers.Length

                        for i = 0 to parsers.Length - 1 do
                            values[i] <- readRouteValue parsers[i] names[i] ctx

                        let (run: RequestDelegate) =
                            (RouteHandler.Invoke(routeHandler, Unchecked.defaultof< ^T>, values, Unchecked.defaultof<RouteHandler>)
                            : ^Eff)
                            *>> this

                        run.Invoke(ctx))

            Endpoint {|
                verb = verb
                path = StringBuilder().AppendPath(path.Value, names)
                requestDelegate = requestDelegate
                conventions = id
            |}
            |> fun x -> x.WithMetadata(HandlingMethod(getMethodInfo expr))

        | _ ->
            Throwhelpers.argumentException
                "This expression is expected to be constructed with ReflectedDefinition(includeValue = true)."

            Unchecked.defaultof<_>

    [<Extension>]
    static member inline RouteGet2
        (
            this: 'H,
            path: PrintfFormat<'Printer, unit, unit, ^Eff, ^T>,
            // annotated, or compilers before F# 11 resolve RouteHandler.Apply here, to the single-value overload
            [<ReflectedDefinition(includeValue = true)>] routeHandler: Expr< ^Handler>
        ) =
        EffectRunnerExtensions.CreateEndpoint2(this, path, HttpMethods.Get, routeHandler)
