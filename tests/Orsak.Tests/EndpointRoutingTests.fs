module Orsak.Tests.EndpointRoutingTests

open System
open Microsoft.AspNetCore.Http
open Microsoft.AspNetCore.Routing
open Microsoft.AspNetCore.Routing.Patterns
open Orsak
open Orsak.AspNetCore
open Orsak.AspNetCore.V2
open Xunit

/// Stands in for an application's runner: runs the route handler's effect and records its result.
type RecordingRunner =
    | Recording of (obj -> unit)

    static member inline ( *>> )(effect: Effect<unit, 'a, string>, Recording record) : RequestDelegate =
        RequestDelegate(fun _ -> task {
            match! Effect.run () effect with
            | Ok a -> record (box a)
            | Error e -> failwith e
        })

/// Invokes an endpoint's request delegate the way routing would: with the endpoint and its route values set.
let invoke (Endpoint e) (routeValues: (string * string) list) = task {
    let ctx = DefaultHttpContext()
    ctx.SetEndpoint(RouteEndpoint(e.requestDelegate, RoutePatternFactory.Parse e.path, 0, EndpointMetadataCollection.Empty, "test"))

    for name, value in routeValues do
        ctx.Request.RouteValues[name] <- value

    do! e.requestDelegate.Invoke ctx
}

let describe (name: string) (count: int) : Effect<unit, string, string> = eff { return $"{name}:{count}" }

let ping () : Effect<unit, string, string> = eff { return "pong" }

let describeTupled (name: string, count: int) : Effect<unit, string, string> = eff { return $"{name}:{count}" }

let recording () =
    let result = ref null
    Recording(fun r -> result.Value <- r), result

[<Fact>]
let ``RouteGet2 calls a curried handler with the parsed route values`` () = task {
    let runner, result = recording ()
    let (Endpoint e) as endpoint = runner.RouteGet2("/items/%s/%i", describe)

    Assert.Equal("/items/{name}/{count:int}", e.path)
    do! invoke endpoint [ "name", "abc"; "count", "42" ]
    Assert.Equal(box "abc:42", result.Value)
}

[<Fact>]
let ``RouteGet2 reads parameter types and names from a curried lambda`` () = task {
    let runner, result = recording ()
    let id = Guid.NewGuid()

    let (Endpoint e) as endpoint =
        runner.RouteGet2(
            "/orders/%i/%b/%O",
            fun (order: int) (paid: bool) (customer: Guid) -> eff { return $"{order}:{paid}:{customer}" }
        )

    Assert.Equal("/orders/{order:int}/{paid:bool}/{customer:guid}", e.path)
    do! invoke endpoint [ "order", "7"; "paid", "true"; "customer", string id ]
    Assert.Equal(box $"7:True:{id}", result.Value)
}

[<Fact>]
let ``RouteGet2 without route parameters calls a unit handler`` () = task {
    let runner, result = recording ()
    let (Endpoint e) as endpoint = runner.RouteGet2("/ping", ping)

    Assert.Equal("/ping", e.path)
    do! invoke endpoint []
    Assert.Equal(box "pong", result.Value)
}

[<Fact>]
let ``RouteGet2 rejects a handler whose parameter names cannot be read`` () =
    let runner, _ = recording ()
    let handler = describe
    let error = Assert.Throws<ArgumentException>(fun () -> runner.RouteGet2("/items/%s/%i", handler) |> ignore)
    Assert.Contains("has 2 values, but 0 parameter names could be read", error.Message)

[<Fact>]
let ``RouteGet still calls a tupled handler`` () = task {
    let runner, result = recording ()
    let (Endpoint e) as endpoint = runner.RouteGet("/tupled/%s/%i", describeTupled)

    Assert.Equal("/tupled/{name}/{count:int}", e.path)
    do! invoke endpoint [ "name", "abc"; "count", "42" ]
    Assert.Equal(box "abc:42", result.Value)
}

[<Fact>]
let ``RouteGet2 calls a handler with a single parameter`` () = task {
    let runner, result = recording ()
    let endpoint = runner.RouteGet2("/one/%i", fun (n: int) -> eff { return $"one:{n}" })

    do! invoke endpoint [ "n", "5" ]
    Assert.Equal(box "one:5", result.Value)
}

[<Fact>]
let ``RouteGet2 passes more arguments than one InvokeFast call takes`` () = task {
    let runner, result = recording ()

    let endpoint =
        runner.RouteGet2(
            "/six/%i/%i/%i/%i/%i/%s",
            fun (a: int) (b: int) (c: int) (d: int) (e: int) (f: string) -> eff { return $"{a + b + c + d + e}{f}" }
        )

    do! invoke endpoint [ "a", "1"; "b", "2"; "c", "3"; "d", "4"; "e", "5"; "f", "!" ]
    Assert.Equal(box "15!", result.Value)
}

[<Fact>]
let ``RouteGet2 parses long, double and char route values`` () = task {
    let runner, result = recording ()

    let (Endpoint e) as endpoint =
        runner.RouteGet2("/rates/%d/%f/%c", fun (big: int64) (ratio: float) (grade: char) -> eff { return $"{big}:{int (ratio * 10.)}:{grade}" })

    Assert.Equal("/rates/{big:long}/{ratio:double}/{grade:length(1)}", e.path)
    do! invoke endpoint [ "big", "9000000000"; "ratio", "0.5"; "grade", "A" ]
    Assert.Equal(box "9000000000:5:A", result.Value)
}

[<Fact>]
let ``A literal %% takes no route value, and %s values are unescaped`` () = task {
    let runner, result = recording ()

    let (Endpoint e) as endpoint =
        runner.RouteGet2("/files%%/%s/%i", fun (name: string) (size: int) -> eff { return $"{name}:{size}" })

    Assert.Equal("/files%/{name}/{size:int}", e.path)
    do! invoke endpoint [ "name", "a%2Fb"; "size", "3" ]
    Assert.Equal(box "a/b:3", result.Value)
}
