/// Orsak.Remoting on .NET: the JSON it writes, and calls through MapRemote on a test server, with the client and its
/// HttpClient transport, through the proxy [<GenRemote>] generates, appended to this file. The JSON is also checked on
/// Fable, in Orsak.Fable.Tests, which must write the same text.
module rec Orsak.Tests.RemotingTests

open System
open System.Net
open System.Net.Http
open System.Text
open System.Threading.Tasks
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.TestHost
open Microsoft.Extensions.Logging
open Orsak.Remoting
open Orsak.Remoting.AspNetCore
open Xunit

type Line = { Product: string; Quantity: int }

type Order = {
    Customer: string
    Lines: Line list
    Note: string option
}

type PlaceError =
    | EmptyOrder
    | UnknownProduct of string

[<GenRemote>]
type ITestApi =
    abstract Echo: Order -> Task<Order>
    abstract Place: Order -> Task<Result<int, PlaceError>>
    abstract Add: a: int * b: int -> Task<int>
    abstract Twice: int -> Async<int>
    abstract Thrice: int -> ValueTask<int>
    abstract Record: string -> Task
    abstract Fail: unit -> Task<int>

type TestApi() =
    member val Recorded = ResizeArray<string>()

    interface ITestApi with
        member _.Echo order = Task.FromResult order

        member _.Place order =
            match order.Lines with
            | [] -> Task.FromResult(Error EmptyOrder)
            | lines ->
                match lines |> List.tryFind (fun l -> l.Product <> "apple") with
                | Some l -> Task.FromResult(Error(UnknownProduct l.Product))
                | None -> Task.FromResult(Ok lines.Length)

        member _.Add(a, b) = Task.FromResult(a + b)
        member _.Twice n = async { return n * 2 }
        member _.Thrice n = ValueTask<int>(n * 3)

        member this.Record message =
            this.Recorded.Add message
            Task.CompletedTask

        member _.Fail() = failwith "a secret in the message"

/// A type that contains itself.
type Tree =
    | Leaf
    | Node of int * Tree list

/// A contract with a type JsonFormat can't write: a Map.
type IUnsupportedApi =
    abstract Totals: unit -> Task<Map<string, int>>

/// A test server mapping ITestApi to the implementation, and a proxy calling it.
let serve (api: TestApi) = task {
    let builder = WebApplication.CreateBuilder()
    builder.WebHost.UseTestServer() |> ignore
    builder.Logging.ClearProviders() |> ignore
    let app = builder.Build()
    app.MapRemote<ITestApi>("/api", [ JsonFormat.Instance ], fun _ -> api) |> ignore
    do! app.StartAsync()
    let http = app.GetTestClient()
    let client = RemoteClient("http://localhost", "/api", JsonFormat.Instance, HttpClientTransport(http))
    return app, http, TestApiProxy.create client
}

let apples = {
    Customer = "ada"
    Lines = [ { Product = "apple"; Quantity = 2 } ]
    Note = Some "ring twice"
}

let json (value: 'T) =
    Encoding.UTF8.GetString((JsonFormat.Instance :> IWireFormat).CodecFor(typeof<'T>).Encode(box value))

[<Fact>]
let ``JsonFormat writes a record with a list and an option as JSON`` () =
    Assert.Equal(
        """{"Customer":"ada","Lines":[{"Product":"apple","Quantity":2}],"Note":"ring twice"}""",
        json apples
    )

[<Fact>]
let ``JsonFormat writes None as null, a union case as its name or an object, and int64 as a string`` () =
    Assert.Equal("""null""", json (None: string option))
    Assert.Equal("""["EmptyOrder",{"UnknownProduct":"plum"}]""", json [ EmptyOrder; UnknownProduct "plum" ])
    Assert.Equal("\"9007199254740993\"", json 9007199254740993L)

[<Fact>]
let ``JsonFormat writes and reads numbers in the invariant culture`` () =
    let culture = Globalization.CultureInfo.CurrentCulture

    try
        Globalization.CultureInfo.CurrentCulture <- Globalization.CultureInfo("sv-SE")
        Assert.Equal("1.5", json 1.5)

        let back = (JsonFormat.Instance :> IWireFormat).CodecFor(typeof<float>).Decode(Encoding.UTF8.GetBytes "1.5")
        Assert.Equal(1.5, unbox<float> back)
    finally
        Globalization.CultureInfo.CurrentCulture <- culture

[<Fact(Timeout = 10_000)>]
let ``a record with a list and an option crosses the wire both ways`` () = task {
    let! app, _, api = serve (TestApi())
    use _ = app
    let! echoed = api.Echo apples
    let! withoutNote = api.Echo { apples with Note = None; Lines = [] }
    Assert.Equal(apples, echoed)
    Assert.Equal({ apples with Note = None; Lines = [] }, withoutNote)
}

[<Fact(Timeout = 10_000)>]
let ``a contract's errors come back as values`` () = task {
    let! app, _, api = serve (TestApi())
    use _ = app
    let! placed = api.Place apples
    let! empty = api.Place { apples with Lines = [] }
    let! unknown = api.Place { apples with Lines = [ { Product = "plum"; Quantity = 1 } ] }
    Assert.Equal<Result<int, PlaceError>>(Ok 1, placed)
    Assert.Equal<Result<int, PlaceError>>(Error EmptyOrder, empty)
    Assert.Equal<Result<int, PlaceError>>(Error(UnknownProduct "plum"), unknown)
}

[<Fact(Timeout = 10_000)>]
let ``members with two arguments, or returning Async, ValueTask or a task without a value, are called`` () = task {
    let testApi = TestApi()
    let! app, _, api = serve testApi
    use _ = app
    let! sum = api.Add(2, 3)
    let! twice = api.Twice 4
    let! thrice = api.Thrice 5
    do! api.Record "called"
    Assert.Equal((5, 8, 15), (sum, twice, thrice))
    Assert.Equal<string list>([ "called" ], List.ofSeq testApi.Recorded)
}

[<Fact(Timeout = 10_000)>]
let ``an exception on the server is raised as a RemoteCallException, without its details`` () = task {
    let! app, _, api = serve (TestApi())
    use _ = app
    let! e = Assert.ThrowsAsync<RemoteCallException>(fun () -> api.Fail() :> Task)
    Assert.Contains("500", e.Message)
    Assert.DoesNotContain("secret", e.Message)
}

[<Fact(Timeout = 10_000)>]
let ``a request in a format the server doesn't have is answered 415`` () = task {
    let! app, http, _ = serve (TestApi())
    use _ = app
    use content = new ByteArrayContent([| 1uy |])
    content.Headers.ContentType <- Headers.MediaTypeHeaderValue.Parse "application/x-msgpack"
    let! response = http.PostAsync("/api/ITestApi/Echo", content)
    Assert.Equal(HttpStatusCode.UnsupportedMediaType, response.StatusCode)
}

[<Fact(Timeout = 10_000)>]
let ``a server that can't be reached is a RemoteCallException`` () = task {
    use http = new HttpClient()
    let client = RemoteClient("http://localhost:1", "/api", JsonFormat.Instance, HttpClientTransport(http))
    let api = TestApiProxy.create client
    let! _ = Assert.ThrowsAsync<RemoteCallException>(fun () -> api.Echo apples :> Task)
    ()
}

[<Fact>]
let ``a type the format can't write fails when the proxy's member is made, naming the type`` () =
    use http = new HttpClient()
    let client = RemoteClient("http://localhost", "/api", JsonFormat.Instance, HttpClientTransport(http))
    let e = Assert.ThrowsAny<exn>(fun () -> client.Member("IUnsupportedApi", "Totals", [| typeof<unit> |], typeof<Map<string, int>>) |> ignore)
    Assert.Contains("FSharpMap", e.Message)

[<Fact>]
let ``a type the format can't write fails when MapRemote maps the member`` () =
    let builder = WebApplication.CreateBuilder()
    builder.WebHost.UseTestServer() |> ignore
    use app = builder.Build()

    let e =
        Assert.ThrowsAny<exn>(fun () ->
            app.MapRemote<IUnsupportedApi>("/api", [ JsonFormat.Instance ], fun _ -> Unchecked.defaultof<_>) |> ignore)

    Assert.Contains("FSharpMap", e.Message)

[<Fact>]
let ``a type that contains itself has a codec`` () =
    Assert.Equal("""{"Node":[1,["Leaf",{"Node":[2,[]]}]]}""", json (Node(1, [ Leaf; Node(2, []) ])))
//------------------------------------------------------------------------------
//        This code was generated by myriad.
//        Changes to this file will be lost when the code is regenerated.
//------------------------------------------------------------------------------



module TestApiProxy =
    let create (client: Orsak.Remoting.RemoteClient) : ITestApi =
        let echo = client.Member("ITestApi", "Echo", [| typeof<Order> |], typeof<Order>)

        let place =
            client.Member("ITestApi", "Place", [| typeof<Order> |], typeof<Result<int, PlaceError>>)

        let add =
            client.Member("ITestApi", "Add", [| typeof<int>; typeof<int> |], typeof<int>)

        let twice = client.Member("ITestApi", "Twice", [| typeof<int> |], typeof<int>)
        let thrice = client.Member("ITestApi", "Thrice", [| typeof<int> |], typeof<int>)
        let record = client.Member("ITestApi", "Record", [| typeof<string> |], typeof<unit>)
        let fail = client.Member("ITestApi", "Fail", [| typeof<unit> |], typeof<int>)

        { new ITestApi with
            member _.Echo a = echo.Task [| box a |]
            member _.Place a = place.Task [| box a |]
            member _.Add(a, b) = add.Task [| box a; box b |]
            member _.Twice a = twice.Async [| box a |]
            member _.Thrice a = thrice.ValueTask [| box a |]
            member _.Record a = record.UnitTask [| box a |]
            member _.Fail() = fail.Task [| box () |]
        }
