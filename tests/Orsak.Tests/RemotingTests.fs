/// Orsak.Remoting on .NET: the JSON it writes, and calls through MapRemote on a test server, with the client and its
/// HttpClient transport. The JSON is also checked on Fable, in Orsak.Fable.Tests, which must write the same text.
module Orsak.Tests.RemotingTests

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

/// The proxy, as a generator would write it: each member's name and types.
type TestApiProxy(client: RemoteClient) =
    let call name (args: obj array) types (resultType: Type) = client.Call("ITestApi", name, args, types, resultType)

    interface ITestApi with
        member _.Echo order = task {
            let! r = call "Echo" [| box order |] [| typeof<Order> |] typeof<Order>
            return unbox r
        }

        member _.Place order = task {
            let! r = call "Place" [| box order |] [| typeof<Order> |] typeof<Result<int, PlaceError>>
            return unbox r
        }

        member _.Add(a, b) = task {
            let! r = call "Add" [| box a; box b |] [| typeof<int>; typeof<int> |] typeof<int>
            return unbox r
        }

        member _.Twice n = async {
            let! r = call "Twice" [| box n |] [| typeof<int> |] typeof<int> |> Async.AwaitTask
            return unbox r
        }

        member _.Thrice n = ValueTask<int>(task {
            let! r = call "Thrice" [| box n |] [| typeof<int> |] typeof<int>
            return unbox r
        })

        member _.Record message = task {
            let! _ = call "Record" [| box message |] [| typeof<string> |] typeof<unit>
            ()
        }

        member _.Fail() = task {
            let! r = call "Fail" [| box () |] [| typeof<unit> |] typeof<int>
            return unbox r
        }

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
    return app, http, TestApiProxy(client) :> ITestApi
}

let apples = {
    Customer = "ada"
    Lines = [ { Product = "apple"; Quantity = 2 } ]
    Note = Some "ring twice"
}

let json (value: 'T) =
    Encoding.UTF8.GetString((JsonFormat.Instance :> IWireFormat).EncodeResult(box value, typeof<'T>))

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

        let back = (JsonFormat.Instance :> IWireFormat).DecodeResult(Encoding.UTF8.GetBytes "1.5", typeof<float>)
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
    let api = TestApiProxy(client) :> ITestApi
    let! _ = Assert.ThrowsAsync<RemoteCallException>(fun () -> api.Echo apples :> Task)
    ()
}
