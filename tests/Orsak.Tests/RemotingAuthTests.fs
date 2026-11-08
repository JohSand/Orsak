/// Authorization with Orsak.Remoting, as the Remoting guide shows it: attributes on a contract's members, which
/// ASP.NET Core enforces before the call and a client sees as RemoteCallException's status, and a resource-based policy
/// checked in an effect, which answers with the contract's own error. The proxy is generated, appended to this file.
module rec Orsak.Tests.RemotingAuthTests

open System.Collections.Concurrent
open System.Net.Http
open System.Security.Claims
open System.Text.Encodings.Web
open System.Threading.Tasks
open Microsoft.AspNetCore.Authentication
open Microsoft.AspNetCore.Authorization
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.AspNetCore.TestHost
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Options
open Orsak
open Orsak.Remoting
open Orsak.Remoting.AspNetCore
open Xunit

type Doc = { Id: int; Owner: string; Title: string }

type DocError =
    | Forbidden
    | NotFound of id: int

[<GenRemote>]
type IDocsApi =
    abstract Ping: unit -> Task<string>

    [<Authorize>]
    abstract WhoAmI: unit -> Task<string>

    [<Authorize(Roles = "editor")>]
    abstract Publish: id: int -> Task<string>

    [<Authorize>]
    abstract Rename: id: int * title: string -> Task<Result<Doc, DocError>>

/// A scheme for the tests: the user is the X-User header, "name" or "name;role,role".
type TestAuthHandler(options: IOptionsMonitor<AuthenticationSchemeOptions>, logger: ILoggerFactory, encoder: UrlEncoder) =
    inherit AuthenticationHandler<AuthenticationSchemeOptions>(options, logger, encoder)

    override this.HandleAuthenticateAsync() =
        match this.Request.Headers.TryGetValue "X-User" with
        | true, value ->
            let parts = (string value).Split ';'

            let claims = [
                yield Claim(ClaimTypes.Name, parts[0])
                if parts.Length > 1 then
                    for role in parts[1].Split ',' -> Claim(ClaimTypes.Role, role)
            ]

            let principal = ClaimsPrincipal(ClaimsIdentity(claims, "Test"))
            Task.FromResult(AuthenticateResult.Success(AuthenticationTicket(principal, "Test")))
        | _ -> Task.FromResult(AuthenticateResult.NoResult())

/// The resource-based policy "owner": only a document's owner may change it.
type OwnerRequirement() =
    interface IAuthorizationRequirement

type OwnerHandler() =
    inherit AuthorizationHandler<OwnerRequirement, Doc>()

    override _.HandleRequirementAsync(context, requirement, doc) =
        if context.User.Identity.Name = doc.Owner then
            context.Succeed requirement

        Task.CompletedTask

// The server's environment, made for each request: its user, ASP.NET Core's authorization, and the store.
type IUserProvider =
    abstract User: ClaimsPrincipal

type IAuthorizationProvider =
    abstract Authorization: IAuthorizationService

type IDocStoreProvider =
    abstract Docs: ConcurrentDictionary<int, Doc>

type ServerEnv = {
    User: ClaimsPrincipal
    Authorization: IAuthorizationService
    Docs: ConcurrentDictionary<int, Doc>
} with

    interface IUserProvider with
        member this.User = this.User

    interface IAuthorizationProvider with
        member this.Authorization = this.Authorization

    interface IDocStoreProvider with
        member this.Docs = this.Docs

module Authz =
    /// Fails with the contract's Forbidden unless the user may do to the resource what the policy says.
    let authorize (resource: obj) (policy: string) = eff {
        let! user = Effect.Create(fun (p: #IUserProvider) -> p.User)
        let! authorization = Effect.Create(fun (p: #IAuthorizationProvider) -> p.Authorization)
        let! result = authorization.AuthorizeAsync(user, resource, policy)

        if not result.Succeeded then
            return! Error Forbidden
    }

let rename id title = eff {
    let! docs = Effect.Create(fun (p: #IDocStoreProvider) -> p.Docs)

    match docs.TryGetValue id with
    | false, _ -> return! Error(NotFound id)
    | true, doc ->
        do! Authz.authorize doc "owner"
        let renamed = { doc with Title = title }
        docs[id] <- renamed
        return renamed
}

type DocsApi(env: ServerEnv) =
    interface IDocsApi with
        member _.Ping() = Task.FromResult "pong"
        member _.WhoAmI() = Task.FromResult env.User.Identity.Name
        member _.Publish id = Task.FromResult $"published {id}"
        member _.Rename(id, title) = (rename id title |> Effect.run env).AsTask()

/// A server with the test scheme and the owner policy, and a proxy for each user, by name, or anonymous.
let serve () = task {
    let builder = WebApplication.CreateBuilder()
    builder.WebHost.UseTestServer() |> ignore
    builder.Logging.ClearProviders() |> ignore

    builder.Services
        .AddAuthentication("Test")
        .AddScheme<AuthenticationSchemeOptions, TestAuthHandler>("Test", ignore)
    |> ignore

    builder.Services.AddAuthorization(fun o -> o.AddPolicy("owner", fun p -> p.AddRequirements(OwnerRequirement()) |> ignore))
    |> ignore

    builder.Services.AddSingleton<IAuthorizationHandler, OwnerHandler>() |> ignore
    let app = builder.Build()
    app.UseAuthentication().UseAuthorization() |> ignore
    let docs = ConcurrentDictionary<int, Doc>()
    docs[1] <- { Id = 1; Owner = "ada"; Title = "draft" }

    app.MapRemote<IDocsApi>(
        "/api",
        [ JsonFormat.Instance ],
        fun ctx ->
            DocsApi {
                User = ctx.User
                Authorization = ctx.RequestServices.GetRequiredService<IAuthorizationService>()
                Docs = docs
            }
    )
    |> ignore

    do! app.StartAsync()

    let proxy (user: string option) =
        let http = app.GetTestClient()
        user |> Option.iter (fun u -> http.DefaultRequestHeaders.Add("X-User", u))
        DocsApiProxy.create (RemoteClient("http://localhost", "/api", JsonFormat.Instance, HttpClientTransport(http)))

    return app, proxy
}

[<Fact(Timeout = 10_000)>]
let ``a member without attributes is called without a user`` () = task {
    let! app, proxy = serve ()
    use _ = app
    let! pong = (proxy None).Ping()
    Assert.Equal("pong", pong)
}

[<Fact(Timeout = 10_000)>]
let ``a member with [<Authorize>] is answered 401 without a user, which the client sees as the status`` () = task {
    let! app, proxy = serve ()
    use _ = app
    let! e = Assert.ThrowsAsync<RemoteCallException>(fun () -> (proxy None).WhoAmI() :> Task)
    Assert.Equal(Some 401, e.StatusCode)
    let! name = (proxy (Some "ada")).WhoAmI()
    Assert.Equal("ada", name)
}

[<Fact(Timeout = 10_000)>]
let ``a member's role applies to it alone, answered 403 to a user without the role`` () = task {
    let! app, proxy = serve ()
    use _ = app
    let! e = Assert.ThrowsAsync<RemoteCallException>(fun () -> (proxy (Some "bo;reader")).Publish 1 :> Task)
    Assert.Equal(Some 403, e.StatusCode)
    let! published = (proxy (Some "ada;editor")).Publish 1
    let! name = (proxy (Some "bo;reader")).WhoAmI()
    Assert.Equal(("published 1", "bo"), (published, name))
}

[<Fact(Timeout = 10_000)>]
let ``a resource-based policy in an effect answers with the contract's error`` () = task {
    let! app, proxy = serve ()
    use _ = app
    let! byOwner = (proxy (Some "ada")).Rename(1, "final")
    let! byOther = (proxy (Some "bo")).Rename(1, "mine")
    let! missing = (proxy (Some "bo")).Rename(2, "mine")
    Assert.Equal<Result<Doc, DocError>>(Ok { Id = 1; Owner = "ada"; Title = "final" }, byOwner)
    Assert.Equal<Result<Doc, DocError>>(Error Forbidden, byOther)
    Assert.Equal<Result<Doc, DocError>>(Error(NotFound 2), missing)
}
//------------------------------------------------------------------------------
//        This code was generated by myriad.
//        Changes to this file will be lost when the code is regenerated.
//------------------------------------------------------------------------------



module DocsApiProxy =
    let create (client: Orsak.Remoting.RemoteClient) : IDocsApi =
        let ping = client.Member("IDocsApi", "Ping", [||], typeof<string>)
        let whoAmI = client.Member("IDocsApi", "WhoAmI", [||], typeof<string>)

        let publish =
            client.Member("IDocsApi", "Publish", [| typeof<int> |], typeof<string>)

        let rename =
            client.Member("IDocsApi", "Rename", [| typeof<int>; typeof<string> |], typeof<Result<Doc, DocError>>)

        { new IDocsApi with
            member _.Ping() = ping.Task [||]
            member _.WhoAmI() = whoAmI.Task [||]
            member _.Publish a = publish.Task [| box a |]
            member _.Rename(a, b) = rename.Task [| box a; box b |]
        }
