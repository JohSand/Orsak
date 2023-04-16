(**
---
title: Http
category: Examples
categoryindex: 2
index: 2
---
*)

(**

###Manual

While also achievable with effects directly, using `DelegatingHandler`s is easy and allows making use of the wider ecosystem.
*)
open System
open System.Net.Http
//make sure your .NET version has sensible defaults for these:

let inner =
    new SocketsHttpHandler(
        PooledConnectionIdleTimeout = TimeSpan.FromMinutes(1),
        PooledConnectionLifetime = TimeSpan.FromMinutes(1)
    )

let clientFactory f handlers =
    let client = new HttpClient(handlers inner, false)
    f client

(**
###IHttpClientFactory
*)
