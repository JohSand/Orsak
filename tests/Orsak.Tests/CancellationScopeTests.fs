module rec Orsak.Tests.CancellationScopeTests

open System
open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.Time.Testing
open Orsak
open Orsak.Myriad
open Orsak.Scoped
open Xunit
open Swensen.Unquote

// the root environment begins cancellation scopes, and grows into the environment in one
[<GenEnvironment>]
type IAppEnvironment =
    inherit ITimeProvider
    inherit IGrowsInto<CancellationScope, ICancellableEnvironment>

[<GenEnvironment>]
type ICancellableEnvironment =
    inherit IAppEnvironment
    inherit ICancellationProvider

let root (clock: TimeProvider) (token: CancellationToken) =
    AppEnvironment.create {| TimeProvider = clock; CancellationScopeScope = CancellationScope.linkedTo token |}

let source () =
    Effect.Create(fun (provider: #ICancellationProvider) -> provider.Source)

[<Fact>]
let ``CancellationScope LinkedTo an already cancelled token is cancelled`` () =
    use scope = CancellationScope.LinkedTo(CancellationToken(true))
    true =! scope.IsCancellationRequested

[<Fact>]
let ``CancellationScope is cancelled with the token it is linked to, and not the other way round`` () =
    use parent = new CancellationTokenSource()
    use scope = CancellationScope.LinkedTo parent.Token
    use other = new CancellationTokenSource()
    use unaffected = CancellationScope.LinkedTo other.Token

    parent.Cancel()
    unaffected.Cancel()

    true =! scope.IsCancellationRequested
    false =! other.IsCancellationRequested

[<Fact>]
let ``a disposed CancellationScope is no longer cancelled by its token`` () =
    use parent = new CancellationTokenSource()
    let scope = CancellationScope.LinkedTo parent.Token
    scope.Dispose()
    // the registration is gone: cancelling would otherwise throw, cancelling the disposed source
    parent.Cancel()

[<Fact>]
let ``Cancellation within provides a scope, disposed when the effect ends`` () = task {
    use parent = new CancellationTokenSource()
    let! scope = source () |> Cancellation.within |> Effect.runOrFail (root TimeProvider.System parent.Token)

    test <@ scope :? CancellationScope @>
    raises<ObjectDisposedException> <@ scope.Token @>
}

[<Fact>]
let ``cancelling a scope from within doesn't cancel the token it is linked to`` () = task {
    use parent = new CancellationTokenSource()

    let work = eff {
        do! CancellationSource.cancel ()
        let! token = CancellationSource.getToken ()
        return token.IsCancellationRequested
    }

    let! cancelled = work |> Cancellation.within |> Effect.runOrFail (root TimeProvider.System parent.Token)

    true =! cancelled
    false =! parent.IsCancellationRequested
}

[<Fact>]
let ``cancelling the root token stops the effect in a scope`` () = task {
    let clock = FakeTimeProvider()
    use parent = new CancellationTokenSource()

    let run =
        Time.delay (TimeSpan.FromMinutes 5.0)
        |> Cancellation.within
        |> Effect.runOrFail (root clock parent.Token)

    parent.Cancel()
    let! _ = Assert.ThrowsAnyAsync<OperationCanceledException>(fun () -> run :> Task)
    ()
}

[<Fact>]
let ``a scope begun in a scope is cancelled with it`` () = task {
    use parent = new CancellationTokenSource()

    let nested = eff {
        let! outer = source ()

        return!
            eff {
                // cancels the outer scope, from the inner one
                outer.Cancel()
                let! inner = source ()
                return inner.IsCancellationRequested
            }
            |> Cancellation.within
    }

    let! innerCancelled = nested |> Cancellation.within |> Effect.runOrFail (root TimeProvider.System parent.Token)

    true =! innerCancelled
    false =! parent.IsCancellationRequested
}

/// Cancels the scope it is nested in, from a scope of its own.
let cancelOuterScope (outer: CancellationTokenSource) = cancellableEff {
    outer.Cancel()
    let! inner = source ()
    return inner.IsCancellationRequested
}

let nestedCancellableEffs () = cancellableEff {
    let! outer = source ()
    return! cancelOuterScope outer
}

[<Fact>]
let ``a cancellableEff begun in a cancellableEff is cancelled with it`` () = task {
    use parent = new CancellationTokenSource()
    let! innerCancelled = nestedCancellableEffs () |> Effect.runOrFail (root TimeProvider.System parent.Token)

    true =! innerCancelled
    false =! parent.IsCancellationRequested
}

[<Fact>]
let ``Cancellation timeout fails with its error, and stops the effect, when it times out`` () = task {
    let clock = FakeTimeProvider()

    let run =
        Time.delay (TimeSpan.FromMinutes 10.0)
        |> Cancellation.timeout (TimeSpan.FromMinutes 1.0) "timed out"
        |> Effect.run (root clock CancellationToken.None)

    clock.Advance(TimeSpan.FromSeconds 59.0)
    false =! run.IsCompleted

    clock.Advance(TimeSpan.FromSeconds 1.0)
    let! result = run
    result =! Error "timed out"
}

[<Fact>]
let ``Cancellation timeout returns the result of an effect that ends in time`` () = task {
    let clock = FakeTimeProvider()

    let work = eff {
        do! Time.delay (TimeSpan.FromSeconds 30.0)
        return 42
    }

    let run =
        work
        |> Cancellation.timeout (TimeSpan.FromMinutes 1.0) "timed out"
        |> Effect.run (root clock CancellationToken.None)

    clock.Advance(TimeSpan.FromSeconds 30.0)
    let! result = run
    result =! Ok 42
}

[<Fact>]
let ``Cancellation timeout lets a cancellation of the root token propagate`` () = task {
    let clock = FakeTimeProvider()
    use parent = new CancellationTokenSource()

    let run =
        Time.delay (TimeSpan.FromMinutes 10.0)
        |> Cancellation.timeout (TimeSpan.FromMinutes 1.0) "timed out"
        |> Effect.run (root clock parent.Token)

    parent.Cancel()
    let! _ = Assert.ThrowsAnyAsync<OperationCanceledException>(fun () -> run.AsTask() :> Task)
    ()
}
//------------------------------------------------------------------------------
//        This code was generated by myriad.
//        Changes to this file will be lost when the code is regenerated.
//------------------------------------------------------------------------------


module CancellableEnvironment =
    let extend (outer: IAppEnvironment) (effect: System.Threading.CancellationTokenSource) : ICancellableEnvironment =
        { new ICancellableEnvironment

          interface ITimeProvider with
              member _.Clock = (outer :> ITimeProvider).Clock
          interface ICancellationProvider with
              member _.Source = effect
          interface ScopeProvider<CancellationScope> with
              member _.BeginScope() = CancellationScope.NestedIn(effect)
          interface IGrowsInto<CancellationScope, ICancellableEnvironment> with
              member _.Grow scope =
                  (outer :> IGrowsInto<CancellationScope, ICancellableEnvironment>).Grow scope
        }

    let create
        (effects:
            {| TimeProvider: System.TimeProvider; CancellationTokenSource: System.Threading.CancellationTokenSource |})
        : ICancellableEnvironment =
        { new ICancellableEnvironment

          interface ITimeProvider with
              member _.Clock = effects.TimeProvider
          interface ICancellationProvider with
              member _.Source = effects.CancellationTokenSource
          interface ScopeProvider<CancellationScope> with
              member _.BeginScope() =
                  CancellationScope.NestedIn(effects.CancellationTokenSource)
          interface IGrowsInto<CancellationScope, ICancellableEnvironment> with
              member this.Grow scope =
                  CancellableEnvironment.extend (this :?> IAppEnvironment) scope
        }

    let run effects = Effect.run (create effects)
    let runOrFail effects = Effect.runOrFail (create effects)

module AppEnvironment =
    let create
        (effects: {| TimeProvider: System.TimeProvider; CancellationScopeScope: ScopeProvider<CancellationScope> |})
        : IAppEnvironment =
        { new IAppEnvironment

          interface ITimeProvider with
              member _.Clock = effects.TimeProvider
          interface ScopeProvider<CancellationScope> with
              member _.BeginScope() =
                  effects.CancellationScopeScope.BeginScope()
          interface IGrowsInto<CancellationScope, ICancellableEnvironment> with
              member this.Grow scope =
                  CancellableEnvironment.extend (this :?> IAppEnvironment) scope
        }

    let run effects = Effect.run (create effects)
    let runOrFail effects = Effect.runOrFail (create effects)

[<AutoOpen>]
module AppEnvironmentExtensions =
    type IAppEnvironment with
        member this.Extend(effect: System.Threading.CancellationTokenSource) =
            CancellableEnvironment.extend this effect
