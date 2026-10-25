module BaseEffectsTests

open System
open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Time.Testing
open Orsak
open Xunit
open Swensen.Unquote

[<Fact>]
let ``CancellationProvider fromSource provides the source it is given`` () =
    use source = new CancellationTokenSource()
    let provider = CancellationProvider.fromSource source
    test <@ obj.ReferenceEquals(provider.Source, source) @>

[<Fact>]
let ``CancellationProvider create provides the same source on every read`` () =
    use provider = CancellationProvider.create ()
    let p = provider :> ICancellationProvider
    test <@ obj.ReferenceEquals(p.Source, p.Source) @>

[<Fact>]
let ``CancellationSource cancel cancels the token getToken returned`` () = task {
    use provider = CancellationProvider.create ()
    let! token = CancellationSource.getToken () |> Effect.runOrFail provider
    do! CancellationSource.cancel () |> Effect.runOrFail provider
    true =! token.IsCancellationRequested
}

type TimeEnv(clock: TimeProvider) =
    interface ITimeProvider with
        member _.Clock = clock

type CancellableTimeEnv(clock: TimeProvider, source: CancellationTokenSource) =
    interface ITimeProvider with
        member _.Clock = clock

    interface ICancellationProvider with
        member _.Source = source

[<Fact>]
let ``Time delay ends when the clock has advanced by the delay`` () = task {
    let clock = FakeTimeProvider()
    let run = Time.delay (TimeSpan.FromMinutes 5.0) |> Effect.runOrFail (TimeEnv clock)

    clock.Advance(TimeSpan.FromMinutes 4.0)
    test <@ not run.IsCompleted @>

    clock.Advance(TimeSpan.FromMinutes 1.0)
    do! run
}

[<Fact>]
let ``Time delay ends early when the environment's source is cancelled`` () = task {
    let clock = FakeTimeProvider()
    use source = new CancellationTokenSource()
    let run = Time.delay (TimeSpan.FromMinutes 5.0) |> Effect.runOrFail (CancellableTimeEnv(clock, source))

    source.Cancel()
    let! _ = Assert.ThrowsAnyAsync<OperationCanceledException>(fun () -> run :> Task)
    ()
}

type RecordingLogger() =
    let entries = Collections.Generic.List<LogLevel * string * exn>()
    member _.Entries = List.ofSeq entries

    interface ILogger with
        member _.BeginScope _ = null
        member _.IsEnabled _ = true

        member _.Log(level, _, state, ex, formatter) =
            entries.Add(level, formatter.Invoke(state, ex), ex)

type LogEnv(logger: ILogger) =
    interface Orsak.ILoggerProvider with
        member _.Effect = logger

[<Fact>]
let ``Log writes templated messages at their level`` () = task {
    let logger = RecordingLogger()

    let work: Effect<LogEnv, unit, string> = eff {
        do! Log.Information("Imported {Count} rows", 3)
        do! Log.Warning("{Name} is slow", "import")
        do! Log.Debug "done"
    }

    do! work |> Effect.runOrFail (LogEnv logger)

    test
        <@
            logger.Entries = [
                LogLevel.Information, "Imported 3 rows", null
                LogLevel.Warning, "import is slow", null
                LogLevel.Debug, "done", null
            ]
        @>
}

[<Fact>]
let ``Log Error writes the exception`` () = task {
    let logger = RecordingLogger()
    let ex = InvalidOperationException "boom"

    do!
        Log.Error(ex, "Import {Id} failed", 7)
        |> Effect.runOrFail (LogEnv logger)

    test <@ logger.Entries = [ LogLevel.Error, "Import 7 failed", (ex :> exn) ] @>
}
