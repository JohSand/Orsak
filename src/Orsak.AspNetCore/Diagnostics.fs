namespace Orsak.AspNetCore

open System.Collections.Generic
open System.Diagnostics
open System.Diagnostics.Metrics
open System.Threading.Tasks
open Microsoft.AspNetCore.Http

/// <summary>
/// OpenTelemetry-compatible diagnostics for effect endpoints. <c>MapEffectEndpoints</c> instruments every endpoint with
/// an activity (span) per request, and a histogram of handler durations and a counter of running handlers. They are
/// only recorded when something listens, e.g. OpenTelemetry configured with <c>SourceName</c> and <c>MeterName</c>.
/// </summary>
/// <example>
/// <code lang="fsharp">
/// builder.Services
///     .AddOpenTelemetry()
///     .WithTracing(fun t -> t.AddSource(EffectDiagnostics.SourceName) |> ignore)
///     .WithMetrics(fun m -> m.AddMeter(EffectDiagnostics.MeterName) |> ignore)
/// |> ignore
/// </code>
/// </example>
[<RequireQualifiedAccess>]
module EffectDiagnostics =

    /// <summary>
    /// The name of the <see cref="T:System.Diagnostics.ActivitySource"/> of the endpoints' activities, named
    /// <c>effect.execute</c>, with the tags <c>orsak.route</c>, <c>http.request.method</c> and
    /// <c>http.response.status_code</c>.
    /// </summary>
    [<Literal>]
    let SourceName = "Orsak.AspNetCore"

    /// <summary>
    /// The name of the <see cref="T:System.Diagnostics.Metrics.Meter"/> of the endpoints' metrics: the histogram
    /// <c>orsak.effect.duration</c>, in milliseconds, and, on .NET 7 and later, the counter <c>orsak.effect.active</c>.
    /// </summary>
    [<Literal>]
    let MeterName = "Orsak.AspNetCore"

    let private source = new ActivitySource(SourceName)
    let private meter = new Meter(MeterName)

    let private effectDuration =
        meter.CreateHistogram<float>("orsak.effect.duration", "ms", "Duration of effect handler execution")

#if NET7_0_OR_GREATER
    let private effectActive =
        meter.CreateUpDownCounter<int>("orsak.effect.active", "{effects}", "Concurrently executing effect handlers")
#endif

    let inline private elapsedMs (startTimestamp: int64) =
        let elapsed = Stopwatch.GetTimestamp() - startTimestamp
        float elapsed * 1000.0 / float Stopwatch.Frequency

    let private finish (start: int64) (route: string) (verb: string) (activity: Activity) (statusCode: int) =
        let ms = elapsedMs start
#if NET7_0_OR_GREATER
        effectActive.Add(-1)
#endif

        effectDuration.Record(
            ms,
            KeyValuePair<string, obj>("orsak.route", route),
            KeyValuePair<string, obj>("http.request.method", verb),
            KeyValuePair<string, obj>("http.response.status_code", statusCode))

        if not (isNull activity) then
            activity.SetTag("http.response.status_code", statusCode) |> ignore

            if statusCode >= 500 then
                activity.SetStatus(ActivityStatusCode.Error) |> ignore

            activity.Dispose()

    /// <summary>
    /// Wraps a request delegate with the endpoint diagnostics. <c>MapEffectEndpoints</c> does this for every endpoint;
    /// use it for request delegates mapped some other way.
    /// </summary>
    /// <param name="route">The route template, recorded as <c>orsak.route</c></param>
    /// <param name="verb">The HTTP method, recorded as <c>http.request.method</c></param>
    /// <param name="inner">The request delegate to instrument</param>
    let instrument (route: string) (verb: string) (inner: RequestDelegate) =
        RequestDelegate(fun ctx ->
            let activity = source.StartActivity("effect.execute", ActivityKind.Internal)

            if not (isNull activity) then
                activity.SetTag("orsak.route", route) |> ignore
                activity.SetTag("http.request.method", verb) |> ignore

#if NET7_0_OR_GREATER
            effectActive.Add(1)
#endif
            let start = Stopwatch.GetTimestamp()
            let innerTask = inner.Invoke(ctx)

            if innerTask.IsCompletedSuccessfully then
                finish start route verb activity ctx.Response.StatusCode
                innerTask
            else
                task {
                    try
                        do! innerTask
                    finally
                        finish start route verb activity ctx.Response.StatusCode
                }
                :> Task)
