namespace Orsak.AspNetCore

open System.Collections.Generic
open System.Diagnostics
open System.Diagnostics.Metrics
open System.Threading.Tasks
open Microsoft.AspNetCore.Http

[<RequireQualifiedAccess>]
module EffectDiagnostics =

    [<Literal>]
    let SourceName = "Orsak.AspNetCore"

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
