/// Reads a server's metrics from outside its process, over EventPipe, the way dotnet-counters does: the
/// runtime's System.Runtime meter and ASP.NET Core's request duration, as published by the
/// System.Diagnostics.Metrics event source. The servers need no endpoint or package for it.
module LoadTest.Driver.Metrics

open System
open System.Collections.Generic
open System.Diagnostics.Tracing
open System.Globalization
open System.Threading.Tasks
open Microsoft.Diagnostics.NETCore.Client
open Microsoft.Diagnostics.Tracing

/// The instruments read, as `Meter\instrument`.
let instruments = [
    @"System.Runtime\dotnet.process.cpu.time"
    @"System.Runtime\dotnet.gc.heap.total_allocated"
    @"System.Runtime\dotnet.gc.collections"
    @"System.Runtime\dotnet.gc.pause.time"
    @"Microsoft.AspNetCore.Hosting\http.server.request.duration"
]

/// One published value of one time series: for a counter, its increase over the interval ending at `time`;
/// for a histogram, the number of values recorded in the interval and their quantiles.
type Sample = {
    time: DateTime
    instrument: string
    rate: float
    count: int
    quantiles: Map<float, float>
}

let private parseFloat (s: string) = Double.Parse(s, CultureInfo.InvariantCulture)

/// "0.5=0.0012;0.95=0.0031;0.99=0.0050"
let private parseQuantiles (s: string) =
    s.Split(';', StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun q ->
        let parts = q.Split('=')
        parseFloat parts[0], parseFloat parts[1])
    |> Map.ofArray

/// An EventPipe session on one process for as long as it is alive, keeping every sample it receives.
type MetricsSession(name: string, pid: int, refreshInterval: TimeSpan) =
    let sessionId = Guid.NewGuid().ToString()
    let samples = List<Sample>()

    let provider =
        EventPipeProvider(
            "System.Diagnostics.Metrics",
            EventLevel.Informational,
            0x3L, // Messages | TimeSeriesValues
            dict [
                "SessionId", sessionId
                "Metrics", String.Join(",", instruments)
                // parsed with the server's culture, which is this machine's too
                "RefreshInterval", refreshInterval.TotalSeconds.ToString(CultureInfo.CurrentCulture)
                "MaxTimeSeries", "1000"
                "MaxHistograms", "100"
            ]
        )

    let session = DiagnosticsClient(pid).StartEventPipeSession([ provider ], false)
    let source = new EventPipeEventSource(session.EventStream)

    let add (sample: Sample) = lock samples (fun () -> samples.Add sample)

    do
        source.Dynamic.add_All(fun e ->
            if e.ProviderName = "System.Diagnostics.Metrics" && string (e.PayloadByName "sessionId") = sessionId then
                match e.EventName with
                | "CounterRateValuePublished" ->
                    // the first value of an observable counter has no rate yet
                    match string (e.PayloadByName "rate") with
                    | "" -> ()
                    | rate ->
                        add {
                            time = e.TimeStamp
                            instrument = string (e.PayloadByName "instrumentName")
                            rate = parseFloat rate
                            count = 0
                            quantiles = Map.empty
                        }
                | "HistogramValuePublished" ->
                    add {
                        time = e.TimeStamp
                        instrument = string (e.PayloadByName "instrumentName")
                        rate = 0.
                        count = e.PayloadByName "count" :?> int
                        quantiles = parseQuantiles (string (e.PayloadByName "quantiles"))
                    }
                | "Error"
                | "ObservableInstrumentCallbackError"
                | "MultipleSessionsNotSupportedError"
                | "TimeSeriesLimitReached"
                | "HistogramLimitReached" -> eprintfn $"  !! %s{name} metrics: %s{e.EventName} %s{e.ToString()}"
                | _ -> ())

    let processing = Task.Run(fun () -> source.Process() |> ignore)

    member _.RefreshInterval = refreshInterval

    /// Every sample received so far.
    member _.All = lock samples (fun () -> List.ofSeq samples)

    /// The samples of the intervals entirely between `from` and `until`.
    member _.Between(from: DateTime, until: DateTime) =
        lock samples (fun () ->
            samples
            |> Seq.filter (fun s -> s.time - refreshInterval >= from && s.time <= until)
            |> List.ofSeq)

    /// The time of the latest sample received, to know when the samples of a period have all arrived.
    member _.Latest = lock samples (fun () -> if samples.Count = 0 then DateTime.MinValue else samples[samples.Count - 1].time)

    interface IDisposable with
        member _.Dispose() =
            try
                session.Stop()
            with _ ->
                () // the server may be gone already

            processing.Wait(TimeSpan.FromSeconds 5.) |> ignore
            source.Dispose()
            session.Dispose()

/// A server's work over a measured phase, per request it handled in that time.
type ServerMetrics = {
    /// Requests the server handled in the intervals read.
    requests: int
    /// CPU time per request, in microseconds.
    cpuUs: float
    /// Bytes allocated per request.
    allocated: float
    /// Garbage collections, of any generation, per 10 000 requests.
    gcs: float
    /// Share of the time the server was paused for garbage collection.
    gcPause: float
    /// Request duration inside the server, in milliseconds: the median over the intervals read.
    p50: float
    p99: float
}

let private median (values: float seq) =
    let sorted = Seq.sort values |> Array.ofSeq

    if sorted.Length = 0 then nan
    elif sorted.Length % 2 = 1 then sorted[sorted.Length / 2]
    else (sorted[sorted.Length / 2 - 1] + sorted[sorted.Length / 2]) / 2.

let summarize (refreshInterval: TimeSpan) (samples: Sample list) =
    let of' name = samples |> List.filter (fun s -> s.instrument = name)
    let total name = of' name |> List.sumBy _.rate
    let durations = of' "http.server.request.duration" |> List.filter (fun s -> s.count > 0)
    let requests = durations |> List.sumBy _.count
    let perRequest value = value / float (max requests 1)
    // one time series, so one sample per interval
    let seconds = float (List.length (of' "dotnet.gc.heap.total_allocated")) * refreshInterval.TotalSeconds
    let quantile q = median [ for s in durations -> s.quantiles[q] * 1000. ]

    {
        requests = requests
        cpuUs = perRequest (total "dotnet.process.cpu.time" * 1e6)
        // no allocations while handling requests means the counter didn't advance, not that none were made
        allocated =
            match total "dotnet.gc.heap.total_allocated" with
            | 0. when requests > 0 -> nan
            | allocated -> perRequest allocated
        gcs = perRequest (total "dotnet.gc.collections" * 10_000.)
        gcPause = if seconds > 0. then total "dotnet.gc.pause.time" / seconds else nan
        p50 = quantile 0.5
        p99 = quantile 0.99
    }
