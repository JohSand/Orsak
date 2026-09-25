/// Drives HTTP load against LoadTest.Baseline (minimal APIs) and LoadTest.Orsak (Orsak.AspNetCore routing),
/// and prints requests/s and the servers' own metrics per request for each scenario side by side.
///
///     dotnet build benchmarks/benchmarks.sln -c Release
///     dotnet run -c Release --project benchmarks/LoadTest.Driver -- [options]
///
/// Options:
///     --rounds <n>          rounds over all scenarios (default 3)
///     --duration <s>        measured seconds per scenario and server, each round (default 5)
///     --warmup <s>          warm-up seconds before each measurement, not measured (default 2)
///     --connections <n>     concurrent connections (default 64)
///     --only <text>         only scenarios whose name contains <text>
///     --dump-metrics <file> write every metrics sample, and the measured periods, to <file> as CSV
///     --server-cpus <mask>  pin the server processes to these logical CPUs, as a hex mask, e.g. FF
///     --client-cpus <mask>  pin this process to these logical CPUs, e.g. FF00
///
/// Client and servers share the machine, so pin them to separate cores. On a CPU with performance and
/// efficiency cores, pin both to performance cores, or results vary by up to 2x between runs: on an
/// i7-12700, logical CPUs 0-15 are the performance cores, so `--server-cpus FF --client-cpus FF00`.
///
/// Both servers run for the whole run, on the same CPUs, and are measured alternately: every scenario
/// once per server in each round, in the opposite order the next round, so that drift over the run
/// (heat, turbo, background work) doesn't all land on one server. Before the first round, each server is
/// warmed up on every scenario. Each column is the median over the rounds; the ratio columns are the
/// median of each round's ratio, with the lowest and highest.
///
/// The servers' metrics are read over EventPipe (see Metrics.fs): CPU time, allocations and garbage
/// collections from the runtime's System.Runtime meter, and request duration from ASP.NET Core, divided by
/// the number of requests the server itself counted in the same intervals.
///
/// Requests/s is limited by whichever side saturates first, and client and server share the machine, so
/// it can be limited by this driver rather than by the server. Server CPU time per request doesn't depend
/// on that, which makes it the steadier comparison. The driver warns when its own CPUs were busy.
module LoadTest.Driver.Program

open System
open System.Diagnostics
open System.IO
open System.Net.Http
open System.Numerics
open System.Threading
open System.Threading.Tasks
open LoadTest.Driver.Metrics

/// One request type, against the equivalent endpoint of each server.
type Scenario = { name: string; baseline: string; orsak: string }

let scenarios = [
    { name = "ping"; baseline = "/ping"; orsak = "/ping" }
    { name = "ping, unit handler (RouteGet2)"; baseline = "/ping"; orsak = "/ping2" }
    { name = "async, completed task"; baseline = "/async/completed"; orsak = "/async/completed" }
    { name = "async, yields"; baseline = "/async/yield"; orsak = "/async/yield" }
    { name = "ping/{id}"; baseline = "/ping/42"; orsak = "/ping/42" }
    { name = "sum/{a}/{b}, tupled (RouteGet)"; baseline = "/sum/1/2"; orsak = "/sum/1/2" }
    { name = "sum/{a}/{b}, curried (RouteGet2)"; baseline = "/sum/1/2"; orsak = "/sum2/1/2" }
    { name = "json"; baseline = "/json"; orsak = "/json" }
    { name = "time (dependency)"; baseline = "/time"; orsak = "/time" }
    { name = "guid (dependency)"; baseline = "/guid"; orsak = "/guid" }
    { name = "combined (two dependencies)"; baseline = "/combined"; orsak = "/combined" }
]

type Options = {
    rounds: int
    duration: TimeSpan
    warmup: TimeSpan
    connections: int
    only: string option
    dumpMetrics: string option
    serverCpus: nativeint option
    clientCpus: nativeint option
}

let parseOptions (argv: string array) =
    let rec parse (options: Options) args =
        match args with
        | [] -> options
        | "--rounds" :: n :: rest -> parse { options with rounds = int n } rest
        | "--duration" :: s :: rest -> parse { options with duration = TimeSpan.FromSeconds(float s) } rest
        | "--warmup" :: s :: rest -> parse { options with warmup = TimeSpan.FromSeconds(float s) } rest
        | "--connections" :: n :: rest -> parse { options with connections = int n } rest
        | "--only" :: text :: rest -> parse { options with only = Some text } rest
        | "--dump-metrics" :: file :: rest -> parse { options with dumpMetrics = Some file } rest
        | "--server-cpus" :: mask :: rest -> parse { options with serverCpus = Some(nativeint (Convert.ToInt64(mask, 16))) } rest
        | "--client-cpus" :: mask :: rest -> parse { options with clientCpus = Some(nativeint (Convert.ToInt64(mask, 16))) } rest
        | unknown :: _ -> failwith $"Unknown option '%s{unknown}'. See the top of LoadTest.Driver/Program.fs."

    parse
        {
            rounds = 3
            duration = TimeSpan.FromSeconds 5.
            warmup = TimeSpan.FromSeconds 2.
            connections = 64
            only = None
            dumpMetrics = None
            serverCpus = None
            clientCpus = None
        }
        (List.ofArray argv)

/// The benchmarks folder, found from the driver's own output folder.
let benchmarksDirectory () =
    let rec up (dir: DirectoryInfo) =
        if isNull dir then
            failwith "Could not find the benchmarks folder above the driver."
        elif Directory.Exists(Path.Combine(dir.FullName, "LoadTest.Baseline")) then
            dir.FullName
        else
            up dir.Parent

    up (DirectoryInfo AppContext.BaseDirectory)

type Measurement = {
    requests: int
    failures: int
    seconds: float
    latenciesMs: float array
    /// When the measured load started and ended, to find the server's metrics of that time.
    started: DateTime
    ended: DateTime
    /// This driver's CPU time over the measurement, as a share of the CPUs it may run on.
    clientLoad: float
}

let percentile (sorted: float array) (p: float) =
    if sorted.Length = 0 then nan else sorted[min (sorted.Length - 1) (int (float sorted.Length * p))]

/// The median of the values there are: NaN stands for a round without one.
let median (values: float seq) =
    let sorted = values |> Seq.filter (Double.IsNaN >> not) |> Seq.sort |> Array.ofSeq
    if sorted.Length = 0 then nan
    elif sorted.Length % 2 = 1 then sorted[sorted.Length / 2]
    else (sorted[sorted.Length / 2 - 1] + sorted[sorted.Length / 2]) / 2.

/// Sends requests back to back on each connection until the duration is up.
let load (client: HttpClient) (uri: Uri) (connections: int) (duration: TimeSpan) = task {
    let deadline = Stopwatch.GetTimestamp() + int64 (duration.TotalSeconds * float Stopwatch.Frequency)
    let started = Stopwatch.GetTimestamp()

    let worker () = task {
        let latencies = ResizeArray<float>(100_000)
        let mutable failures = 0

        while Stopwatch.GetTimestamp() < deadline do
            let t0 = Stopwatch.GetTimestamp()

            try
                use! response = client.GetAsync(uri)
                let! _ = response.Content.ReadAsByteArrayAsync()

                if response.IsSuccessStatusCode then
                    latencies.Add(float (Stopwatch.GetTimestamp() - t0) * 1000.0 / float Stopwatch.Frequency)
                else
                    failures <- failures + 1
            with _ ->
                failures <- failures + 1

        return latencies, failures
    }

    let! results = Task.WhenAll [ for _ in 1..connections -> Task.Run<ResizeArray<float> * int>(fun () -> worker ()) ]
    let seconds = float (Stopwatch.GetTimestamp() - started) / float Stopwatch.Frequency
    let latencies = results |> Array.collect (fun (l, _) -> l.ToArray()) |> Array.sort
    return latencies, results |> Array.sumBy snd, seconds
}

/// A server process on its own port, with its output in a log file.
type Server(name: string, dll: string, port: int, cpus: nativeint option) =
    let log = Path.Combine(Path.GetTempPath(), $"%s{name}.log")
    let url = $"http://127.0.0.1:%i{port}"

    let proc =
        let info =
            ProcessStartInfo(
                "dotnet",
                $"\"%s{dll}\" --urls %s{url} --Logging:LogLevel:Default=Warning",
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false
            )

        // Server GC without DATAS, its dynamic number of heaps: DATAS keeps adapting the heap count during
        // a run, which changes how often the GC runs from one round to the next, and while it does, the
        // runtime's dotnet.gc.heap.total_allocated has been seen to stop increasing for up to a minute
        info.Environment["DOTNET_GCDynamicAdaptationMode"] <- "0"

        let p = Process.Start info
        let writer = new StreamWriter(new FileStream(log, FileMode.Create, FileAccess.Write, FileShare.ReadWrite), AutoFlush = true)
        p.OutputDataReceived.Add(fun e -> if not (isNull e.Data) then lock writer (fun () -> writer.WriteLine e.Data))
        p.ErrorDataReceived.Add(fun e -> if not (isNull e.Data) then lock writer (fun () -> writer.WriteLine e.Data))
        p.BeginOutputReadLine()
        p.BeginErrorReadLine()
        cpus |> Option.iter (fun mask -> p.ProcessorAffinity <- mask)
        p

    member _.Name = name
    member _.Uri(path: string) = Uri(url + path)
    member _.Log = log

    /// The log so far, while the server still has it open for writing.
    member _.ReadLog() =
        use reader = new StreamReader(new FileStream(log, FileMode.Open, FileAccess.Read, FileShare.ReadWrite))
        reader.ReadToEnd().Split(Environment.NewLine)

    member _.Pid = proc.Id

    member _.WaitUntilReady(client: HttpClient) = task {
        let deadline = DateTime.UtcNow.AddSeconds 30.
        let mutable ready = false

        while not ready do
            if proc.HasExited then
                failwith $"%s{name} exited with code %i{proc.ExitCode}; see %s{log}"

            if DateTime.UtcNow > deadline then
                failwith $"%s{name} did not answer on %s{url}/ping within 30 s; see %s{log}"

            try
                let! response = client.GetAsync(Uri(url + "/ping"))
                ready <- response.IsSuccessStatusCode
            with _ ->
                do! Task.Delay 200
    }

    interface IDisposable with
        member _.Dispose() =
            if not proc.HasExited then
                proc.Kill(entireProcessTree = true)
                proc.WaitForExit()

let startServer (options: Options) (name: string) (port: int) =
    let dll = Path.Combine(benchmarksDirectory (), name, "bin", "Release", "net10.0", name + ".dll")

    if not (File.Exists dll) then
        failwith $"%s{dll} not found. Build first: dotnet build benchmarks/benchmarks.sln -c Release"

    new Server(name, dll, port, options.serverCpus)

/// The number of CPUs this driver may run on.
let clientCpuCount (options: Options) =
    match options.clientCpus with
    | Some mask -> BitOperations.PopCount(uint64 mask)
    | None -> Environment.ProcessorCount

/// Warms up, then measures one scenario against one server.
let measure (options: Options) (client: HttpClient) (server: Server) (path: string) = task {
    let uri = server.Uri path
    let! _ = load client uri options.connections options.warmup
    let clientCpu = Process.GetCurrentProcess().TotalProcessorTime
    let started = DateTime.Now
    let! latencies, failures, seconds = load client uri options.connections options.duration
    let ended = DateTime.Now
    let clientCpu = Process.GetCurrentProcess().TotalProcessorTime - clientCpu

    if failures > 0 then
        eprintfn $"  !! %i{failures} failed requests; server log: %s{server.Log}"

        for line in server.ReadLog() |> Array.truncate 20 do
            eprintfn $"     %s{line}"

    return {
        requests = latencies.Length
        failures = failures
        seconds = seconds
        latenciesMs = latencies
        started = started
        ended = ended
        clientLoad = clientCpu.TotalSeconds / (seconds * float (clientCpuCount options))
    }
}

let rps (m: Measurement) = float m.requests / m.seconds

/// How often the servers publish their metrics. Each measurement reads the intervals entirely inside it.
let refreshInterval = TimeSpan.FromSeconds 0.25

/// A scenario's two table rows, one per server: medians over the rounds, and on Orsak's row each round's
/// ratio to Baseline in the same round.
let rows (scenario: Scenario) (rounds: ((Measurement * ServerMetrics) * (Measurement * ServerMetrics)) list) =
    let baseline, orsak = List.unzip rounds
    let med f ms = median (List.map f ms)
    let ratio f = [ for (b, o) in rounds -> f o / f b ]
    let range (ratios: float list) = $"%.2f{median ratios} (%.2f{List.min ratios}–%.2f{List.max ratios})"

    let row name (runs: (Measurement * ServerMetrics) list) (vsRps: string) (vsCpu: string) =
        String.Join(
            " | ",
            [
                (if name = "Baseline" then scenario.name else "")
                name
                $"%.0f{med (fst >> rps) runs}"
                vsRps
                $"%.1f{med (snd >> _.cpuUs) runs}"
                vsCpu
                $"%.0f{med (snd >> _.allocated) runs}"
                $"%.1f{med (snd >> _.gcs) runs}"
                $"%.1f{med (snd >> _.gcPause) runs * 100.}%%"
                $"%.3f{med (snd >> _.p50) runs} / %.3f{med (snd >> _.p99) runs}"
                $"%.2f{med (fun (m, _) -> percentile m.latenciesMs 0.5) runs}"
                $"%i{List.sumBy (fst >> _.failures) runs}"
            ]
        )
        |> sprintf "| %s |"

    [
        row "Baseline" baseline "" ""
        row "Orsak" orsak (range (ratio (fst >> rps))) (range (ratio (snd >> _.cpuUs)))
    ]

/// A warning when the servers or this driver aren't pinned to CPUs, which leaves them to the OS scheduler:
/// on a CPU with efficiency cores, runs landing on them have differed by 2x.
let unpinnedWarning (options: Options) =
    match
        [
            if options.serverCpus.IsNone then "the servers (--server-cpus)"
            if options.clientCpus.IsNone then "the driver (--client-cpus)"
        ]
    with
    | [] -> None
    | unpinned ->
        let unpinned = String.Join(" and ", unpinned)

        Some
            $"!! Not pinned to CPUs: %s{unpinned}. Results can vary by up to 2x between runs on a CPU with efficiency cores; pin to performance cores, e.g. --server-cpus FF --client-cpus FF00 on an i7-12700. See benchmarks/README.md."

[<EntryPoint>]
let main argv =
    let options = parseOptions argv
    options.clientCpus |> Option.iter (fun mask -> Process.GetCurrentProcess().ProcessorAffinity <- mask)
    unpinnedWarning options |> Option.iter (eprintfn "%s\n")

    let selected =
        match options.only with
        | Some text -> scenarios |> List.filter (fun s -> s.name.Contains(text, StringComparison.OrdinalIgnoreCase))
        | None -> scenarios

    eprintfn
        $"%i{selected.Length} scenarios, %i{options.rounds} rounds, %i{options.connections} connections, %.0f{options.warmup.TotalSeconds} s warm-up + %.0f{options.duration.TotalSeconds} s per scenario and server"

    use handler =
        new SocketsHttpHandler(MaxConnectionsPerServer = options.connections, PooledConnectionLifetime = Timeout.InfiniteTimeSpan)

    use client = new HttpClient(handler, Timeout = TimeSpan.FromSeconds 30.)
    use baseline = startServer options "LoadTest.Baseline" 5101
    use orsak = startServer options "LoadTest.Orsak" 5102
    Task.WaitAll(baseline.WaitUntilReady client, orsak.WaitUntilReady client)
    use baselineMetrics = new MetricsSession(baseline.Name, baseline.Pid, refreshInterval)
    use orsakMetrics = new MetricsSession(orsak.Name, orsak.Pid, refreshInterval)

    // warm each server up on every scenario before the first measurement, so that the first scenarios
    // aren't the ones measured while the servers' code is still being compiled and their pools grown
    for scenario in selected do
        for server, path in [ baseline, scenario.baseline; orsak, scenario.orsak ] do
            eprintfn $"  warm-up, %s{server.Name}: %s{scenario.name} (%s{path})"
            load client (server.Uri path) options.connections options.warmup |> _.Wait()

    let results =
        [
            for round in 1 .. options.rounds do
                for scenario in selected do
                    let measureOn (server: Server) path =
                        eprintfn $"  round %i{round}, %s{server.Name}: %s{scenario.name} (%s{path})"
                        (measure options client server path).Result

                    // alternate which server goes first, so neither is always measured right after the other
                    if round % 2 = 1 then
                        let b = measureOn baseline scenario.baseline
                        let o = measureOn orsak scenario.orsak
                        scenario, (b, o)
                    else
                        let o = measureOn orsak scenario.orsak
                        let b = measureOn baseline scenario.baseline
                        scenario, (b, o)
        ]

    // the metrics of the last measurement are published after it ends
    let last = results |> List.map (fun (_, (b, o)) -> max b.ended o.ended) |> List.max
    let deadline = DateTime.Now.AddSeconds 5.

    while (baselineMetrics.Latest < last || orsakMetrics.Latest < last) && DateTime.Now < deadline do
        Thread.Sleep 100

    options.dumpMetrics
    |> Option.iter (fun file ->
        File.WriteAllLines(
            file,
            [
                "kind,server,scenario,time,instrument,rate,count"
                for scenario, (b, o) in results do
                    for server, m in [ baseline.Name, b; orsak.Name, o ] do
                        $"start,%s{server},%s{scenario.name},{m.started:o},,,"
                        $"end,%s{server},%s{scenario.name},{m.ended:o},,,"
                for server, session in [ baseline.Name, baselineMetrics; orsak.Name, orsakMetrics ] do
                    for s in session.All do
                        let rate = s.rate.ToString(Globalization.CultureInfo.InvariantCulture)
                        $"sample,%s{server},,{s.time:o},%s{s.instrument},%s{rate},%i{s.count}"
            ]
        ))

    let withServerMetrics (session: MetricsSession) (m: Measurement) =
        m, summarize refreshInterval (session.Between(m.started, m.ended))

    printfn ""
    printfn "| Scenario | Server | req/s | vs Baseline req/s (min–max) | CPU µs/req | vs Baseline CPU (min–max) | B/req | GCs / 10k req | GC pause | Server p50 / p99 ms | Client p50 ms | Failures |"
    printfn "|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|"

    for scenario in selected do
        let rounds =
            results
            |> List.filter (fun (s, _) -> s = scenario)
            |> List.map (fun (_, (b, o)) -> withServerMetrics baselineMetrics b, withServerMetrics orsakMetrics o)

        for line in rows scenario rounds do
            printfn "%s" line

    let measurements = results |> List.collect (fun (_, (b, o)) -> [ b; o ])
    let busiest = measurements |> List.map _.clientLoad |> List.max

    if busiest > 0.9 then
        eprintfn
            $"\n!! The driver used up to %.0f{busiest * 100.}%% of its CPUs, so requests/s may be limited by the driver rather than the servers. Compare CPU µs/req, or give the driver more CPUs."

    // again after the table, where it sits next to the results it applies to
    unpinnedWarning options |> Option.iter (eprintfn "\n%s")

    if measurements |> List.exists (fun m -> m.failures > 0) then 1 else 0
