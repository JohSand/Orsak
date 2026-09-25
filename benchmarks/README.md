# Benchmarks

Performance measurements for Orsak, separate from `experiments/`, which is for throwaway work.

| Project | What it measures |
|---|---|
| `Orsak.Benchmarks` | BenchmarkDotNet micro benchmarks: `Effect.par`/`whenAll`, `effSeq`, and Orsak.AspNetCore routing (`RoutingBenchmarks.fs`) |
| `LoadTest.Baseline` | An ASP.NET Core app using minimal API endpoints |
| `LoadTest.Orsak` | The same endpoints through Orsak.AspNetCore routing (`RouteGet`, `RouteGet2`) |
| `LoadTest.Driver` | Drives HTTP load against both apps and prints them side by side |

Build everything first:

```
dotnet build benchmarks/benchmarks.sln -c Release
```

## Micro benchmarks

```
dotnet run -c Release --project benchmarks/Orsak.Benchmarks -- --filter "*Routing*" --affinity 1
```

`--filter` selects benchmarks by name (`*` for all); results are written to `BenchmarkDotNet.Artifacts/`.

## Load test

```
dotnet run -c Release --project benchmarks/LoadTest.Driver -- --server-cpus FF --client-cpus FF00
```

The driver starts both apps as their own processes, on the same CPUs, and warms each up on every scenario.
It then measures in rounds: in each round every scenario runs against both apps, one after the other, with
a warm-up and a measured phase of a fixed number of connections sending requests back to back. The order
of the two apps flips every round, so that drift over the run doesn't all land on one app.

The apps' own metrics are read from outside their processes, over EventPipe, the way `dotnet-counters`
does: the runtime's `System.Runtime` meter and ASP.NET Core's `http.server.request.duration`, as published by
the `System.Diagnostics.Metrics` event source. The apps need no endpoint or package for it. They publish
every 0.25 s, and each measurement uses the intervals that lie entirely inside its measured phase, divided by
the number of requests the app itself counted in those intervals.

The apps run with Server GC but without DATAS, its dynamic number of heaps (`DOTNET_GCDynamicAdaptationMode=0`,
set by the driver). DATAS is on by default since .NET 9 and keeps adapting the heap count during a run, which
changed how often the GC ran by about 10x from one round to the next. While it did, the runtime's
`dotnet.gc.heap.total_allocated` stopped increasing for up to a minute, so that allocations read as 0. A
measurement with requests but no allocations shows no value rather than 0.

For each scenario and app it prints the median over the rounds of:

- requests/s, and on Orsak's row the Orsak / Baseline ratio, with the lowest and highest of any round;
- server CPU time per request (`dotnet.process.cpu.time`), and its ratio in the same way;
- server bytes allocated per request (`dotnet.gc.heap.total_allocated`);
- garbage collections per 10 000 requests (`dotnet.gc.collections`) and the share of time paused for them
  (`dotnet.gc.pause.time`);
- p50 and p99 of the request duration inside the server (`http.server.request.duration`);
- p50 latency seen by the driver, and the number of failed requests (with the server log if any failed).

Requests/s is limited by whichever of the driver and the app saturates first. On one machine that is often
the driver, which then reports similar requests/s for both apps; it warns when its CPUs were more than 90%
busy. Server CPU time per request doesn't depend on that, so compare it first. The driver's own p99 is not
reported: each connection waits for its response before sending the next request, so a stalled server is
sent fewer requests, and the slow tail is undercounted. The server-side p99 doesn't have that problem, but
only covers the time inside ASP.NET Core's hosting layer, not Kestrel's reading and writing.

Reading the metrics adds a little work per request to both apps: the event source aggregates the request
duration histogram on top of OpenTelemetry's. Compared with reading them from inside the apps, this measured
about 1 µs and 100 B more per request, the same for both.

Options:

| Option | Default | |
|---|---|---|
| `--rounds <n>` | 3 | rounds over all scenarios |
| `--duration <s>` | 5 | measured seconds per scenario and app, each round |
| `--warmup <s>` | 2 | warm-up seconds before each measurement, and per scenario before the first round |
| `--connections <n>` | 64 | concurrent connections |
| `--only <text>` | | only scenarios whose name contains `<text>` |
| `--server-cpus <mask>` | | pin the apps to these logical CPUs (hex mask) |
| `--client-cpus <mask>` | | pin the driver to these logical CPUs (hex mask) |

## Getting stable numbers

- Pin to CPUs. The driver and the apps share the machine, so give them separate cores. On a CPU with
  performance and efficiency cores (e.g. Intel 12th gen and later), pin both to performance cores: unpinned
  runs landing on efficiency cores have been seen to differ by 2x. On an i7-12700, logical CPUs 0-15 are
  the performance cores, hence `FF` / `FF00` above and `--affinity 1` for BenchmarkDotNet.
- Look at the ratio's range before trusting a difference of a few percent, and add rounds if it is wide.
- Routing itself costs tens of nanoseconds per request, far below what the load test can resolve; use the
  micro benchmarks for that. The load test shows what a whole request costs, with Kestrel and OpenTelemetry.
