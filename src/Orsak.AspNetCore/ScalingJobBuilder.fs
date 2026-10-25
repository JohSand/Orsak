namespace Orsak.AspNetCore

open System
open System.Threading
open System.Threading.Tasks
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Hosting
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Quotations
open FSharp.Control
open Orsak


/// A job whose workload is a function, so the builder needs no subclass per job.
type DelegatingScalingEffectJob<'r, 'err>
    (
        name,
        logger,
        _services: IServiceProvider,
        work: unit -> Effect<'r, unit, 'err>,
        runnerFactory,
        workload: CancellationToken -> Task<WorkerLoad>,
        ?maxExtraWorkers,
        ?monitorInterval,
        ?restartDelay,
        ?maxRestartDelay
    ) =
    inherit
        ScalingEffectJob<'r, unit, 'err>(
            name,
            logger,
            (),
            work,
            runnerFactory,
            ?maxExtraWorkers = maxExtraWorkers,
            ?monitorInterval = monitorInterval,
            ?restartDelay = restartDelay,
            ?maxRestartDelay = maxRestartDelay
        )

    override _.GetWorkload token = workload token

// Markers for the operations a job needs, until they are given: Run only accepts a spec without them.
type WorkNotSet = WorkNotSet
type EnvironmentNotSet = EnvironmentNotSet
type WorkloadNotSet = WorkloadNotSet

/// What the builder has collected so far. 'Work, 'Env and 'Load start as markers, and become the types of the
/// functions given to the work, environment and workload operations.
type ScalingJobSpec<'Work, 'Env, 'Load> = {
    Name: string option
    Work: 'Work
    Environment: 'Env
    Workload: 'Load
    MaxExtraWorkers: int option
    MonitorInterval: TimeSpan option
    RestartDelay: TimeSpan option
    MaxRestartDelay: TimeSpan option
}

/// A job ready to be added to the services.
type ScalingJobDefinition = { Name: string; Create: IServiceProvider -> IHostedService }

type ScalingJobBuilder(name: string option) =
    member _.Yield(()) : ScalingJobSpec<WorkNotSet, EnvironmentNotSet, WorkloadNotSet> = {
        Name = name
        Work = WorkNotSet
        Environment = EnvironmentNotSet
        Workload = WorkloadNotSet
        MaxExtraWorkers = None
        MonitorInterval = None
        RestartDelay = None
        MaxRestartDelay = None
    }

    /// The effect each worker runs over and over, created once per worker from the services.
    [<CustomOperation("work")>]
    member _.Work
        (
            spec: ScalingJobSpec<_, 'Env, 'Load>,
            [<ReflectedDefinition(includeValue = true)>] work: Expr<unit -> Effect<'r, unit, 'err>>
        ) =
        // without a name of its own, the job is named after its work, when that is a function passed by name
        let work, shape = ReflectedFunction.capture work
        let name = spec.Name |> Option.orElse shape.Name

        {
            Name = name
            Work = work
            Environment = spec.Environment
            Workload = spec.Workload
            MaxExtraWorkers = spec.MaxExtraWorkers
            MonitorInterval = spec.MonitorInterval
            RestartDelay = spec.RestartDelay
            MaxRestartDelay = spec.MaxRestartDelay
        }

    /// The environment of a worker, from the services and the token that stops the worker.
    [<CustomOperation("environment")>]
    member _.Environment(spec: ScalingJobSpec<'Work, _, 'Load>, create: IServiceProvider -> CancellationToken -> 'r) = {
        Name = spec.Name
        Work = spec.Work
        Environment = create
        Workload = spec.Workload
        MaxExtraWorkers = spec.MaxExtraWorkers
        MonitorInterval = spec.MonitorInterval
        RestartDelay = spec.RestartDelay
        MaxRestartDelay = spec.MaxRestartDelay
    }

    /// How much work there is, from the services, checked to decide whether to add or remove a worker.
    [<CustomOperation("workload")>]
    member _.Workload
        (spec: ScalingJobSpec<'Work, 'Env, _>, workload: IServiceProvider -> CancellationToken -> Task<WorkerLoad>)
        = {
        Name = spec.Name
        Work = spec.Work
        Environment = spec.Environment
        Workload = workload
        MaxExtraWorkers = spec.MaxExtraWorkers
        MonitorInterval = spec.MonitorInterval
        RestartDelay = spec.RestartDelay
        MaxRestartDelay = spec.MaxRestartDelay
    }

    [<CustomOperation("maxExtraWorkers")>]
    member _.MaxExtraWorkers(spec: ScalingJobSpec<_, _, _>, count) = { spec with MaxExtraWorkers = Some count }

    [<CustomOperation("monitorEvery")>]
    member _.MonitorEvery(spec: ScalingJobSpec<_, _, _>, interval) = { spec with MonitorInterval = Some interval }

    [<CustomOperation("restartAfter")>]
    member _.RestartAfter(spec: ScalingJobSpec<_, _, _>, delay) = { spec with RestartDelay = Some delay }

    [<CustomOperation("maxRestartDelay")>]
    member _.MaxRestartDelay(spec: ScalingJobSpec<_, _, _>, delay) = { spec with MaxRestartDelay = Some delay }

    member _.Run
        (spec:
            ScalingJobSpec<unit -> Effect<'r, unit, 'err>, IServiceProvider -> CancellationToken -> 'r, WorkloadNotSet>)
        : ScalingJobDefinition =
        let name = spec.Name |> Option.defaultValue "ScalingJob"

        {
            Name = name
            Create =
                fun services ->
                    let logger = services.GetRequiredService<ILoggerFactory>().CreateLogger(name)

                    new DelegatingScalingEffectJob<'r, 'err>(
                        name,
                        logger,
                        services,
                        spec.Work,
                        spec.Environment services,
                        (fun _ -> task { return WorkerLoad.Sustained }),
                        ?maxExtraWorkers = spec.MaxExtraWorkers,
                        ?monitorInterval = spec.MonitorInterval,
                        ?restartDelay = spec.RestartDelay,
                        ?maxRestartDelay = spec.MaxRestartDelay
                    )
                    :> IHostedService
        }

    /// Only a spec with work, an environment and a workload gets here: 'r and 'err are shared by them.
    member _.Run
        (spec:
            ScalingJobSpec<
                unit -> Effect<'r, unit, 'err>,
                IServiceProvider -> CancellationToken -> 'r,
                IServiceProvider -> CancellationToken -> Task<WorkerLoad>
             >)
        : ScalingJobDefinition =
        let name = spec.Name |> Option.defaultValue "ScalingJob"

        {
            Name = name
            Create =
                fun services ->
                    let logger = services.GetRequiredService<ILoggerFactory>().CreateLogger(name)

                    new DelegatingScalingEffectJob<'r, 'err>(
                        name,
                        logger,
                        services,
                        spec.Work,
                        spec.Environment services,
                        spec.Workload services,
                        ?maxExtraWorkers = spec.MaxExtraWorkers,
                        ?monitorInterval = spec.MonitorInterval,
                        ?restartDelay = spec.RestartDelay,
                        ?maxRestartDelay = spec.MaxRestartDelay
                    )
                    :> IHostedService
        }
#if NET8_0_OR_GREATER
/// A job whose environment is extended with a cancellation scope for each run of its work, by `extend`: usually an
/// environment's Extend member, such as one Myriad generates, as in `scalingJob2 env.Extend { ... }`.
type ScalingJobBuilder2<'r>(extend: CancellationTokenSource -> 'r) =
    inherit ScalingJobBuilder(None)

    member _.Run
        (spec: ScalingJobSpec<unit -> Effect<'r, unit, 'err>, EnvironmentNotSet, WorkloadNotSet>)
        : ScalingJobDefinition =
        let name = spec.Name |> Option.defaultValue "ScalingJob"

        {
            Name = name
            Create =
                fun services ->
                    let logger = services.GetRequiredService<ILoggerFactory>().CreateLogger(name)

                    // each run of the work gets a cancellation scope of its own, linked to the worker's token and
                    // disposed when the run ends, so the environment's source never outlives the effects using it
                    let work () =
                        let effect = spec.Work()

                        mkEffect (fun (token: CancellationToken) -> vtask {
                            use scope = CancellationScope.LinkedTo token
                            return! effect.Run(extend scope)
                        })

                    new DelegatingScalingEffectJob<CancellationToken, 'err>(
                        name,
                        logger,
                        services,
                        work,
                        id,
                        (fun _ -> task { return WorkerLoad.Sustained }),
                        ?maxExtraWorkers = spec.MaxExtraWorkers,
                        ?monitorInterval = spec.MonitorInterval,
                        ?restartDelay = spec.RestartDelay,
                        ?maxRestartDelay = spec.MaxRestartDelay
                    )
                    :> IHostedService
        }
#endif
[<AutoOpen>]
module ScalingJob =
    /// A job named after its work function
    let scalingJob = ScalingJobBuilder(None)
    /// A job with a name of its own
    let namedScalingJob name = ScalingJobBuilder(Some name)
#if NET8_0_OR_GREATER
    let scalingJob2 extend = ScalingJobBuilder2(extend)
#endif
    type IServiceCollection with
        member services.AddScalingJob(job: ScalingJobDefinition) =
            services.AddHostedService(fun sp -> job.Create sp :?> BackgroundService)
