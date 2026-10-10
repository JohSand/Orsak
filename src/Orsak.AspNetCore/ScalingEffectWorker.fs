namespace Orsak.AspNetCore

open System.Collections.Generic
open Microsoft.Extensions.Hosting
open System.Threading
open System.Threading.Tasks
open System
open System.Collections.Concurrent
open Microsoft.Extensions.Logging
open Orsak

/// <summary>
/// How much work a <see cref="T:Orsak.AspNetCore.ScalingEffectJob`3"/> has, and so whether it should change its number
/// of workers.
/// </summary>
type WorkerLoad =
    /// <summary>More than the workers keep up with: one more worker is added, up to the maximum.</summary>
    | Increased
    /// <summary>Less than the workers have capacity for: one worker is removed, down to the baseline worker.</summary>
    | Low
    /// <summary>About what the workers handle: their number stays the same.</summary>
    | Sustained

/// <summary>
/// A background service that runs an effect over and over in a baseline worker, and in extra workers added and removed
/// according to <c>GetWorkload</c>, which is checked every <c>monitorInterval</c>, 10 seconds by default. At most
/// <c>maxExtraWorkers</c> are added, 10 by default.
/// </summary>
/// <remarks>
/// <para>
/// The service keeps its workers running, but leaves how each run behaves to the effect. When a run fails or throws,
/// the error is logged, and the worker waits before running the effect again, from <c>restartDelay</c>, 1 second by
/// default, doubling with each failure in a row, up to <c>maxRestartDelay</c>, 1 minute by default. Retrying, backing
/// off when there is nothing to do, and logging errors in a way that suits the work, belong in the effect, composed
/// with the functions of <c>Orsak.Resilience</c>.
/// </para>
/// <para>
/// <paramref name="work"/> is called once for each worker, and its effect is run by that worker until it stops.
/// Compose the effect inside <paramref name="work"/>, so each worker gets its own state, such as the backoff of
/// <c>Effect.addDelay</c>:
/// <code lang="fsharp">
/// fun queue -> handleNext queue |> Effect.addDelayWithMax_ 1.&lt;s&gt; 10.&lt;s&gt;
/// </code>
/// A worker stops when its token is cancelled. The token is passed to <paramref name="runnerFactory"/>, and delays
/// in the effect only end early when the environment provides it, such as through <c>ICancellationProvider</c>.
/// </para>
/// </remarks>
/// <param name="name">Names the job in logs and metrics</param>
/// <param name="logger">Logs the failures of the workers, and of <c>GetWorkload</c></param>
/// <param name="state">Passed to <paramref name="work"/></param>
/// <param name="work">Creates the effect a worker runs, once for each worker</param>
/// <param name="runnerFactory">Creates the environment of a worker, from the token that stops it</param>
[<AbstractClass>]
type ScalingEffectJob<'r, 's, 'err>
    (
        name: string,
        logger: ILogger,
        state: 's,
        work: 's -> Effect<'r, unit, 'err>,
        runnerFactory: CancellationToken -> 'r,
        ?maxExtraWorkers: int,
        ?monitorInterval: TimeSpan,
        ?restartDelay: TimeSpan,
        ?maxRestartDelay: TimeSpan
    ) =
    inherit BackgroundService()

    let extraWorkers = ConcurrentStack<CancellationTokenSource * Task<unit>>()
    let maxExtraWorkers = defaultArg maxExtraWorkers 10
    let monitorInterval = defaultArg monitorInterval (TimeSpan.FromSeconds 10.)
    let restartDelay = defaultArg restartDelay (TimeSpan.FromSeconds 1.)
    let maxRestartDelay = defaultArg maxRestartDelay (TimeSpan.FromMinutes 1.)

    /// The delay after the given number of failures in a row: doubling from restartDelay, up to maxRestartDelay.
    let delayAfter (failures: int) =
        let factor = Math.Pow(2., float (min (failures - 1) 30))
        TimeSpan.FromTicks(min maxRestartDelay.Ticks (int64 (float restartDelay.Ticks * factor)))

#if NET8_0_OR_GREATER
    let workerTag = KeyValuePair<string, obj>("sub", name)
#endif

    /// Runs the effect of a worker until the token is cancelled, logging failures, and waiting after them.
    let runWorker (token: CancellationToken) = task {
        // so starting a worker doesn't block the caller, such as the host starting up
        do! Task.Yield()
#if NET8_0_OR_GREATER
        EffectDiagnostics.workers.Add(1, workerTag)
#endif
        try
            let runner = runnerFactory token
            let effect = work state
            let mutable failures = 0

            while not token.IsCancellationRequested do
                try
                    match! effect |> Effect.run runner with
                    | Ok() -> failures <- 0
                    | Error err ->
                        failures <- failures + 1
                        logger.LogError("{Job} failed with {Error}, and will run again", name, box err)
                with
                | :? OperationCanceledException when token.IsCancellationRequested -> ()
                | exn ->
                    failures <- failures + 1
                    logger.LogError(exn, "{Job} failed with an exception, and will run again", name)

                if failures > 0 && not token.IsCancellationRequested then
                    try
                        do! Task.Delay(delayAfter failures, token)
                    with :? OperationCanceledException ->
                        ()
        finally
#if NET8_0_OR_GREATER
            EffectDiagnostics.workers.Add(-1, workerTag)
#else
            ()
#endif
    }

    member private this.AddWorker(token: CancellationToken) =
        let source = CancellationTokenSource.CreateLinkedTokenSource(token)
        extraWorkers.Push((source, runWorker source.Token))

    member private this.RemoveWorker() = task {
        match extraWorkers.TryPop() with
        | true, (source, worker) ->
            use source = source
            source.Cancel()
            do! worker
        | false, _ -> ()
    }

    member private this.MonitorWorkers(token: CancellationToken) = task {
        do! Task.Yield()

        while not token.IsCancellationRequested do
            try
                match! this.GetWorkload token with
                | Increased when extraWorkers.Count < maxExtraWorkers -> this.AddWorker token
                | Increased
                | Sustained -> ()
                | Low -> do! this.RemoveWorker()
            with
            | :? OperationCanceledException when token.IsCancellationRequested -> ()
            | exn -> logger.LogError(exn, "{Job} failed to get its workload, and will try again", name)

            try
                do! Task.Delay(monitorInterval, token)
            with :? OperationCanceledException ->
                ()
    }

    /// <summary>
    /// How much work the job has, checked every <c>monitorInterval</c> to decide whether to add or remove a worker.
    /// An exception is logged, and the workers stay as they are until the next check.
    /// </summary>
    abstract member GetWorkload: CancellationToken -> Task<WorkerLoad>

    /// <summary>
    /// Runs the baseline worker and the monitor until <paramref name="token"/> is cancelled, and then stops the extra
    /// workers.
    /// </summary>
    /// <param name="token">Stops the job</param>
    override this.ExecuteAsync(token: CancellationToken) = task {
        let monitor = this.MonitorWorkers token
        do! runWorker token
        do! monitor

        while extraWorkers.Count > 0 do
            do! this.RemoveWorker()
    }
