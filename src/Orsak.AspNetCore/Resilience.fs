namespace rec Orsak.AspNetCore

open System
open System.Threading
open System.Threading.Tasks
open Polly
open Polly.Retry
open Orsak
open Microsoft.FSharp.Linq
open Microsoft.Extensions.Logging

type Nothing = private Nothing of Nothing

[<AutoOpen>]
module NothingPatterns =
    let (|Safe|) (r: Result<'a, Nothing>) =
        match r with
        | Ok a -> a
        | Error(Nothing _) ->
            Throwhelpers.argumentException "Nothing should be impossible to construct."
            Unchecked.defaultof<_>

type RetryStrategy(_p: TimeProvider, logger: ILogger, t: CancellationToken) =
    let d = RetryStrategyOptions()
    let mutable attempt = -1
    let mutable prevDelay = 0.

    new(logger, ct) = RetryStrategy(TimeProvider.System, logger, ct)

    member _.TimeProvider = _p

    member private _.Delay() =
        if attempt = -1 then
            attempt <- attempt + 1
            Task.CompletedTask
        else
            let (delay: TimeSpan) = Resilience.getRetryDelay attempt &prevDelay d
            attempt <- attempt + 1
            logger.LogWarning("Blaha")
#if NET8_0_OR_GREATER
            Task.Delay(delay, _p, t)
#else
            Task.Delay(delay, t)
#endif
    member this.Delay(e: Effect<_, _, _>) = eff {
        do! this.Delay()
        return! e
    }

//Adapted from Polly.Contrib.WaitAndRetry
//New BSD License
//=
//Copyright (c) 2019, AppvNext and contributors
//All rights reserved.

//Redistribution and use in source and binary forms, with or without
//modification, are permitted provided that the following conditions are met:
//    * Redistributions of source code must retain the above copyright
//      notice, this list of conditions and the following disclaimer.
//    * Redistributions in binary form must reproduce the above copyright
//      notice, this list of conditions and the following disclaimer in the
//      documentation and/or other materials provided with the distribution.
//    * Neither the name of App vNext nor the
//      names of its contributors may be used to endorse or promote products
//      derived from this software without specific prior written permission.

//THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
//ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
//WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
//DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
//DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
//(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
//LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
//ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
//(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
//SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
module Resilience =
    [<Literal>]
    let ExponentialFactor = 2.0

    [<Literal>]
    let JitterFactor = 0.5

    [<Literal>]
    let Pfactor = 4.0

    [<Literal>]
    let RpScalingFactor = 1. / 1.4

    let backofJitter (attempt: int) (prev: byref<float>) (opts: RetryStrategyOptions) =
        let exponent = float attempt + opts.Randomizer.Invoke()
        let next = ExponentialFactor ** exponent * Math.Tanh(Math.Sqrt(Pfactor * exponent))
        let rp = next - prev
        prev <- next
        let ticks = Math.Min(rp * RpScalingFactor * float opts.Delay.Ticks, (float) TimeSpan.MaxValue.Ticks - 1000.)

        TimeSpan.FromTicks(int64 ticks)

    let applyJitter (delay: TimeSpan) (delayer: RetryStrategyOptions) =
        let offset = delay.TotalMilliseconds * JitterFactor / 2.

        let randomDelay =
            delay.TotalMilliseconds * JitterFactor * (delayer.Randomizer.Invoke()) - offset

        let newDelay = delay.TotalMilliseconds + randomDelay
        TimeSpan.FromMilliseconds(newDelay)

    let getRetryDelay (attempt: int) (prev: byref<float>) (opts: RetryStrategyOptions) =
        try
            let delay =
                if (opts.Delay = TimeSpan.Zero) then
                    TimeSpan.Zero
                else
                    match opts.BackoffType with
                    | DelayBackoffType.Constant when opts.UseJitter -> applyJitter opts.Delay opts
                    | DelayBackoffType.Constant -> opts.Delay

                    | DelayBackoffType.Linear when opts.UseJitter -> applyJitter (float (attempt + 1) * opts.Delay) opts
                    | DelayBackoffType.Linear -> float (attempt + 1) * opts.Delay

                    | DelayBackoffType.Exponential when opts.UseJitter -> backofJitter attempt &prev opts
                    | DelayBackoffType.Exponential -> ExponentialFactor ** (float attempt) * opts.Delay
                    | t ->
                        Throwhelpers.argumentOutOfRange t
                        TimeSpan.Zero

            if opts.MaxDelay ?<= delay then
                opts.MaxDelay.Value
            else
                delay
        with :? OverflowException ->
            if opts.MaxDelay.HasValue then
                opts.MaxDelay.Value
            else
                TimeSpan.MaxValue

module Effect =
    let untilCancellation logger (ct: CancellationToken) (e: Effect<'r, unit, 'e>) =
        let d = RetryStrategy(logger, ct)

        eff {

            while not ct.IsCancellationRequested do
                try
                    do! Effect.forever (d.Delay(e))
                with
                | :? TaskCanceledException -> return ()
                | _ ->
                    logger.LogWarning("The effect failed with an exception. The effect will retry after a delay.")
        #if NET8_0_OR_GREATER
                    do! Task.Delay(TimeSpan.FromSeconds 1000, d.TimeProvider, ct)
        #else
                    do! Task.Delay(TimeSpan.FromSeconds 1000, ct)
        #endif

        }
        |> Effect.changeError (fun _ -> Unchecked.defaultof<Nothing>)
