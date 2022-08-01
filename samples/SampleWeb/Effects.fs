namespace Orsak.Extensions

open System
open Orsak
open Microsoft.Extensions.Logging
open Microsoft.AspNetCore.Http

type ILoggerProvider =
    abstract member Logger: ILogger

type Log =
    static member getLogger () =
        Effect.Create(fun (provider: #ILoggerProvider) -> provider.Logger)

    static member logInformation(message, [<ParamArray>]args) =
        Effect.Create(fun (provider: #ILoggerProvider) -> 
            provider.Logger.LogInformation(message, args))

type IContextProvider =
    abstract member Context: HttpContext

module HttpContext =
    open Giraffe

    let current () =
        Effect.Create(fun (provider: #IContextProvider) -> provider.Context)

    let pushCorrelationId () =
        eff {
            let! logger = Log.getLogger ()            
            let! ctx = current ()
            match ctx.TryGetRequestHeader "X-Svea-CorrelationId" with
            | Some header ->
                return logger.BeginScope ("CorrelationId: {CorrelationId}", header)
            | None ->
                return logger.BeginScope ("")
        }