namespace Orsak.Extensions

open System
open Orsak
open Microsoft.Extensions.Logging
open Microsoft.AspNetCore.Http

type ILoggerProvider =
    abstract member Logger: ILogger

type Log =
    static member getLogger() =
        Effect.Create(fun (provider: #ILoggerProvider) -> provider.Logger)

    static member logInformation(message, [<ParamArray>] args) =
        Effect.Create(fun (provider: #ILoggerProvider) -> provider.Logger.LogInformation(message, args))

type IContextProvider =
    abstract member Context: HttpContext

module HttpContext =
    open Giraffe

    let current () =
        Effect.Create(fun (provider: #IContextProvider) -> provider.Context)
