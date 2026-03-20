#r "../bin/Debug/net10.0/Orsak.dll"
#r "../bin/Debug/net10.0/Orsak.Myriad.dll"
#r "../bin/Debug/net10.0/Microsoft.Extensions.Logging.Abstractions.dll"
namespace Tmp
open Microsoft.Extensions.Logging
open System
open Orsak
open Orsak.Myriad
type LoggerRunner = {
    LoggerFactory: ILoggerFactory
} with
    interface ILoggerFactory with
        member this.CreateLogger s = this.LoggerFactory.CreateLogger s
        member this.AddProvider s = this.LoggerFactory.AddProvider s
    interface Orsak.ILoggerProvider with
        member this.Effect = this.LoggerFactory.CreateLogger(this.GetType())
    interface System.IDisposable with
        member this.Dispose() = this.LoggerFactory.Dispose()
namespace Orsak.Myriad.Gen.Tmp
open Microsoft.Extensions.Logging
open System
open Orsak
open Orsak.Myriad
open Orsak.Myriad.Gen
open System.Runtime.InteropServices
open Tmp
type LoggerFactoryExtractor =
    static member Extract(_: LoggerFactoryExtractor, e: EffectContext<ILoggerFactory>) = e.A
[<AutoOpen>]
module Extractors =
    let inline (|ExtractLoggerFactory|) a : ILoggerFactory =
        Writer.extract Unchecked.defaultof<LoggerFactoryExtractor> a
type EffectRunnerBuilder() =
    member _.Yield(_: unit) = EffectContext()
    [<CustomOperation("fromEffect")>]
    member inline _.FromEffect(x: GenContext<_, _, _>, p: ILoggerFactory) = x.Create(p)
    member inline _.Run(a) =
        match a with
        | ExtractLoggerFactory(a) -> {
            LoggerFactory = a
        }
namespace Tmp
open Orsak.Myriad.Gen.Tmp
[<AutoOpen>]
module Runner =
    let mkRunner = EffectRunnerBuilder()
