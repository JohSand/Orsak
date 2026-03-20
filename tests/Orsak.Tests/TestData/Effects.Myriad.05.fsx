#r "../bin/Debug/net10.0/Orsak.dll"
#r "../bin/Debug/net10.0/Orsak.Myriad.dll"
#r "../bin/Debug/net10.0/Microsoft.Extensions.Logging.Abstractions.dll"

open Microsoft.Extensions.Logging

open System
open Orsak
open Orsak.Myriad

[<GenRunner(Name = "LoggerRunner")>]
type IRunner =
    inherit IProvide<ILoggerFactory>
