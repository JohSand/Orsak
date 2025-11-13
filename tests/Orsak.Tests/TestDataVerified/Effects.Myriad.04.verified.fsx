#r "../bin/Debug/net9.0/Orsak.dll" 
#r "../bin/Debug/net9.0/Orsak.Myriad.dll" 
#r "../bin/Debug/net9.0/Microsoft.Extensions.Logging.Abstractions.dll" 
namespace Tmp
open System
open System.Threading
open Orsak
open Orsak.Myriad
type Runner3 = {
    GuidGenerator: IGuidGenerator
    Random: IRandom
    CancellationTokenSource: CancellationTokenSource
} with
    interface IGuidGenProvider with
        member this.GuidGenerator = this.GuidGenerator
    interface IRandomProvider with
        member this.Effect = this.Random
    interface ICancellationProvider with
        member this.Effect = this.CancellationTokenSource
type Runner2 = {
    GuidGenerator: IGuidGenerator
    CancellationTokenSource: CancellationTokenSource
} with
    interface IGuidGenProvider with
        member this.GuidGenerator = this.GuidGenerator
    interface ICancellationProvider with
        member this.Effect = this.CancellationTokenSource
type Runner1 = {
    GuidGenerator: IGuidGenerator
    TimeProvider: TimeProvider
} with
    interface IGuidGenProvider with
        member this.GuidGenerator = this.GuidGenerator
    interface ITimeProvider with
        member this.Clock = this.TimeProvider
namespace Orsak.Myriad.Gen.Tmp
open System
open System.Threading
open Orsak
open Orsak.Myriad
open Orsak.Myriad.Gen
open System.Runtime.InteropServices
open Tmp
type GuidGeneratorExtractor =
    static member Extract(_: GuidGeneratorExtractor, e: EffectContext<IGuidGenerator, 'b>) = e.A
    static member Extract(_: GuidGeneratorExtractor, e: EffectContext<'a, IGuidGenerator>) = e.B
    static member Extract(_: GuidGeneratorExtractor, e: EffectContext<IGuidGenerator, 'b, 'c>) = e.A
    static member Extract(_: GuidGeneratorExtractor, e: EffectContext<'a, IGuidGenerator, 'c>) = e.B
    static member Extract(_: GuidGeneratorExtractor, e: EffectContext<'a, 'b, IGuidGenerator>) = e.C
type RandomExtractor =
    static member Extract(_: RandomExtractor, e: EffectContext<IRandom, 'b, 'c>) = e.A
    static member Extract(_: RandomExtractor, e: EffectContext<'a, IRandom, 'c>) = e.B
    static member Extract(_: RandomExtractor, e: EffectContext<'a, 'b, IRandom>) = e.C
type CancellationTokenSourceExtractor =
    static member Extract(_: CancellationTokenSourceExtractor, e: EffectContext<CancellationTokenSource, 'b>) = e.A
    static member Extract(_: CancellationTokenSourceExtractor, e: EffectContext<'a, CancellationTokenSource>) = e.B
    static member Extract(_: CancellationTokenSourceExtractor, e: EffectContext<CancellationTokenSource, 'b, 'c>) = e.A
    static member Extract(_: CancellationTokenSourceExtractor, e: EffectContext<'a, CancellationTokenSource, 'c>) = e.B
    static member Extract(_: CancellationTokenSourceExtractor, e: EffectContext<'a, 'b, CancellationTokenSource>) = e.C
type TimeProviderExtractor =
    static member Extract(_: TimeProviderExtractor, e: EffectContext<TimeProvider, 'b>) = e.A
    static member Extract(_: TimeProviderExtractor, e: EffectContext<'a, TimeProvider>) = e.B
[<AutoOpen>]
module Extractors =
    let inline (|ExtractGuidGenerator|) a : IGuidGenerator =
        Writer.extract Unchecked.defaultof<GuidGeneratorExtractor> a
    let inline (|ExtractRandom|) a : IRandom =
        Writer.extract Unchecked.defaultof<RandomExtractor> a
    let inline (|ExtractCancellationTokenSource|) a : CancellationTokenSource =
        Writer.extract Unchecked.defaultof<CancellationTokenSourceExtractor> a
    let inline (|ExtractTimeProvider|) a : TimeProvider =
        Writer.extract Unchecked.defaultof<TimeProviderExtractor> a
type EffectRunnerBuilder() =
    member _.Yield(_: unit) = EffectContext()
    member inline _.Run(a: EffectContext<IGuidGenerator>) = { new IGuidGenProvider with member _.GuidGenerator = a.A }
    member inline _.Run(a: EffectContext<IRandom>) = { new IRandomProvider with member _.Effect = a.A }
    member inline _.Run(a: EffectContext<CancellationTokenSource>) = { new ICancellationProvider with member _.Effect = a.A }
    member inline _.Run(a: EffectContext<TimeProvider>) = { new ITimeProvider with member _.Clock = a.A }
    [<CustomOperation("fromEffect")>]
    member inline _.FromEffect(x: GenContext<_, _, _>, p: IGuidGenerator) = x.Create(p)
    [<CustomOperation("fromEffect")>]
    member inline _.FromEffect(x: GenContext<_, _, _>, p: IRandom) = x.Create(p)
    [<CustomOperation("fromEffect")>]
    member inline _.FromEffect(x: GenContext<_, _, _>, p: CancellationTokenSource) = x.Create(p)
    [<CustomOperation("fromEffect")>]
    member inline _.FromEffect(x: GenContext<_, _, _>, p: TimeProvider) = x.Create(p)
    member inline _.Run(a) =
        match a with
        | ExtractGuidGenerator(a) & ExtractRandom(b) & ExtractCancellationTokenSource(c) -> {
            GuidGenerator = a
            Random = b
            CancellationTokenSource = c
        }
namespace Tmp
open Orsak.Myriad.Gen.Tmp
open System.Runtime.InteropServices

[<AutoOpen>]
module Extension2 =
    type EffectRunnerBuilder with
        member inline _.Run(a, [<Optional>]_a: byte) =
            match a with
            | ExtractGuidGenerator(a) & ExtractCancellationTokenSource(b) -> {
                GuidGenerator = a
                CancellationTokenSource = b
            }
        member inline _.Run(a, [<Optional>]_a: sbyte) =
            match a with
            | ExtractGuidGenerator(a) & ExtractTimeProvider(b) -> {
                GuidGenerator = a
                TimeProvider = b
            }
namespace Tmp
open Orsak.Myriad.Gen.Tmp
[<AutoOpen>]
module Runner =
    let mkRunner = EffectRunnerBuilder()
