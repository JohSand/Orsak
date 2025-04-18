module Orsak.Tests.GenWriterTests


open Orsak
open Orsak.Myriad
open Orsak.Myriad
open Orsak.Myriad.Gen
open Xunit
open System.Text
open System.Threading
open Microsoft.Extensions.Caching.Memory

[<Fact>]
let writeCorrectParmeters () =
    let sb = StringBuilder().ToIndentingBuilder()
    do Writer.writeTypeParameters "Test" 5 1 sb
    let result = sb.ToString()
    Assert.Equal("<Test, 'b, 'c, 'd, 'e>", result)
    ()


[<Fact>]
let correctWriteExtractorMembers () =
    let sb = StringBuilder().ToIndentingBuilder()
    do Writer.writeExtractorMembers "RandomGeneratorExtractor" "IRandomGenerator" 5 sb
    let result = sb.ToString()

    let expected =
        "\
static member Extract(_: RandomGeneratorExtractor, e: EffectContext<IRandomGenerator, 'b, 'c, 'd, 'e>) = e.A
static member Extract(_: RandomGeneratorExtractor, e: EffectContext<'a, IRandomGenerator, 'c, 'd, 'e>) = e.B
static member Extract(_: RandomGeneratorExtractor, e: EffectContext<'a, 'b, IRandomGenerator, 'd, 'e>) = e.C
static member Extract(_: RandomGeneratorExtractor, e: EffectContext<'a, 'b, 'c, IRandomGenerator, 'e>) = e.D
static member Extract(_: RandomGeneratorExtractor, e: EffectContext<'a, 'b, 'c, 'd, IRandomGenerator>) = e.E
"

    Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
    ()

[<Fact>]
let correctWriteExtractor () =
    let sb = StringBuilder().ToIndentingBuilder()
    do Writer.writeExtractor "IRandomGenerator" [| 2; 5 |] sb
    let result = sb.ToString()

    let expected =
        "\
type RandomGeneratorExtractor =
    static member Extract(_: RandomGeneratorExtractor, e: EffectContext<IRandomGenerator, 'b>) = e.A
    static member Extract(_: RandomGeneratorExtractor, e: EffectContext<'a, IRandomGenerator>) = e.B
    static member Extract(_: RandomGeneratorExtractor, e: EffectContext<IRandomGenerator, 'b, 'c, 'd, 'e>) = e.A
    static member Extract(_: RandomGeneratorExtractor, e: EffectContext<'a, IRandomGenerator, 'c, 'd, 'e>) = e.B
    static member Extract(_: RandomGeneratorExtractor, e: EffectContext<'a, 'b, IRandomGenerator, 'd, 'e>) = e.C
    static member Extract(_: RandomGeneratorExtractor, e: EffectContext<'a, 'b, 'c, IRandomGenerator, 'e>) = e.D
    static member Extract(_: RandomGeneratorExtractor, e: EffectContext<'a, 'b, 'c, 'd, IRandomGenerator>) = e.E
"

    Assert.Equal(expected, result, ignoreLineEndingDifferences = true)

[<Fact>]
let testTrimI () =
    let trimmed = Writer.trimI "IMemoryCache"
    Assert.Equal("MemoryCache", trimmed)


[<Fact>]
let testTrimI2 () =
    let trimmed = Writer.trimI "MemoryCache"
    Assert.Equal("MemoryCache", trimmed)

[<Fact>]
let testWriteRun () =
    let sb = StringBuilder().ToIndentingBuilder()
    Writer.writeRunMethod 0 0 [| "CancellationTokenSource"; "IMemoryCache"; "IRandomGenerator" |] sb
    let x = sb.ToString()

    let expected =
        "\
member inline _.Run(a) =
    match a with
    | ExtractCancellationTokenSource(a) & ExtractMemoryCache(b) & ExtractRandomGenerator(c) -> {
        CancellationTokenSource = a
        MemoryCache = b
        RandomGenerator = c
    }
"

    Assert.Equal(expected, x, ignoreLineEndingDifferences = true)

[<Fact>]
let testWriteRun2 () =
    let sb = StringBuilder().ToIndentingBuilder()
    Writer.writeRunMethod 0 2 [| "CancellationTokenSource"; "IMemoryCache"; "IRandomGenerator" |] sb
    let x = sb.ToString()

    let expected =
        "\
member inline _.Run(a, [<Optional>]_a: byte, [<Optional>]_b: sbyte) =
    match a with
    | ExtractCancellationTokenSource(a) & ExtractMemoryCache(b) & ExtractRandomGenerator(c) -> {
        CancellationTokenSource = a
        MemoryCache = b
        RandomGenerator = c
    }
"

    Assert.Equal(expected, x, ignoreLineEndingDifferences = true)

[<Fact>]
let testWriteRun4 () =
    let sb = StringBuilder().ToIndentingBuilder()
    Writer.writeRunMethod 10 5 [| "CancellationTokenSource"; "IMemoryCache"; "IRandomGenerator" |] sb
    let x = sb.ToString()

    let expected =
        "\
member inline _.Run(a, [<Optional>]_a: double, [<Optional>]_b: char, [<Optional>]_c: bool, [<Optional>]_d: byte, [<Optional>]_e: sbyte) =
    match a with
    | ExtractCancellationTokenSource(a) & ExtractMemoryCache(b) & ExtractRandomGenerator(c) -> {
        CancellationTokenSource = a
        MemoryCache = b
        RandomGenerator = c
    }
"

    Assert.Equal(expected, x, ignoreLineEndingDifferences = true)

[<Fact>]
let testWriteRun3 () =
    let sb = StringBuilder().ToIndentingBuilder()

    let arity = {
        requiredTieBreakers = 2
        runners = [|
            { effects = [| "CancellationTokenSource"; "IMemoryCache" |]; name = "todo" }
            { effects = [| "IMemoryCache"; "IRandomGenerator" |]; name = "todo" }
        |]
        arity = 2
    }

    Writer.writeRunMethods arity sb
    let x = sb.ToString()

    let expected =
        "\
member inline _.Run(a, [<Optional>]_a: byte, [<Optional>]_b: sbyte) =
    match a with
    | ExtractCancellationTokenSource(a) & ExtractMemoryCache(b) -> {
        CancellationTokenSource = a
        MemoryCache = b
    }
member inline _.Run(a, [<Optional>]_a: sbyte, [<Optional>]_b: int16) =
    match a with
    | ExtractMemoryCache(a) & ExtractRandomGenerator(b) -> {
        MemoryCache = a
        RandomGenerator = b
    }
"

    Assert.Equal(expected, x, ignoreLineEndingDifferences = true)


[<Fact>]
let testWriteRunner () =
    let sb = StringBuilder().ToIndentingBuilder()
    Writer.writeRunner { name = "Runner0"; effects =  [| "CancellationTokenSource"; "IMemoryCache"; "IRandomGenerator" |] } sb
    let x = sb.ToString()

    let expected =
        "\
type Runner0 = {
    CancellationTokenSource: CancellationTokenSource
    MemoryCache: IMemoryCache
    RandomGenerator: IRandomGenerator
} with
    interface IProvide<CancellationTokenSource> with
        member this.Effect = this.CancellationTokenSource
    interface IProvide<IMemoryCache> with
        member this.Effect = this.MemoryCache
    interface IProvide<IRandomGenerator> with
        member this.Effect = this.RandomGenerator
"

    Assert.Equal(expected, x, ignoreLineEndingDifferences = true)


[<Fact>]
let createLayerTests () =
    let sut = [
        [| "CancellationTokenSource"; "IMemoryCache" |]
        [| "CancellationTokenSource"; "IMemoryCache"; "IRandomGenerator" |]
        [| "IMemoryCache"; "IRandomGenerator" |]

    ]

    let result = Writer.orderLayers sut

    let expected: RunnerArityCfg array = [|
        {
            runners = [|
                { effects = [| "CancellationTokenSource"; "IMemoryCache"; "IRandomGenerator" |]; name = "Runner3W0" }
            |]
            requiredTieBreakers = 0
            arity = 3
        }
        {
            runners = [|
                { effects = [| "CancellationTokenSource"; "IMemoryCache" |]; name = "Runner2W0" }
                { effects = [| "IMemoryCache"; "IRandomGenerator" |]; name = "Runner2W1" }
            |]
            requiredTieBreakers = 1
            arity = 2
        }
    |]

    Assert.Equal<RunnerArityCfg>(expected, result)

[<Fact>]
let writeExtractPatternTests () =
    let sb = StringBuilder().ToIndentingBuilder()
    Writer.writeExtractPattern "IMemoryCache" sb
    let result = sb.ToString()
    let expected =
        "\
let inline (|ExtractMemoryCache|) a : IMemoryCache =
    extract Unchecked.defaultof<MemoryCacheExtractor> a
"
    Assert.Equal(expected, result, ignoreLineEndingDifferences = true)
    ()
