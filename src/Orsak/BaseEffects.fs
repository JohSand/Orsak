namespace Orsak

open System
open System.Threading
open Microsoft.Extensions.Caching.Memory


type IGuidGenerator =
    abstract GenGuid: unit -> Guid

type IGuidGenProvider =
    abstract GuidGenerator: IGuidGenerator

module GuidGenerator =
    let genGuid () =
        Effect.Create(fun (provider: #IGuidGenProvider) -> provider.GuidGenerator.GenGuid())

    let defaultGen () =
        { new IGuidGenerator with
            member _.GenGuid() = Guid.NewGuid()
        }

#if NET8_0_OR_GREATER
type ITimeProvider =
    abstract Clock: TimeProvider

module Time =
    let utcNow () =
        Effect.Create(fun (provider: #ITimeProvider) -> provider.Clock.GetUtcNow())
#endif

type IRandomGenerator =
    abstract Next: (int * int) -> int
    abstract NextDouble: unit -> float

type DefaultRandom(rand: Random) =
    interface IRandomGenerator with
        member _.Next((minValue, maxValue)) = rand.Next(minValue, maxValue)
        member _.NextDouble() = rand.NextDouble()

type IRandomProvider =
    abstract Random: IRandomGenerator

module Random =
    let next min max =
        Effect.Create(fun (provider: #IRandomProvider) -> provider.Random.Next(min, max))

    let nextDouble () =
        Effect.Create(fun (provider: #IRandomProvider) -> provider.Random.NextDouble())


type ICacheProvider =
    abstract member Cache: IMemoryCache

module MemoryCache =
    let private get () =
        Effect.Create(fun (p: #ICacheProvider) -> p.Cache)

    //gives bad error if you don't bind the effect, not sure how big of an issue it is
    let getOrCreate (key: 'a) (f: ICacheEntry -> Effect<#ICacheProvider, 'entry, 'Err>) = eff {
        let! cache = get ()

        let mutable result = Unchecked.defaultof<'entry>

        if (not (cache.TryGetValue(key, &result))) then
            use entry = cache.CreateEntry(key)
            let! result' = f entry
            result <- result'
            entry.Value <- result

        return result
    }

    let updateOrDefault (key: string) (defaultValue: 'entry) (f: 'entry -> Effect<#ICacheProvider, 'entry, 'Err>) = eff {
        let! cache = get ()
        let mutable result = Unchecked.defaultof<'entry>

        if cache.TryGetValue(key, &result) then
            let! newResult = f result
            return cache.Set(key, newResult)
        else
            use entry = cache.CreateEntry(key)
            entry.Value <- defaultValue
            return defaultValue
    }

#if NET8_0_OR_GREATER
type ICancellationProvider =
    abstract member Source: CancellationTokenSource

module CancellationSource =
    let getToken () =
        Effect.Create(fun (provider: #ICancellationProvider) -> provider.Source.Token)

    let cancel () =
        Effect.Create(fun (provider: #ICancellationProvider) -> task { do! provider.Source.CancelAsync() })
#endif


module Runner =
    let createFrom<'a> a =
        { new IProvide<'a> with
            member _.Effect = a
        }
