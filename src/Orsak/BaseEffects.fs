namespace Orsak

open System
open System.Threading
open Microsoft.Extensions.Caching.Memory


/// <summary>
/// Generates <see cref="T:System.Guid"/>s. An effect interface, so that code creating ids can be run with a
/// deterministic generator in tests.
/// </summary>
type IGuidGenerator =
    /// <summary>Generates a new <see cref="T:System.Guid"/>.</summary>
    abstract GenGuid: unit -> Guid

/// <summary>
/// Provides an <see cref="T:Orsak.IGuidGenerator"/> to effects, as part of an environment.
/// </summary>
type IGuidGenProvider =
    /// <summary>The generator used by <c>GuidGenerator.genGuid</c>.</summary>
    abstract GuidGenerator: IGuidGenerator

/// <summary>
/// Effects for generating <see cref="T:System.Guid"/>s through an <see cref="T:Orsak.IGuidGenProvider"/>.
/// </summary>
module GuidGenerator =
    /// <summary>
    /// An effect that generates a new <see cref="T:System.Guid"/> with the environment's generator.
    /// </summary>
    /// <example>
    /// <code lang="fsharp">
    /// let createOrder () = eff {
    ///     let! id = GuidGenerator.genGuid ()
    ///     return { Id = id; Lines = [] }
    /// }
    /// </code>
    /// </example>
    let genGuid () =
        Effect.Create(fun (provider: #IGuidGenProvider) -> provider.GuidGenerator.GenGuid())

    /// <summary>
    /// An <see cref="T:Orsak.IGuidGenerator"/> that uses <see cref="M:System.Guid.NewGuid"/>.
    /// </summary>
    let defaultGen () =
        { new IGuidGenerator with
            member _.GenGuid() = Guid.NewGuid()
        }

#if NET8_0_OR_GREATER
/// <summary>
/// Provides a <see cref="T:System.TimeProvider"/> to effects, as part of an environment. Using
/// <c>TimeProvider.System</c> in production and a fake time provider in tests makes time-dependent effects testable.
/// </summary>
type ITimeProvider =
    /// <summary>The clock used by <c>Time.utcNow</c>.</summary>
    abstract Clock: TimeProvider

/// <summary>
/// Effects for reading the time through an <see cref="T:Orsak.ITimeProvider"/>.
/// </summary>
module Time =
    /// <summary>
    /// An effect that reads the current UTC time from the environment's clock.
    /// </summary>
    /// <example>
    /// <code lang="fsharp">
    /// let isExpired (expires: DateTimeOffset) = eff {
    ///     let! now = Time.utcNow ()
    ///     return now > expires
    /// }
    /// </code>
    /// </example>
    let utcNow () =
        Effect.Create(fun (provider: #ITimeProvider) -> provider.Clock.GetUtcNow())
#endif

/// <summary>
/// Generates random numbers. An effect interface, so that code using randomness can be run with a seeded or fixed
/// generator in tests.
/// </summary>
type IRandom =
    /// <summary>
    /// A random integer from the first value of the tuple, inclusive, to the second, exclusive.
    /// </summary>
    abstract Next: (int * int) -> int
    /// <summary>A random floating-point number that is greater than or equal to 0.0, and less than 1.0.</summary>
    abstract NextDouble: unit -> float

/// <summary>An alias of <see cref="T:Orsak.IRandom"/>.</summary>
type IRandomGenerator = IRandom

/// <summary>
/// An <see cref="T:Orsak.IRandomGenerator"/> backed by a <see cref="T:System.Random"/>.
/// </summary>
/// <param name="rand">The random number generator to use, e.g. <c>Random.Shared</c>, or a seeded one in tests.</param>
type DefaultRandom(rand: Random) =
    interface IRandomGenerator with
        member _.Next((minValue, maxValue)) = rand.Next(minValue, maxValue)
        member _.NextDouble() = rand.NextDouble()

/// <summary>
/// Provides an <see cref="T:Orsak.IRandomGenerator"/> to effects, as part of an environment.
/// </summary>
type IRandomGeneratorProvider =
    /// <summary>The generator used by the functions in <c>Random</c>.</summary>
    abstract Effect: IRandomGenerator

/// <summary>An alias of <see cref="T:Orsak.IRandomGeneratorProvider"/>.</summary>
type IRandomProvider = IRandomGeneratorProvider

/// <summary>
/// Effects for generating random numbers through an <see cref="T:Orsak.IRandomProvider"/>.
/// </summary>
module Random =
    /// <summary>
    /// An effect that generates a random integer from <paramref name="min"/>, inclusive, to <paramref name="max"/>,
    /// exclusive.
    /// </summary>
    /// <param name="min">The inclusive lower bound.</param>
    /// <param name="max">The exclusive upper bound.</param>
    /// <example>
    /// <code lang="fsharp">
    /// let rollDie () = Random.next 1 7
    /// </code>
    /// </example>
    let next min max =
        Effect.Create(fun (provider: #IRandomProvider) -> provider.Effect.Next(min, max))

    /// <summary>
    /// An effect that generates a random floating-point number that is greater than or equal to 0.0, and less than 1.0.
    /// </summary>
    let nextDouble () =
        Effect.Create(fun (provider: #IRandomProvider) -> provider.Effect.NextDouble())


/// <summary>
/// Provides an <see cref="T:Microsoft.Extensions.Caching.Memory.IMemoryCache"/> to effects, as part of an environment.
/// </summary>
type ICacheProvider =
    /// <summary>The cache used by the functions in <c>MemoryCache</c>.</summary>
    abstract member Cache: IMemoryCache

/// <summary>
/// Effects for caching values in the environment's <see cref="T:Microsoft.Extensions.Caching.Memory.IMemoryCache"/>.
/// </summary>
module MemoryCache =
    let private get () =
        Effect.Create(fun (p: #ICacheProvider) -> p.Cache)

    //gives bad error if you don't bind the effect, not sure how big of an issue it is
    /// <summary>
    /// Gets the value cached under <paramref name="key"/>, or runs the effect created by <paramref name="f"/> to create
    /// and cache it. <paramref name="f"/> is given the new cache entry, to set e.g. its expiration.
    /// </summary>
    /// <param name="key">The key of the cached value.</param>
    /// <param name="f">Creates the effect that produces the value, when it is not cached.</param>
    /// <example>
    /// <code lang="fsharp">
    /// let getCustomer (id: int) =
    ///     MemoryCache.getOrCreate id (fun entry -> eff {
    ///         entry.SetAbsoluteExpiration(TimeSpan.FromMinutes 5.0) |> ignore
    ///         return! Customers.load id
    ///     })
    /// </code>
    /// </example>
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

    /// <summary>
    /// Updates the value cached under <paramref name="key"/> with the effect created by <paramref name="f"/>, or
    /// caches <paramref name="defaultValue"/> if there is none.
    /// </summary>
    /// <param name="key">The key of the cached value.</param>
    /// <param name="defaultValue">The value to cache, and return, when nothing is cached under the key.</param>
    /// <param name="f">Creates the effect that produces the updated value from the cached one.</param>
    /// <returns>The updated value, or <paramref name="defaultValue"/>.</returns>
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
/// <summary>
/// Provides a <see cref="T:System.Threading.CancellationTokenSource"/> to effects, as part of an environment.
/// </summary>
type ICancellationProvider =
    /// <summary>The source used by the functions in <c>CancellationSource</c>.</summary>
    abstract member Source: CancellationTokenSource

/// <summary>
/// Effects for reading and cancelling the environment's <see cref="T:System.Threading.CancellationTokenSource"/>.
/// </summary>
module CancellationSource =
    /// <summary>
    /// An effect that gets the token of the environment's cancellation source, e.g. to pass on to .NET APIs.
    /// </summary>
    /// <example>
    /// <code lang="fsharp">
    /// let download (client: HttpClient) (url: string) = eff {
    ///     let! token = CancellationSource.getToken ()
    ///     return! client.GetStringAsync(url, token)
    /// }
    /// </code>
    /// </example>
    let getToken () =
        Effect.Create(fun (provider: #ICancellationProvider) -> provider.Source.Token)

    /// <summary>
    /// An effect that cancels the environment's cancellation source.
    /// </summary>
    let cancel () =
        Effect.Create(fun (provider: #ICancellationProvider) -> task { do! provider.Source.CancelAsync() })
#endif

open Microsoft.Extensions.Logging

/// <summary>
/// Provides an <see cref="T:Microsoft.Extensions.Logging.ILogger"/> to effects, as part of an environment.
/// </summary>
type ILoggerProvider =
    /// <summary>The logger.</summary>
    abstract member Effect: ILogger
