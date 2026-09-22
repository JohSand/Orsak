open System
open Orsak
open Orsak.Myriad

// Qualified providers keep their prefix, Orsak's own providers resolve to their real effect types,
// and IBeanCounting is resolved through config: EffectType = "Beans.IBeanCounter", ProviderPropertyName = "Counter".
[<GenEnvironment>]
type IAppEnvironment =
    inherit Buttons.IButtonPusherProvider
    inherit IGuidGenProvider
    inherit ITimeProvider
    inherit ICacheProvider
    inherit IBeanCounting

namespace Tmp

open System
open Orsak
open Orsak.Myriad

module AppEnvironment =
    let create
        (effects:
            {|
                ButtonPusher: Buttons.IButtonPusher
                GuidGenerator: Orsak.IGuidGenerator
                TimeProvider: System.TimeProvider
                MemoryCache: Microsoft.Extensions.Caching.Memory.IMemoryCache
                BeanCounter: Beans.IBeanCounter
            |})
        : IAppEnvironment =
        { new IAppEnvironment

          interface Buttons.IButtonPusherProvider with
              member _.Effect = effects.ButtonPusher
          interface IGuidGenProvider with
              member _.GuidGenerator = effects.GuidGenerator
          interface ITimeProvider with
              member _.Clock = effects.TimeProvider
          interface ICacheProvider with
              member _.Cache = effects.MemoryCache
          interface IBeanCounting with
              member _.Counter = effects.BeanCounter
        }
