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
