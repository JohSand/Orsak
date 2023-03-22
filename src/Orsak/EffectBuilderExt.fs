namespace Orsak

open System.Runtime.CompilerServices

/// <exclude/>
[<Extension>]
type TaskHelper =
    [<Extension>]
    static member inline Merge< ^a, ^err when ^err: (static member (+): ^err -> ^err -> ^err)>
        (
            err: ^err,
            res: Result< ^a, ^err >
        ) =
        match res with
        | Ok _ -> err
        | Error e -> err + e

namespace Orsak.Extensions

open Orsak

/// <exclude/>
[<AutoOpen>]
module EffectBuilderExt =
    open System.Threading.Tasks

    type EffBuilder with

        member inline this.Bind3Return
            (
                m1: Effect<'Env, 'a, 'Err>,
                m2: Effect<'Env, 'b, 'Err>,
                m3: Effect<'Env, 'c, 'Err>,
                f
            ) =
            let inline whenAll (a: _ ValueTask) (b: _ ValueTask) (c: _ ValueTask) =
                task {
                    let! a' = a
                    let! b' = b
                    let! c' = c
                    return (a', b', c')
                }

            this.ReturnFrom(
                Effect(
                    EffectDelegate(fun rEnv ->
                        ValueTask<_>(
                            task =
                                task {
                                    let a' = m1.Run rEnv
                                    let b' = m2.Run rEnv
                                    let c' = m3.Run rEnv

                                    match! whenAll a' b' c' with
                                    | Ok a, Ok b, Ok c -> return Ok(f (a, b, c))
                                    | Error e, _, _ -> return Error e
                                    | _, Error e, _ -> return Error e
                                    | _, _, Error e -> return Error e
                                }
                        ))
                )
            )

        member inline this.Bind4Return
            (
                m1: Effect<'Env, 'a, 'Err>,
                m2: Effect<'Env, 'b, 'Err>,
                m3: Effect<'Env, 'c, 'Err>,
                m4: Effect<'Env, 'd, 'Err>,
                f
            ) =
            let inline whenAll (a: _ ValueTask) (b: _ ValueTask) (c: _ ValueTask) (d: _ ValueTask) =
                task {
                    let! a' = a
                    let! b' = b
                    let! c' = c
                    let! d' = d
                    return (a', b', c', d')
                }

            this.ReturnFrom(
                Effect(
                    EffectDelegate(fun rEnv ->
                        ValueTask<_>(
                            task =
                                task {
                                    let a' = m1.Run rEnv
                                    let b' = m2.Run rEnv
                                    let c' = m3.Run rEnv
                                    let d' = m4.Run rEnv

                                    match! whenAll a' b' c' d' with
                                    | Ok a, Ok b, Ok c, Ok d -> return Ok(f (a, b, c, d))
                                    | Error e, _, _, _ -> return Error e
                                    | _, Error e, _, _ -> return Error e
                                    | _, _, Error e, _ -> return Error e
                                    | _, _, _, Error e -> return Error e
                                }
                        ))
                )
            )


        member inline this.Bind5Return
            (
                m1: Effect<'Env, 'a, _>,
                m2: Effect<'Env, 'b, _>,
                m3: Effect<'Env, 'c, _>,
                m4: Effect<'Env, 'd, _>,
                m5: Effect<'Env, 'e, _>,
                f
            ) =
            let inline whenAll (a: _ ValueTask) (b: _ ValueTask) (c: _ ValueTask) (d: _ ValueTask) (e: _ ValueTask) =
                task {
                    let! a' = a
                    let! b' = b
                    let! c' = c
                    let! d' = d
                    let! e' = e
                    return (a', b', c', d', e')
                }

            this.ReturnFrom(
                Effect(
                    EffectDelegate(fun rEnv ->
                        ValueTask<_>(
                            task =
                                task {
                                    let a' = m1.Run rEnv
                                    let b' = m2.Run rEnv
                                    let c' = m3.Run rEnv
                                    let d' = m4.Run rEnv
                                    let e' = m5.Run rEnv

                                    match! whenAll a' b' c' d' e' with
                                    | Ok a, Ok b, Ok c, Ok d, Ok e -> return Ok(f (a, b, c, d, e))
                                    | Error e, b, c, d, r -> return Error(e.Merge(b).Merge(c).Merge(d).Merge(r))
                                    | _, Error e, b, c, d -> return Error(e.Merge(b).Merge(c).Merge(d))
                                    | _, _, Error e, b, c -> return Error(e.Merge(b).Merge(c))
                                    | _, _, _, Error e, b -> return Error(e.Merge(b))
                                    | _, _, _, _, Error e -> return Error e
                                }
                        ))
                )
            )



        member inline this.MergeSources3(m1, m2, m3) = this.Bind3Return(m1, m2, m3, id)

        member inline this.MergeSources4(m1, m2, m3, m4) = this.Bind4Return(m1, m2, m3, m4, id)

        member inline this.MergeSources5(m1, m2, m3, m4, m5) =
            this.Bind5Return(m1, m2, m3, m4, m5, id)
