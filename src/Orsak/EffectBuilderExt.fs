namespace Orsak

open System.Runtime.CompilerServices

module Proptype =
    //do i want this?
    type FSharp.Control.TaskBuilderBase with
        member inline this.For((a, b): struct ('a * 'a), [<InlineIfLambda>]f: 'a -> _) =
            this.Delay (fun () -> this.Combine(f a, f b))

        member inline this.For((a, b, c): struct ('a * 'a * 'a), [<InlineIfLambda>]f: 'a -> _) =
            this.Delay (fun () -> this.Combine(this.Combine(f a, f b), f c))

        member inline this.For((a, b, c, d): struct ('a * 'a * 'a * 'a), [<InlineIfLambda>]f: 'a -> _) =
            this.Delay (fun () -> this.Combine (this.Combine (this.Combine(f a, f b), f c), f d))

        member inline this.For((a, b, c, d, e): struct ('a * 'a * 'a * 'a * 'a), [<InlineIfLambda>]f: 'a -> _) =
            this.Delay (fun () -> this.Combine(this.Combine (this.Combine (this.Combine(f a, f b), f c), f d), f e))

        member inline this.For((a, b, c ,d, e, f): struct ('a * 'a * 'a * 'a * 'a * 'a), [<InlineIfLambda>]fn: 'a -> _) =
            this.Delay (fun () -> this.Combine(this.Combine(this.Combine (this.Combine (this.Combine(fn a, fn b), fn c), fn d), fn e), fn f))

        member inline this.For((a, b, c ,d, e, f, g): struct ('a * 'a * 'a * 'a * 'a * 'a * 'a), [<InlineIfLambda>]fn: 'a -> _) =
            this.Delay (fun () -> this.Combine(this.Combine(this.Combine(this.Combine (this.Combine (this.Combine(fn a, fn b), fn c), fn d), fn e), fn f), fn g))

    [<Extension>]
    type TaskHeler =
        [<Extension>]
        static member inline Merge<^a, ^err when ^err: (static member (+): ^err -> ^err -> ^err)>(err: ^err, res: Result<^a, ^err>) = 
            match res with
            | Ok _ -> err
            | Error e -> err + e

namespace Orsak.Extensions
open Orsak

[<AutoOpen>]
module EffectBuilderExt =
    open Proptype
    open System.Threading.Tasks

    type EffBuilder with

        member _.Dummy () =
            task {
                for a in struct (1, 2) do
                    ()            
            }


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
                    EffectDelegate (fun rEnv ->
                        ValueTask<_>(
                            task =
                                task {
                                    let a' = m1.Invoke rEnv
                                    let b' = m2.Invoke rEnv
                                    let c' = m3.Invoke rEnv

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
                    EffectDelegate (fun rEnv ->
                        ValueTask<_>(
                            task =
                                task {
                                    let a' = m1.Invoke rEnv
                                    let b' = m2.Invoke rEnv
                                    let c' = m3.Invoke rEnv
                                    let d' = m4.Invoke rEnv

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
                    EffectDelegate (fun rEnv ->
                        ValueTask<_>(
                            task =
                                task {
                                    let a' = m1.Invoke rEnv
                                    let b' = m2.Invoke rEnv
                                    let c' = m3.Invoke rEnv
                                    let d' = m4.Invoke rEnv
                                    let e' = m5.Invoke rEnv

                                    match! whenAll a' b' c' d' e' with
                                    | Ok a, Ok b, Ok c, Ok d, Ok e -> return Ok(f (a, b, c, d, e))
                                    | Error e, b, c, d, er ->                                         
                                        return Error (e.Merge(b).Merge(c).Merge(d).Merge(er))
                                    | _, Error e, b, c, d -> 
                                        return Error (e.Merge(b).Merge(c).Merge(d))
                                    | _, _, Error e, b, c ->    
                                        return Error (e.Merge(b).Merge(c))
                                    | _, _, _, Error e, b -> 
                                        return Error (e.Merge(b))
                                    | _, _, _, _, Error e -> 
                                        return Error e
                                }
                        ))
                )
            )



        member inline this.MergeSources3(m1, m2, m3) = this.Bind3Return(m1, m2, m3, id)

        member inline this.MergeSources4(m1, m2, m3, m4) = this.Bind4Return(m1, m2, m3, m4, id)

        member inline this.MergeSources5(m1, m2, m3, m4, m5) =
            this.Bind5Return(m1, m2, m3, m4, m5, id)