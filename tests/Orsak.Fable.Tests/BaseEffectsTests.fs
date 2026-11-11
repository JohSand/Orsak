/// The base effects that the .NET and Fable implementations share.
module Orsak.Fable.Tests.BaseEffectsTests

open System
open System.Threading
open System.Threading.Tasks
open Orsak
open Orsak.Fable.Tests.Runner

let fixedTime = DateTimeOffset(2026, 11, 11, 12, 0, 0, TimeSpan.Zero)

/// A clock, written the same for .NET's TimeProvider and Fable's.
let fixedClock =
    { new TimeProvider() with
        member _.GetUtcNow() = fixedTime
    }

let fixedGuid = Guid.Parse "6f9619ff-8b86-d011-b42d-00cf4fc964ff"

type Env(clock: TimeProvider, source: CancellationTokenSource) =
    interface ITimeProvider with
        member _.Clock = clock

    interface IGuidGenProvider with
        member _.GuidGenerator = { new IGuidGenerator with member _.GenGuid() = fixedGuid }

    interface IRandomProvider with
        member _.Effect =
            { new IRandomGenerator with
                member _.Next((min, _)) = min
                member _.NextDouble() = 0.5
            }

    interface ICancellationProvider with
        member _.Source = source

let run (env: Env) (e: Effect<Env, 'a, string>) = Effect.run env e

let tests = [
    test "Time.utcNow reads the environment's clock" (fun () -> task {
        let! now = run (Env(fixedClock, new CancellationTokenSource())) (Time.utcNow ())
        equal (Ok fixedTime) now
    })

    test "Time.delay waits on the environment's clock" (fun () -> task {
        let! result = run (Env(TimeProvider.System, new CancellationTokenSource())) (eff {
            let! before = Time.utcNow ()
            do! Time.delay (TimeSpan.FromMilliseconds 20.0)
            let! after = Time.utcNow ()
            return after - before
        })

        match result with
        | Ok waited -> isTrue $"waited {waited}" (waited >= TimeSpan.FromMilliseconds 15.0)
        | Error e -> failwith e
    })

    test "GuidGenerator.genGuid uses the environment's generator" (fun () -> task {
        let! id = run (Env(fixedClock, new CancellationTokenSource())) (GuidGenerator.genGuid ())
        equal (Ok fixedGuid) id
    })

    test "GuidGenerator.defaultGen generates different guids" (fun () -> task {
        let gen = GuidGenerator.defaultGen ()
        isTrue "the same guid twice" (gen.GenGuid() <> gen.GenGuid())
    })

    test "Random.next and Random.nextDouble use the environment's generator" (fun () -> task {
        let! result = run (Env(fixedClock, new CancellationTokenSource())) (eff {
            let! n = Random.next 3 7
            let! d = Random.nextDouble ()
            return n, d
        })

        equal (Ok(3, 0.5)) result
    })

    test "DefaultRandom stays within its bounds" (fun () -> task {
        let random = DefaultRandom(System.Random 42) :> IRandomGenerator

        for _ in 1..100 do
            let n = random.Next(1, 7)
            isTrue $"{n} out of bounds" (n >= 1 && n < 7)
            let d = random.NextDouble()
            isTrue $"{d} out of bounds" (d >= 0.0 && d < 1.0)
    })

    test "CancellationSource.cancel cancels the token getToken gets" (fun () -> task {
        let source = new CancellationTokenSource()

        let! result = run (Env(fixedClock, source)) (eff {
            let! token = CancellationSource.getToken ()
            let before = token.IsCancellationRequested
            do! CancellationSource.cancel ()
            return before, token.IsCancellationRequested
        })

        equal (Ok(false, true)) result
    })
]
