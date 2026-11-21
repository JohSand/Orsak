// The recipe, the same for any app: a change is an effect that ends with how a model changes, whether it succeeds or
// fails, and Latest orders what changes read. Depends on neither React nor Elmish.
namespace Todos.Changes

open Orsak

/// An effect that ends with how the model changes, whether it succeeds or fails.
type Change<'r, 'model> = Effect<'r, 'model -> 'model, 'model -> 'model>

/// A value an effect read, with the order it was asked for: an older read answered last doesn't replace a newer one.
type Latest<'a> = { Value: 'a; Order: int }

module Latest =
    let mutable private asked = 0

    /// The order of a read, taken when it starts, within an effect, so later reads have greater orders.
    let next () =
        asked <- asked + 1
        asked

    let empty value = { Value = value; Order = 0 }

    /// The newer of the two.
    let newer (current: Latest<'a>) (read: Latest<'a>) =
        if read.Order > current.Order then read else current

module Change =
    /// A change made at once, such as opening a page, with no effect to run.
    let now (f: 'model -> 'model) : Change<'r, 'model> = Effect.ret f

    /// A change of a part of the model as one of the whole. The part is found again when the change ends: if it is
    /// gone, as when the page it belongs to was left, the change is dropped.
    let lift
        (get: 'whole -> 'part option)
        (set: 'part -> 'whole -> 'whole)
        (change: Change<'r, 'part>)
        : Change<'r, 'whole> =
        let apply f whole =
            match get whole with
            | Some part -> set (f part) whole
            | None -> whole

        change |> Effect.map apply |> Effect.changeError apply

    /// Both changes, one after the other, as one: the second runs if the first succeeds, and the first's is applied
    /// whether the second succeeds or fails.
    let andThen (second: Change<'r, 'model>) (first: Change<'r, 'model>) : Change<'r, 'model> = eff {
        let! f = first
        return! second |> Effect.map (fun g -> f >> g) |> Effect.changeError (fun g -> f >> g)
    }
