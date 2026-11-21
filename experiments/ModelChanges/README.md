# Model changes: effects that end with how the model changes

The question: can a front end keep what makes the Elmish recipe attractive, commands that a test runs against fakes on
.NET, without Elmish's ceremony: a message for each result, an `update` matching them, and request numbers for answers
out of order. The app is the same todo list, with the remoting sample's contract and server, drawn with Feliz and
React 19, bundled by Vite.

The idea: a change is an effect that ends with a function from the model to the model, whether it succeeds or fails.

```fsharp
type Change<'r, 'model> = Effect<'r, 'model -> 'model, 'model -> 'model>

let load () : Change<'r, Model> = eff {
    let order = Latest.next ()
    let! todos = TodoApi.list ()
    return fun model -> { model with Todos = Latest.newer model.Todos { Value = todos; Order = order } }
}
```

The view runs one, `run (add title)`, and the adapter applies what it ends with to the model as it is then.

* **`App/Change.fs`:** the recipe, the same for any app: `Change`, `Latest`, and `Change.lift`, `now` and `andThen`.
* **`App/TodoList.fs`, `App/Detail.fs`:** two parts of the app, the list and a todo's page, each a model and its
  changes, knowing nothing of the rest. **`App/AppModel.fs`:** the whole, with the parts' changes lifted into it, and
  those changing more than one part. None of these use React or Elmish, so they compile and are tested on .NET.
* **`App/OrsakReact.fs`:** the adapter, 22 lines: `React.useChanges` gives the model, `run`, and how many changes are
  running. `run` applies a change's function with React's updater, so changes ending in any order each apply to the
  model as the others left it.
* **`App/Views.fs`:** the views, which call the changes, with no messages in between. Each part's view is given its
  part of the model and a `run` for its part's changes.
* **`Tests/Program.fs`:** the changes, run against a fake `ITodoApi` on .NET, with what they end with applied to a model.

## What it shows

* **No messages, no `update`, no `Cmd`.** The Elmish sample's `Msg` has seven cases, five of them only to carry results
  back, and `update` matches each. Here what the user does calls a change, and a change's result is already how the
  model changes. The app's logic is 39 lines to the Elmish sample's 50, the adapter 22 to 34, and there are fewer
  things to learn: a change, and `Latest`.
* **Tests stay on .NET, without a browser.** A test runs a change against the fake and applies what it ends with:
  `let! add = run env (add "milk")`, then checks `add initial`. As with the Elmish recipe, and unlike the React hooks
  spike, which needed jsdom. The changes stay generic in the environment.
* **Answers out of order are data, not a protocol.** A value a change reads is a `Latest<'a>`, with the order it was
  asked for, taken when the change starts; applying it keeps whichever is newer. An older load answered last is
  ignored, in either order, as a test checks, with no request numbers in messages and no busy flag. With the ordering
  turned off, that test fails.
* **The same goes for the error shown, which the browser found.** At first a change's success cleared the error, so of
  three quick adds, the empty title's error was cleared by an earlier add ending after it. The error is a
  `Latest<string option>` now, the outcome of the change started last, whatever order they end in; two tests check
  both orders. It is the one place the spike needed a second look: a field that more than one change sets is ordered
  by when the changes started, or the last to end wins.
* **No button needs disabling.** Nothing goes wrong when changes overlap, so the Add button stays enabled, and quick
  submissions with Enter are all added; the React hooks spike and the Elmish sample disable it while busy, which made
  Enter do nothing.
* **The adapter isn't tied to React.** It is the model as state and a `run` that applies functions: a Solid or Sutil
  store's update, or a single `Apply` message in Elmish, would do the same. Only React's was tried.
* **The bundle** is as the React hooks spike's: 335 kB, 105 kB gzipped.

In the browser, against the remoting sample's server: adding, quick adds, the contract's error, completing, and the
refresh every few seconds work.

## Second round: a larger model

The question: does it hold up once the model has parts, where Elmish maps a child's messages into the parent's. The
app has two now: the list, and a todo's page, opened from it, with a model of its own.

* **A part's changes are lifted into the whole,** with `Change.lift get set`, which applies a change of the part to
  the part found in the whole when it ends. A part's view is given its part of the model and `AppModel.list >> run`:
  it knows nothing of the rest. This is Elmish's mapping of a child's messages and commands, in one function, for any
  part.
* **A part that may be gone is found again, and its changes dropped if it is.** `get` gives an option: a change of
  todo 2's page, ending once the page was left or todo 3's opened, isn't applied. `Latest` alone covers one order, an
  older page's load ending after the newer page's; the page's identity is needed for the other, as a test found: with
  only `Latest`, todo 3's page showed todo 2 until its own load ended. The identity is the todo's id here, so leaving a
  page and opening it again lets the first opening's load fill the second, which is harmless for a load.
* **A change of more than one part is the whole's,** composed from the parts': completing on the page is the page's
  `complete`, then the list's `load`, with `Change.andThen`. The page's view is given it as a callback, as Elmish's
  child would send an external message.
* **A change applies once, when it ends.** Opening a page is two changes, `openPage`, at once, then `loadPage`; one
  change can't show the page before its effect has run, where Elmish's `update` returns the model and the command
  together. The view calls `run` twice.
* **Cost:** `Change.fs`, the part any app copies, is 28 lines with `lift`, `now` and `andThen`; `AppModel.fs`, the
  composition of this app, 21. 18 tests on .NET, 8 of them of the composition; with the identity check removed, one
  fails.

In the browser: opening a todo's page, completing it there, and going back to the list, which shows it done.

## What it gives up

* **No list of everything that can happen.** Elmish's `Msg` is a log of what happened, for its debugger or for
  logging; changes are functions, which can't be shown or replayed.
* **Ordering is the app's to remember.** A field more than one change sets needs `Latest`; forgetting it is the bug
  above, and nothing warns. Elmish's request numbers are as easy to forget, but `update` puts them in one place.
* **Showing something at once, and more once an effect ends, is two changes,** where Elmish's `update` is one step.
* **`Latest.next` and `crashed` aren't pure:** they take the next number of a counter. Tests don't notice, as orders
  only need to grow.

## Open

* **Cancelling a load a newer one has overtaken:** `Latest` drops its answer, but the call runs to its end on the
  server, as with Elmish's request numbers.
* **Subscriptions:** the refresh is a timer in the view calling `run (load ())`; a server's stream would call `run`
  for each item in the same way. Not tried.

## As a feature

A recipe again, not a package: the adapter is 22 lines, and `Change.fs` 28, with lifting parts. It's the lighter story for an API already
using Orsak that wants a front end: changes are effects of the contract tested like the server's, with Elmish for
apps that want its structure. A docs page would show both, this first.

## Running it

From this folder, after building the remoting sample's server, which generates the contract's proxy:

```sh
dotnet tool restore
npm install
dotnet run --project Tests
dotnet build ../../samples/Remoting/Server
dotnet fable App -o out
dotnet run --project ../../samples/Remoting/Server -- --urls http://localhost:5288
npx vite
```

The tests print each check. Vite serves the app at http://localhost:5173 and sends its calls to `/api` on to the
server.
