# React hooks: Orsak effects run by React components

The question: what an adapter for React's hooks looks like, without the Elmish architecture, and how it compares with
the Elmish recipe (`docs/elmish.md`, `samples/Elmish`). The app is the same todo list, with the remoting sample's
contract and server, written with [Feliz](https://fable-hub.github.io/Feliz/) 3.4 and React 19, bundled by Vite.

* **`App/OrsakReact.fs`:** the adapter, about 60 lines: `React.useEffectResult` runs an effect when the component
  mounts and when a dependency changes, and `React.useEffectCallback` gives a function that runs one, as on a click.
  Both keep a `Run<'a, 'e>`: the latest result, whether a run is running, and an exception.
* **`App/Todos.fs`:** the components. The environment comes from a React context, typed as the app's `ClientEnv`,
  given once at the root.
* **`App/Main.fs`:** the environment, with the generated proxy, and the root.
* **`Tests/Program.fs`:** the components, rendered by React in jsdom on Node.js, against a fake `ITodoApi` whose calls
  wait until the test answers them, so the test chooses the order the answers come back in.

## What it shows

* **Stale answers are dropped by the hook,** not by the app. A run whose dependencies changed, or whose component
  unmounted, before it ended is overtaken: its result is dropped. The Elmish sample needs request numbers in its
  model and `update` for this; here the components have none. A test answers two loads in the wrong order, and the
  older is dropped; with the check in the hook removed, it fails.
* **The environment is typed, from a context.** `React.createContext<ClientEnv>` gives the components the app's
  environment, so an effect needing a provider the environment lacks doesn't compile, as with the Elmish recipe.
  `ClientEnv` takes the `ITodoApi`, so a test gives it a fake. A context of `obj`, unboxed to what each effect needs,
  would let components be generic in the environment, as Elmish's `update` is, but a missing provider would fail
  when it runs, and Fable can't test for interfaces.
* **Testing is heavier than with Elmish.** What the app decides, such as loading again after a change, is in the
  components, so a test renders them: Fable, Node.js, jsdom and React's `act`, rather than `update` on .NET with no
  JavaScript. The effects themselves test on .NET as always. An app wanting `update`'s tests keeps its decisions in
  a reducer, which is the Elmish recipe again, with Feliz.UseElmish.
* **The two kinds of failure stay apart,** as in the Elmish recipe: the contract's errors are in `Run.Result`, and an
  exception, such as a server that can't be reached, in `Run.Crashed`.
* **React's `act` and Fable's `task` don't mix directly.** `act` returns a thenable whose `then` returns nothing, so
  a `task` binding it loses everything after; the test makes it a promise first.
* **The bundle:** 334 kB minified, 105 kB gzipped. React alone, bundled the same way, is 222 kB and 69 kB, so Orsak,
  Orsak.Remoting, Fable's library, Feliz and the app add about 112 kB and 36 kB.
* **npm and a bundler need nothing of Orsak.** Fable's output, with Orsak's and Orsak.Remoting's sources, bundles with
  Vite as it is. This is the check the Elmish guide left open, for a view library with npm.

In the browser, against the remoting sample's server: adding, the contract's error, completing, and the refresh every
few seconds work. Pressing Enter while an add runs does nothing, as the Add button is disabled then and a form isn't
submitted by Enter with its submit button disabled; the Elmish sample's form does the same.

## Open

* **Should a change's error be dropped?** A call of `useEffectCallback` overtaken by another drops its result, even an
  error, and doesn't call `onEnded`, as the tests check. That's right for a load, but a change has happened, or failed,
  on the server either way, and its error is worth showing. A callback that keeps every call's error, and only drops
  stale values, may fit changes better.
* **Cancelling on cleanup:** the hook's cleanup is where a run would be cancelled, rather than only dropped, by
  cancelling a source whose token the effect is given, for a member taking a `CancellationToken`. The contract's
  members don't take one, so this isn't tried.
* **React 19's `use` and Suspense:** an alternative to `useEffectResult`, reading a promise of `Effect.run` that a
  parent or cache starts. Not tried.

## Verdict: a weaker fit than Elmish

It works, but Orsak adds less here than to Elmish. With Elmish, Orsak makes commands values that a test runs against
fakes, which Elmish's `Cmd` can't be. With hooks, React already has the rest: a context is its dependency injection,
and the hook does the running. The environment is fixed to the app's by the context, so being generic in it, what
makes Elmish's `update` testable on .NET, goes unused, and the decisions worth testing are in components, which need
jsdom with Orsak or without. Libraries such as TanStack Query do more of what the hook does: caching, retries and
dropping stale answers.

So it's glue for an app already using Orsak, such as with Orsak.Remoting or effects shared with its server, rather
than a reason for a React app to use Orsak.

**Settled:** not a package, and not a guide of its own, but a short section, React hooks without Elmish, on the Elmish
page, with the adapter and the typed context. A change's error should be kept even when another call overtakes it, as
the change has happened, or failed, on the server either way; only stale values are dropped. The spike stops here:
cancelling on cleanup and Suspense wouldn't change the verdict.

## Running it

From this folder, after building the remoting sample's server, which generates the contract's proxy:

```sh
dotnet tool restore
npm install
dotnet build ../../samples/Remoting/Server
dotnet fable Tests -o tests-out && node --import ./Tests/setup-dom.mjs tests-out/Program.js
dotnet fable App -o out
dotnet run --project ../../samples/Remoting/Server -- --urls http://localhost:5288
npx vite
```

The tests print each check. Vite serves the app at http://localhost:5173 and sends its calls to `/api` on to the
server; `npx vite build` bundles it into `dist/`.
