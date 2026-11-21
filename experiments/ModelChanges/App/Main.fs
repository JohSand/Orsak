/// The browser: the environment, with the generated proxy, given to the app at its root.
module Todos.Changes.Main

open Browser.Dom
open Feliz
open Orsak.Remoting
open Todos.Shared.Contract
open Todos.Changes.Views

let env =
    ClientEnv(TodoApiProxy.create (RemoteClient(window.location.origin, "/api", JsonFormat.Instance, FetchTransport())))

ReactDOM.createRoot(document.getElementById "root").render (App env)
