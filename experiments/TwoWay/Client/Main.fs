/// The browser: the environment, with the generated proxy, whose calls send the connection's id, given to the app.
module Todos.Changes.Main

open Browser.Dom
open Feliz
open Orsak.Remoting
open Orsak.Remoting.Callbacks.Connection
open Todos.Shared.Contract
open Todos.Changes.Views

let connectionId = ConnectionId()

let env =
    let transport = FetchTransport(connectionId.Headers)
    ClientEnv(TodoApiProxy.create (RemoteClient(window.location.origin, "/api", JsonFormat.Instance, transport)), connectionId)

ReactDOM.createRoot(document.getElementById "root").render (App env)
