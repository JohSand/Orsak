/// What Orsak.Remoting would provide for calls from the server to the client: they go down a stream, one per connection,
/// each a member of the client's interface and its arguments, written by the format, as a call's are. Same on .NET and
/// Fable.
namespace Orsak.Remoting.Callbacks

open System
open Orsak.Remoting

/// An item of a connection's stream.
type Callback =
    /// The first item: the connection's id, which the client sends with its calls, so the server knows who called.
    | Connected of id: string
    /// A call of a member of the client's interface, with its arguments written by the format.
    | Call of memberName: string * arguments: byte array
    /// Sent when nothing else has been for a while, so a client hearing nothing for longer knows the connection is
    /// dead, though it wasn't closed, as when a proxy keeps the client's side open after the server has gone.
    | Alive

/// A member of the client's interface, with the codec of its arguments, made once: what a generator writes, for the
/// server's sender and the client's receiver.
type CallbackMember(format: IWireFormat, memberName: string, argTypes: Type array) =
    let codec = format.ArgumentsCodecFor argTypes

    member _.Name = memberName

    /// The callback calling the member with the arguments.
    member _.Call(arguments: obj array) = Call(memberName, codec.Encode arguments)

    /// The arguments of a callback calling the member.
    member _.Arguments(arguments: byte array) = codec.Decode arguments
