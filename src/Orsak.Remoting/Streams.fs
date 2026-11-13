namespace Orsak.Remoting

open System
open System.Threading
open System.Threading.Tasks

#if FABLE_COMPILER
/// <summary>
/// Reads a <see cref="T:Orsak.Remoting.RemoteStream`1"/>, one item at a time: Fable's stand-in for
/// <c>IAsyncEnumerator</c>, whose members return a <c>ValueTask</c>, which Fable doesn't have. Its members have the
/// same names, so code reading a stream in a <c>task</c> compiles on both.
/// </summary>
type IRemoteStreamEnumerator<'T> =
    /// <summary>The item read last.</summary>
    abstract Current: 'T
    /// <summary>Reads the next item, if there is one.</summary>
    abstract MoveNextAsync: unit -> Task<bool>
    /// <summary>Stops reading, ending the call if it hasn't ended.</summary>
    abstract DisposeAsync: unit -> Task

/// <summary>
/// The items a remote member streams, as they come, read with <c>GetAsyncEnumerator</c>, or with the functions of
/// <c>RemoteStream</c>. Each reading is a call of its own. On .NET, an <c>IAsyncEnumerable</c>; on Fable, which has
/// none, this, with members of the same names.
/// </summary>
type RemoteStream<'T> =
    /// <summary>Starts reading the stream, which the token cancels.</summary>
    abstract GetAsyncEnumerator: cancellationToken: CancellationToken -> IRemoteStreamEnumerator<'T>
#else
/// <summary>
/// The items a remote member streams, as they come: an <c>IAsyncEnumerable</c>, so a server returns one as usual,
/// such as an effect sequence run in its environment. Each reading is a call of its own. On Fable, which has no
/// <c>IAsyncEnumerable</c>, an interface of Orsak.Remoting's own, with members of the same names.
/// </summary>
type RemoteStream<'T> = System.Collections.Generic.IAsyncEnumerable<'T>
#endif

/// <summary>
/// The body of a response, read as it comes, in chunks, such as a stream's frames.
/// </summary>
type IRemoteBody =
    inherit IDisposable
    /// <summary>The next chunk of the body, empty once it has all been read.</summary>
    abstract Read: unit -> Task<byte array>

/// <summary>
/// A frame of a streamed response: an item, the end of the stream, or its failure on the server.
/// </summary>
type Frame =
    | Item of byte array
    | End
    | Failed of message: string

/// <summary>
/// The frames of a streamed response: a byte of their kind, the length of their content, as 4 bytes, the most
/// significant first, and the content. An item's content is the item, written by the member's format; a failure's is
/// a message, in UTF-8. The end has none, so a response cut short is told from one that ended.
/// </summary>
module Frames =
    [<Literal>]
    let private ItemKind = 1uy

    [<Literal>]
    let private EndKind = 2uy

    [<Literal>]
    let private FailedKind = 3uy

    let private frame (kind: byte) (content: byte array) =
        let n = content.Length

        Array.append
            [| kind; byte ((n >>> 24) &&& 0xff); byte ((n >>> 16) &&& 0xff); byte ((n >>> 8) &&& 0xff); byte (n &&& 0xff) |]
            content

    /// <summary>The bytes of a frame.</summary>
    let write (f: Frame) =
        match f with
        | Item content -> frame ItemKind content
        | End -> frame EndKind [||]
        | Failed message -> frame FailedKind (Text.Encoding.UTF8.GetBytes message)

    /// <summary>
    /// Reads the frames of a body, as they come: <c>None</c> once the body ends, which it should only after the end
    /// frame.
    /// </summary>
    type Reader(body: IRemoteBody) =
        let mutable pending: byte array = [||]

        let length (bytes: byte array) =
            (int bytes[1] <<< 24) ||| (int bytes[2] <<< 16) ||| (int bytes[3] <<< 8) ||| int bytes[4]

        /// <summary>The next frame, or <c>None</c> once the body has ended.</summary>
        member this.Next() : Task<Frame option> = task {
            if pending.Length >= 5 && pending.Length >= 5 + length pending then
                let n = length pending
                let kind = pending[0]
                let content = pending[5 .. 4 + n]
                pending <- pending[5 + n ..]

                return
                    match kind with
                    | ItemKind -> Some(Item content)
                    | EndKind -> Some End
                    | FailedKind -> Some(Failed(Text.Encoding.UTF8.GetString content))
                    | k -> failwith $"A streamed response had a frame of an unknown kind, {k}."
            else
                let! chunk = body.Read()

                if chunk.Length = 0 then
                    if pending.Length > 0 then
                        failwith "A streamed response ended within a frame."

                    return None
                else
                    pending <- Array.append pending chunk
                    return! this.Next()
        }

/// <summary>
/// Reads a <see cref="T:Orsak.Remoting.RemoteStream`1"/> the same way on .NET and Fable.
/// </summary>
[<RequireQualifiedAccess; CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module RemoteStream =
    /// <summary>
    /// Calls <paramref name="f"/> with each item of the stream, as it comes, until it ends or the token is cancelled.
    /// </summary>
    let iterTask (cancellationToken: CancellationToken) (f: 'T -> Task<unit>) (stream: RemoteStream<'T>) : Task<unit> = task {
        let e = stream.GetAsyncEnumerator(cancellationToken)

        // disposed after a failure too, awaited, which a finally in a task can't
        let! failure =
            task {
                try
                    let mutable more = true

                    while more do
                        let! next = e.MoveNextAsync()

                        if next then
                            do! f e.Current
                        else
                            more <- false

                    return None
                with ex ->
                    return Some ex
            }

        do! e.DisposeAsync()

        match failure with
        | Some ex -> return raise ex
        | None -> ()
    }

    /// <summary>Calls <paramref name="f"/> with each item of the stream, as it comes, until it ends.</summary>
    let iter (f: 'T -> unit) (stream: RemoteStream<'T>) : Task<unit> =
        iterTask CancellationToken.None (fun a -> task { f a }) stream

    /// <summary>The items of the stream, once it has ended.</summary>
    let toList (stream: RemoteStream<'T>) : Task<'T list> = task {
        let items = ResizeArray<'T>()
        do! iter items.Add stream
        return List.ofSeq items
    }
