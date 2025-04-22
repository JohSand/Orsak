module AstParserTests

open Fantomas.Core
open Xunit
open Orsak.Myriad

let fromText (s: string) =
    CodeFormatter.ParseAsync(false, s)

let text = "\
namespace Orsak.Myriad.Showcase

open Orsak
open Orsak.Myriad
open System.Threading.Tasks

[<GenEffects>]
type IFace =
    abstract PushButton: 'a -> Task<unit>
"

[<Fact>]
let myTest () = async {
    let! a = fromText text
    let ast, _ = Assert.Single(a)

    let a = Ast.parseEffects ast
    return ()
}
