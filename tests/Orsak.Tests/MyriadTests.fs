namespace Orsak.Tests

open Fantomas.FCS.Text
open Xunit
open System.Text
open Orsak.Myriad
open Myriad.Core
open System.Reflection
open System.IO
open VerifyTests
open VerifyXunit
open Microsoft.Extensions.Logging

type AssemblyHook() =
    class
    end

module Ast =
    open Fantomas.FCS

    let parse s =
        let source = SourceText.ofString s
        Parse.parseFile false source []


    let generate s =
        let sb = StringBuilder().ToIndentingBuilder()

        let context = {
            GeneratorContext.ConfigKey = None
            ConfigGetter =
                fun s ->
                    match s with
                    | "TimeProvider" -> [ "ProviderName", box "ITimeProvider"; "ProviderPropertyName", "Clock" ]
                    | "CancellationTokenSource" -> [ "ProviderName", box "ICancellationProvider"; "ProviderPropertyName", "Source"  ]
                    | "IGuidGenerator" -> [ "ProviderName", box "IGuidGenProvider"; "ProviderPropertyName", "GuidGenerator" ]
                    | _ -> [

                      ]
            InputFilename = ""
            ProjectContext = None
            AdditionalParameters = Map.empty
        }

        let ast, _ = parse s

        match Ast.parseRunnerDefn context ast with
        | [ a ] ->
            if a.effects.Length > 0 then
                Writer.writeForScope a sb

        | _ -> ()

        sb.ToString()

    /// The Fantomas settings Myriad would apply to a generated file next to the snapshots,
    /// i.e. those from the repository's .editorconfig.
    let private formatConfig =
        EditorConfig.readConfiguration (Path.Combine(__SOURCE_DIRECTORY__, "TestDataVerified", "Generated.fs"))

    /// Formats generated modules the way Myriad does, with Fantomas and .editorconfig.
    let format modules =
        let file =
            Syntax.ParsedInput.ImplFile(
                Syntax.ParsedImplFileInput(
                    "tmp.fs",
                    false,
                    Syntax.QualifiedNameOfFile(Syntax.Ident("Tmp", Range.range0)),
                    [],
                    [],
                    modules,
                    (false, false),
                    { ConditionalDirectives = []; CodeComments = [] },
                    Set.empty
                )
            )

        Fantomas.Core.CodeFormatter.FormatASTAsync(file, formatConfig) |> Async.RunSynchronously

    let generateEff s =
        let context = {
            GeneratorContext.ConfigKey = None
            ConfigGetter =
                fun s ->
                    match s with
                    | "IStorage" -> [ "ProviderName", box "IStorageAccess"; "ProviderPropertyName", "Storage" ]
                    | "IDotted" -> [ "ProviderName", box "Somewhere.IDottedProvider" ]
                    | _ -> []
            InputFilename = ""
            ProjectContext = None
            AdditionalParameters = Map.empty
        }

        let ast, _ = parse s
        Ast.parseEffects context ast |> EffectSyntax.create |> format

    let generateEnvironment s =
        let context = {
            GeneratorContext.ConfigKey = None
            ConfigGetter =
                fun s ->
                    match s with
                    | "IBeanCounting" -> [ "EffectType", box "Beans.IBeanCounter"; "ProviderPropertyName", "Counter" ]
                    | _ -> []
            InputFilename = ""
            ProjectContext = None
            AdditionalParameters = Map.empty
        }

        let ast, _ = parse s
        Ast.parseEnvironments context ast |> EnvironmentSyntax.create |> format

module MyriadTests =

    [<Theory>]
    [<InlineData(1)>]
    [<InlineData(2)>]
    [<InlineData(3)>]
    [<InlineData(4)>]
    [<InlineData(5)>]
    let ``RunnerGen creates the expected output`` (i: int) = task {
        let assm = typeof<AssemblyHook>.GetTypeInfo().Assembly
        let logger = Unchecked.defaultof<ILogger>
        ignore logger

        use resource =
            assm.GetManifestResourceStream($"Orsak.Tests.TestData.Effects.Myriad.%02i{i}.fsx")

        use reader = new StreamReader(resource)
        let txt = reader.ReadToEnd()
        let result = Ast.generate txt

        let opens =
            System.String.Join(
                System.Environment.NewLine,
                value = [|
                    "#r \"../bin/Debug/net10.0/Orsak.dll\""
                    "#r \"../bin/Debug/net10.0/Orsak.Myriad.dll\""
                    "#r \"../bin/Debug/net10.0/Microsoft.Extensions.Logging.Abstractions.dll\""
                    result
                |]
            )

        let settings = VerifySettings()
        settings.UseDirectory("TestDataVerified")
        settings.UseFileName($"Effects.Myriad.%02i{i}")
        let! _result = Verifier.Verify(target = opens, extension = "fsx", settings = settings)
        ()
    }

    [<Theory>]
    [<InlineData(6)>]
    [<InlineData(7)>]
    [<InlineData(8)>]
    [<InlineData(9)>]
    [<InlineData(10)>]
    [<InlineData(11)>]
    [<InlineData(15)>]
    [<InlineData(16)>]
    let ``EffectGen creates the expected output`` (i: int) = task {
        let assm = typeof<AssemblyHook>.GetTypeInfo().Assembly

        use resource =
            assm.GetManifestResourceStream($"Orsak.Tests.TestData.Effects.Myriad.%02i{i}.fsx")

        use reader = new StreamReader(resource)
        let txt = reader.ReadToEnd()
        let result = Ast.generateEff txt

        let opens =
            System.String.Join(
                System.Environment.NewLine,
                value = [|
                    """namespace Tmp"""
                    txt
                    result
                |]
            )

        let settings = VerifySettings()
        settings.UseDirectory("TestDataVerified")
        settings.UseFileName($"Effects.Myriad.%02i{i}")
        let! _result = Verifier.Verify(target = opens, extension = "fsx", settings = settings)
        ()
    }

    [<Theory>]
    [<InlineData(12)>]
    [<InlineData(13)>]
    [<InlineData(14)>]
    let ``EnvironmentGen creates the expected output`` (i: int) = task {
        let assm = typeof<AssemblyHook>.GetTypeInfo().Assembly

        use resource =
            assm.GetManifestResourceStream($"Orsak.Tests.TestData.Effects.Myriad.%02i{i}.fsx")

        use reader = new StreamReader(resource)
        let txt = reader.ReadToEnd()
        let result = Ast.generateEnvironment txt

        let settings = VerifySettings()
        settings.UseDirectory("TestDataVerified")
        settings.UseFileName($"Effects.Myriad.%02i{i}")

        let! _result =
            Verifier.Verify(target = System.String.Join(System.Environment.NewLine, txt, result), extension = "fsx", settings = settings)

        ()
    }

    [<Theory>]
    [<InlineData("inherit IProvide<IFoo>", "can only inherit non-generic provider interfaces")>]
    [<InlineData("abstract Foo: int", "cannot declare members of its own")>]
    [<InlineData("inherit IFooService", "cannot infer the effect type of 'IFooService'")>]
    [<InlineData("inherit A.IFooProvider\n    inherit B.IFooProvider", "more than one provider whose effect is named 'Foo'")>]
    let ``EnvironmentGen rejects interfaces it cannot implement by convention`` (body: string, expected: string) =
        let source = $"[<GenEnvironment>]\ntype IEnvironment =\n    {body}\n"
        let error = Assert.Throws<exn>(fun () -> Ast.generateEnvironment source |> ignore)
        Assert.Contains(expected, error.Message)

    [<Fact>]
    let ``EnvironmentGen rejects a module with the same name as the declaring module`` () =
        let source = "module Company.App.Environment\n\n[<GenEnvironment>]\ntype IEnvironment =\n    inherit IFooProvider\n"
        let error = Assert.Throws<exn>(fun () -> Ast.generateEnvironment source |> ignore)
        Assert.Contains("would have the same name as the module 'Company.App.Environment'", error.Message)

    [<Fact>]
    let ``EffectGen rejects a qualified ProviderName, since it declares the provider`` () =
        let source = "[<GenEffects>]\ntype IDotted =\n    abstract Foo: unit -> int\n"
        let error = Assert.Throws<exn>(fun () -> Ast.generateEff source |> ignore)
        Assert.Contains("ProviderName 'Somewhere.IDottedProvider' for 'IDotted' must be a plain type name", error.Message)

    /// Both AST generators run over a file whose attributes set Inline = true, their output
    /// concatenated the way Myriad appends it to the end of that file.
    [<Theory>]
    [<InlineData(17)>]
    [<InlineData(18)>]
    let ``Generators produce code to append when Inline is set`` (i: int) = task {
        let assm = typeof<AssemblyHook>.GetTypeInfo().Assembly

        use resource =
            assm.GetManifestResourceStream($"Orsak.Tests.TestData.Effects.Myriad.%02i{i}.fsx")

        use reader = new StreamReader(resource)
        let txt = reader.ReadToEnd()

        let settings = VerifySettings()
        settings.UseDirectory("TestDataVerified")
        settings.UseFileName($"Effects.Myriad.%02i{i}")

        let appended =
            System.String.Join(System.Environment.NewLine, txt, Ast.generateEff txt, Ast.generateEnvironment txt)

        let! _result = Verifier.Verify(target = appended, extension = "fsx", settings = settings)
        ()
    }

    [<Fact>]
    let ``EnvironmentGen allows a module named like the declaring module when Inline is set`` () =
        let source =
            "module Company.App.Environment\n\n[<GenEnvironment(Inline = true)>]\ntype IEnvironment =\n    inherit IFooProvider\n"

        let generated = Ast.generateEnvironment source
        Assert.Contains("module Environment =", generated)
        Assert.DoesNotContain("namespace", generated)

    [<Fact>]
    let ``Inline is rejected outside the last namespace of a file`` () =
        let source =
            "namespace A\n\n[<Orsak.Myriad.GenEffects(Inline = true)>]\ntype IFoo =\n    abstract Foo: unit -> int\n\nnamespace B\n\ntype Other = int\n"

        let error = Assert.Throws<exn>(fun () -> Ast.generateEff source |> ignore)
        Assert.Contains("GenEffects: Inline = true is only supported in the last namespace or module of a file", error.Message)
