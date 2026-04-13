namespace Orsak.Tests

open Fantomas.FCS.Text
open Xunit
open System.Text
open Orsak.Myriad
open Myriad.Core
open System
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

    let generateEff s =
        let sb = StringBuilder().ToIndentingBuilder()

        let context = {
            GeneratorContext.ConfigKey = None
            ConfigGetter = fun _ -> [ ]
            InputFilename = ""
            ProjectContext = None
            AdditionalParameters = Map.empty
        }

        let ast, _ = parse s

        match Ast.parseEffects context ast with
        | [ ctx ] ->
            Writer.writeEffectGen ctx sb
            ()
        | _ -> ()

        sb.ToString()

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
            String.Join(
                Environment.NewLine,
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
