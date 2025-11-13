namespace Orsak.Myriad

open System
open System.Text

type IndentingStringBuilder(sb: StringBuilder) =
    let mutable prepend = true
    member val IndentationLevel = 0 with get, set

    member private this.EnsureIndentation() =
        if prepend then
            sb.Append(String.replicate this.IndentationLevel " ") |> ignore
            prepend <- false

    member this.Append(s: string) =
        this.EnsureIndentation()
        sb.Append(s) |> ignore

    member this.AppendLine(s: string) =
        this.EnsureIndentation()

        sb.AppendLine(s) |> ignore
        prepend <- true

    override this.ToString() : string = sb.ToString()

[<AutoOpen>]
module StringBuilderExtensions =
    type StringBuilder with

        member this.ToIndentingBuilder() = IndentingStringBuilder(this)

type EffectCfg = {
    name: string
    providerName: string
    fullName: string
    providerPropertyName: string
}

type EffectAttributeMatches = { effects: EffectCfg array; nameOverride: string }

type EffectMemberCfg = {
    memberName: string
    argumentCount: int
    isTuple: bool
    isUnit: bool
}

type EffectProviderCfg = {
    providerPropertyName: string
    effectName: string
    providerName: string
    members: EffectMemberCfg list
}

type RunnerCfg = { effects: EffectProviderCfg array; name: string }

type ContextWriterScope = { ns: string; openStatements: string list; effects: EffectAttributeMatches list }

type ContextEffectScope = { ns: string; openStatements: string list; effects: EffectProviderCfg list }
/// <summary>
/// All runners in the runners-array must have effects-arrays of equal length, that is the arity
/// </summary>
type RunnerArityCfg = { runners: RunnerCfg array; requiredTieBreakers: int; arity: int }

module Writer =
    let trimI (s: string) = if s.StartsWith("I") then s[1..] else s

    let toCamelCase (name: string) =
        if name.Length > 0 then
            name[0].ToString().ToLowerInvariant() + name.Substring(1)
        else
            name

    let writeRunner (builderInfo: RunnerCfg) (sb: IndentingStringBuilder) =
        sb.AppendLine($"type {builderInfo.name} = {{")

        for e in builderInfo.effects do
            let effectName = trimI e.effectName
            sb.AppendLine($"    %s{effectName}: %s{e.effectName}")

        sb.AppendLine("} with")

        for e in builderInfo.effects do
            let effectMember = e.providerPropertyName
            let effectName = trimI e.effectName
            let providerName = $"%s{e.providerName}"
            if providerName = "ILoggerFactory" then
                sb.AppendLine($"    interface ILoggerFactory with")
                sb.AppendLine($"        member this.CreateLogger s = this.%s{effectName}.CreateLogger s")
                sb.AppendLine($"        member this.AddProvider s = this.%s{effectName}.AddProvider s")
                sb.AppendLine($"    interface Orsak.ILoggerProvider with")
                sb.AppendLine($"        member this.Effect = this.%s{effectName}.CreateLogger(this.GetType())")
                sb.AppendLine($"    interface System.IDisposable with")
                sb.AppendLine($"        member this.Dispose() = this.%s{effectName}.Dispose()")
            else
                sb.AppendLine($"    interface %s{providerName} with")
                sb.AppendLine($"        member this.%s{effectMember} = this.%s{effectName}")

    let writeTypeParameters (effectName: string) (arity: int) (pos: int) (sb: IndentingStringBuilder) =
        sb.Append("<")

        for i = 1 to arity do
            if i = pos then
                sb.Append(effectName)
            else
                let c = char (96 + i) |> string
                sb.Append($"'{c}")

            if i <> arity then
                sb.Append(", ")

        sb.Append(">")

    let writeExtractorMembers (extractorName: string) (effectName: string) (arity: int) (sb: IndentingStringBuilder) =
        for i = 1 to arity do
            let prop = char (64 + i) |> string
            sb.Append($"static member Extract(_: {extractorName}, e: EffectContext")
            do writeTypeParameters effectName arity i sb
            sb.AppendLine($") = e.%s{prop}")


    let writeExtractor (effectName: string) (arities: int array) (sb: IndentingStringBuilder) =
        let typeName = $"{trimI effectName}Extractor"
        sb.AppendLine($"type {typeName} =")
        sb.IndentationLevel <- sb.IndentationLevel + 4

        for i in arities |> Array.sort do
            writeExtractorMembers typeName effectName i sb

        sb.IndentationLevel <- sb.IndentationLevel - 4

    let writeExtractPattern (effectName: string) (sb: IndentingStringBuilder) =
        let typeName = $"{trimI effectName}Extractor"
        let patternName = $"Extract{trimI effectName}"
        sb.AppendLine($"let inline (|{patternName}|) a : {effectName} =")
        sb.AppendLine($"    Writer.extract Unchecked.defaultof<{typeName}> a")

    let writeExtractPatterns (cfgs: RunnerArityCfg array) (sb: IndentingStringBuilder) =
        let allEffects =
            cfgs |> Array.collect _.runners |> Array.collect _.effects |> Array.distinct

        sb.AppendLine($"[<AutoOpen>]")
        sb.AppendLine($"module Extractors =")
        sb.IndentationLevel <- sb.IndentationLevel + 4

        for eff in allEffects do
            writeExtractPattern eff.effectName sb

        sb.IndentationLevel <- sb.IndentationLevel - 4

    let getTieBreakerType (i: int) =
        match (i % 13) with
        | 0 -> "bool"
        | 1 -> "byte"
        | 2 -> "sbyte"
        | 3 -> "int16"
        | 4 -> "uint16"
        | 5 -> "int"
        | 6 -> "uint"
        | 7 -> "int64"
        | 8 -> "uint16"
        | 9 -> "uint64"
        | 10 -> "float"
        | 11 -> "double"
        | 12 -> "char"
        | _ -> ""

    let writeRunTieBreakerParams (tieBreakerSeed: int) (tieBreakerCount: int) (sb: IndentingStringBuilder) =
        for i = 1 to tieBreakerCount do
            let c = char (96 + i) |> string
            sb.Append($", [<Optional>]_{c}: ")
            sb.Append(getTieBreakerType (i + tieBreakerSeed))

    let writeRunMethod (tieBreakerSeed: int) (tieBreakerCount: int) (effects: EffectProviderCfg array) (sb: IndentingStringBuilder) =
        sb.Append($"member inline _.Run(a")
        writeRunTieBreakerParams tieBreakerSeed tieBreakerCount sb
        sb.AppendLine(") =")
        sb.AppendLine($"    match a with")
        sb.Append("    | ")

        effects
        |> Array.iteri (fun i effectName ->
            let varName = char (97 + i) |> string
            let patternName = $"Extract{trimI effectName.effectName}"
            sb.Append($"{patternName}({varName})")

            if i < effects.Length - 1 then
                sb.Append(" & "))

        sb.AppendLine(" -> {")

        effects
        |> Array.iteri (fun i effectName ->
            let varName = char (97 + i) |> string
            sb.AppendLine($"        {trimI effectName.effectName} = {varName}"))

        sb.AppendLine("    }")

    /// <summary>
    /// Write all runners with given arity
    /// </summary>
    /// <param name="info"></param>
    /// <param name="sb"></param>
    let writeRunMethodLayer (info: RunnerArityCfg) (sb: IndentingStringBuilder) =
        let mutable seed = 0

        sb.IndentationLevel <- sb.IndentationLevel + 4

        for b in info.runners do
            writeRunMethod seed info.requiredTieBreakers b.effects sb
            seed <- seed + 1

        sb.IndentationLevel <- sb.IndentationLevel - 4

    let findAritiesForEffect (eff: string) (cfgs: RunnerArityCfg array) =
        let rar = ResizeArray<_>()

        for cfg in cfgs do
            if
                cfg.runners
                |> Array.exists (fun r -> r.effects |> Array.exists (fun e -> e.effectName = eff))
            then
                rar.Add(cfg.arity)

        rar.ToArray()

    let findEffectUsage (cfgs: RunnerArityCfg array) =
        let allEffects =
            cfgs |> Array.collect _.runners |> Array.collect _.effects |> Array.distinct

        let rar = ResizeArray<_>()

        for eff in allEffects do
            rar.Add((eff, findAritiesForEffect eff.effectName cfgs))

        rar.ToArray()

    let writeExtractors (cfgs: RunnerArityCfg array) (sb: IndentingStringBuilder) =
        let usages = findEffectUsage cfgs

        for e, arity in usages do
            writeExtractor e.effectName arity sb

    let rec createLayers (seed: int) acc =
        function
        | [] -> List.rev acc |> List.toArray
        | (arity, cfgs: RunnerCfg list) :: xs ->
            let a =
                cfgs
                |> List.mapi (fun i cfg ->
                    if cfg.name = "" then
                        { cfg with name = $"Runner{arity}W{i}" }
                    else
                        cfg)

                |> List.toArray

            let tie =
                //if we have more than 1 effect, we need to bump up the seed
                if seed = 0 && a.Length > 1 then
                    1
                else
                    seed
            createLayers (tie + 1) ({ runners = a; requiredTieBreakers = tie; arity = arity } :: acc) xs

    let orderLayers (effects: RunnerCfg list) =
        effects
        |> List.groupBy _.effects.Length
        |> List.sortByDescending fst
        |> createLayers 0 []

    let writeRunMethods ns (cfgs: RunnerArityCfg array) (sb: IndentingStringBuilder) =
        //run
        if cfgs.Length > 1 then
            writeRunMethodLayer cfgs[0] sb

            for cfg in cfgs[1..] |> Array.rev do
                let moduleName = $"Extension{cfg.arity}"
                sb.AppendLine($"namespace {ns}")
                sb.AppendLine($"open Orsak.Myriad.Gen.{ns}")
                sb.AppendLine("open System.Runtime.InteropServices")
                sb.AppendLine("")
                sb.AppendLine("[<AutoOpen>]")
                sb.AppendLine($"module {moduleName} =")
                sb.IndentationLevel <- sb.IndentationLevel + 4
                sb.AppendLine("type EffectRunnerBuilder with")
                writeRunMethodLayer cfg sb
                sb.IndentationLevel <- sb.IndentationLevel - 4
        else
            //1 or 0
            for cfg in cfgs do
                writeRunMethodLayer cfg sb

    let writeExpressionBuilder (ns: string) (cfgs: RunnerArityCfg array) (sb: IndentingStringBuilder) =
        let allEffects =
            cfgs |> Array.collect _.runners |> Array.collect _.effects |> Array.distinct
        //create runner builder...
        sb.AppendLine("type EffectRunnerBuilder() =")
        sb.IndentationLevel <- sb.IndentationLevel + 4
        //yield
        sb.AppendLine("member _.Yield(_: unit) = EffectContext()")

        //bonus run
        for eff in allEffects do
            if eff.effectName <> "ILoggerFactory" then
                sb.AppendLine(
                    $"member inline _.Run(a: EffectContext<{eff.effectName}>) = {{ new {eff.providerName} with member _.{eff.providerPropertyName} = a.A }}"
                )

        //custom operations
        for eff in allEffects do
            sb.AppendLine(@"[<CustomOperation(""fromEffect"")>]")
            sb.AppendLine($"member inline _.FromEffect(x: GenContext<_, _, _>, p: %s{eff.effectName}) = x.Create(p)")

        sb.IndentationLevel <- sb.IndentationLevel - 4

        writeRunMethods ns cfgs sb


    let writeForScope (scope: ContextWriterScope) (sb: IndentingStringBuilder) =

        sb.AppendLine($"namespace {scope.ns}")
        //write opens
        for op in scope.openStatements |> List.rev do
            sb.AppendLine($"open {op}")

        //write runner types
        let x =
            scope.effects
            |> List.map (fun a -> {
                name = a.nameOverride
                effects =
                    a.effects
                    |> Array.map (fun a -> {
                        providerPropertyName = a.providerPropertyName
                        effectName = a.name
                        providerName = a.providerName
                        members = []
                    })
            })

        let runnerLayers = orderLayers x

        for layer in runnerLayers do
            for runner in layer.runners do
                writeRunner runner sb

        sb.AppendLine($"namespace Orsak.Myriad.Gen.{scope.ns}")
        //write opens
        for op in scope.openStatements |> List.rev do
            sb.AppendLine($"open {op}")

        sb.AppendLine("open Orsak.Myriad.Gen")
        sb.AppendLine("open System.Runtime.InteropServices")
        sb.AppendLine($"open {scope.ns}")
        //write extractors
        writeExtractors runnerLayers sb

        //write Extractor active patterns
        writeExtractPatterns runnerLayers sb

        writeExpressionBuilder scope.ns runnerLayers sb

        sb.AppendLine($"namespace {scope.ns}")
        sb.AppendLine($"open Orsak.Myriad.Gen.{scope.ns}")
        //instantiate runner builder
        sb.AppendLine("[<AutoOpen>]")
        sb.AppendLine("module Runner =")
        sb.AppendLine("    let mkRunner = EffectRunnerBuilder()")
        ()

    let writeEffectGen (ctx: ContextEffectScope) (sb: IndentingStringBuilder) =
        sb.AppendLine($"namespace {ctx.ns}")
        sb.AppendLine("open Orsak")


        for e in ctx.effects do
            sb.AppendLine($"type {e.effectName}Provider =")
            sb.AppendLine($"    abstract {e.providerPropertyName}: {e.effectName}")
            sb.AppendLine("")

            sb.AppendLine($"module {e.effectName.TrimStart('I')} =")

            for m in e.members do
                sb.Append($"    let {m.memberName |> toCamelCase} ")
                let parameterCount = m.argumentCount

                if m.isUnit then
                    sb.Append("() ")
                else
                    for i = 1 to parameterCount do
                        let varName = char (96 + i) |> string
                        sb.Append($"{varName} ")

                sb.AppendLine("=")

                let apply =
                    if m.isUnit then
                        "()"
                    elif m.isTuple then
                        let p = String.Join(", ", [ for i in 1..parameterCount -> char (96 + i) |> string ])
                        $"({p})"
                    else
                        let p = String.Join(" ", [ for i in 1..parameterCount -> char (96 + i) |> string ])
                        $" {p}"

                sb.AppendLine $"        Effect.Create(fun (er: #{e.providerName}) -> er.{e.providerPropertyName}.{m.memberName}{apply})"

    //just the one
    let inline extract a b =
        ((^a or ^b): (static member Extract: ^a * ^b -> 'c) (a, b))
