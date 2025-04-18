namespace Orsak.Myriad

open System.Text

type IndentingStringBuilder(sb: StringBuilder) =
    member val IndentationLevel = 0 with get, set

    member this.Append(s: string) = sb.Append(s) |> ignore

    member this.AppendStart(s: string) =
        sb.Append(String.replicate this.IndentationLevel " ").Append(s) |> ignore

    member this.AppendLine(s: string) = sb.AppendLine(s) |> ignore

    override this.ToString() : string = sb.ToString()

[<AutoOpen>]
module StringBuilderExtensions =
    type StringBuilder with

        member this.ToIndentingBuilder() = IndentingStringBuilder(this)

type EffectCfg = { name: string; fullName: string }
type EffectAttributeMatches = { effects: EffectCfg array; typeName: string }

type RunnerCfg = { effects: string array; name: string }

/// <summary>
/// All runners in the runners-array must have effects-arrays of equal length, that is the arity
/// </summary>
type RunnerArityCfg = { runners: RunnerCfg array; requiredTieBreakers: int; arity: int }

module Writer =
    let trimI (s: string) = if s.StartsWith("I") then s[1..] else s

    let writeRunner (builderInfo: RunnerCfg) (sb: IndentingStringBuilder) =
        sb.AppendLine($"type {builderInfo.name} = {{")

        for e in builderInfo.effects do
            let effectName = trimI e
            sb.AppendLine($"    {effectName}: {e}")

        sb.AppendLine("} with")

        for e in builderInfo.effects do
            let effectName = trimI e
            sb.AppendLine($"    interface IProvide<{e}> with")
            sb.AppendLine($"        member this.Effect = this.{effectName}")

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
            sb.AppendStart($"static member Extract(_: {extractorName}, e: EffectContext")
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
        sb.AppendLine($"    extract Unchecked.defaultof<{typeName}> a")

    let writeExtractPatterns (cfgs: RunnerArityCfg array) (sb: IndentingStringBuilder) =
        let allEffects =
            cfgs |> Array.collect _.runners |> Array.collect _.effects |> Array.distinct

        sb.AppendLine($"[<AutoOpen>]")
        sb.AppendLine($"module Extractors =")
        sb.IndentationLevel <- sb.IndentationLevel + 4

        for eff in allEffects do
            writeExtractPattern eff sb

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

    let writeRunMethod
        (tieBreakerSeed: int)
        (tieBreakerCount: int)
        (effects: string array)
        (sb: IndentingStringBuilder)
        =
        sb.Append($"member inline _.Run(a")
        writeRunTieBreakerParams tieBreakerSeed tieBreakerCount sb
        sb.AppendLine(") =")
        sb.AppendLine($"    match a with")
        sb.Append("    | ")

        effects
        |> Array.iteri (fun i effectName ->
            let varName = char (97 + i) |> string
            let patternName = $"Extract{trimI effectName}"
            sb.Append($"{patternName}({varName})")

            if i < effects.Length - 1 then
                sb.Append(" & "))

        sb.AppendLine(" -> {")

        effects
        |> Array.iteri (fun i effectName ->
            let varName = char (97 + i) |> string
            sb.AppendLine($"        {trimI effectName} = {varName}"))

        sb.AppendLine("    }")

    let writeRunMethods (info: RunnerArityCfg) (sb: IndentingStringBuilder) =
        let mutable seed = 0

        for b in info.runners do
            writeRunMethod seed info.requiredTieBreakers b.effects sb
            seed <- seed + 1

    let findAritiesForEffect (eff: string) (cfgs: RunnerArityCfg array) =
        let rar = ResizeArray<_>()

        for cfg in cfgs do
            if cfg.runners |> Array.exists (fun r -> r.effects |> Array.contains eff) then
                rar.Add(cfg.arity)

        rar.ToArray()

    let findEffectUsage (cfgs: RunnerArityCfg array) =
        let allEffects =
            cfgs |> Array.collect _.runners |> Array.collect _.effects |> Array.distinct

        let rar = ResizeArray<_>()

        for eff in allEffects do
            rar.Add((eff, findAritiesForEffect eff cfgs))

        rar.ToArray()

    let writeExtractors (cfgs: RunnerArityCfg array) (sb: IndentingStringBuilder) =
        let usages = findEffectUsage cfgs

        for effectName, arity in usages do
            writeExtractor effectName arity sb

    let rec createLayers (seed: int) acc =
        function
        | [] -> List.rev acc |> List.toArray
        | (arity, effects) :: xs ->
            let a =
                effects
                |> List.mapi (fun i effects -> { name = $"Runner{arity}W{i}"; effects = effects })
                |> List.toArray

            let tie = if a.Length = 1 && seed = 0 then 0 else 1
            createLayers (tie + 1) ({ runners = a; requiredTieBreakers = tie; arity = arity } :: acc) xs

    let orderLayers (effects: string array list) =
        effects
        |> List.groupBy _.Length
        |> List.sortByDescending fst
        |> createLayers 0 []

    let writeExpressionBuilder (cfgs: RunnerArityCfg array) (sb: IndentingStringBuilder) =
        let allEffects =
            cfgs |> Array.collect _.runners |> Array.collect _.effects |> Array.distinct
        //create runner builder...
        sb.AppendLine("type EffectRunnerBuilder() =")
        sb.IndentationLevel <- sb.IndentationLevel + 4
        //yield
        sb.AppendLine("member _.Yield(_: unit) = EffectContext()")
        //bonus run
        sb.AppendLine("member inline _.Run(a: EffectContext<'a>) = Runner.createFrom a.A")

        //custom operations
        for eff in allEffects do
            sb.AppendLine(@"[<CustomOperation(""fromEffect"")>]")
            sb.AppendLine($"member inline _.FromEffect(x: GenContext<_, _, _>, p: %s{eff}) = x.Create(p)")

        //run
        for cfg in cfgs do
            writeRunMethods cfg sb

        sb.IndentationLevel <- sb.IndentationLevel - 4

    let writeAll (input: EffectAttributeMatches list) =
        let x = input |> List.map (fun a -> a.effects |> Array.map _.name)
        let sb = StringBuilder().ToIndentingBuilder()
        let runnerLayers = orderLayers x
        //write opens
        //write runner types
        for layer in runnerLayers do
            for runner in layer.runners do
                writeRunner runner sb

        //write extractors
        writeExtractors runnerLayers sb

        //write Extractor active patterns
        writeExtractPatterns runnerLayers sb

        writeExpressionBuilder runnerLayers sb

        //instantiate runner builder
        sb.AppendLine("module Silly =")
        sb.AppendLine("    let mkRunner = EffectRunnerBuilder()")
        ()

    //just the one
    let inline extract a b =
        ((^a or ^b): (static member Extract: ^a * ^b -> 'c) (a, b))
