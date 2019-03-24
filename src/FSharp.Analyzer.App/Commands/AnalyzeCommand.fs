namespace FSharp.Analyzer.App
open FSharp.Analyzer.Checker
open FsToolkit.ErrorHandling.CE.AsyncResult

module AnalyzeCommand =
    open System
    open FSharp.Analyzer
    open System.IO
    open FsToolkit.ErrorHandling
    open FsToolkit.ErrorHandling.CE.AsyncResult.AsyncResult
    open FsToolkit.ErrorHandling.Operator.Result
    open FsToolkit.ErrorHandling.CE.Result.Result
    open FSharp.Analyzers.SDK
    open Argu

    type AnalyzeArgs =
        | Project of string
        | Analyzers of string
        | Fail_On_Warnings of string list
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Project _ -> "The folder path to the project to analyze"
                | Analyzers _ -> "The folder path where this app should look for analyzers.  Defaults to packages/Analyzers"
                | Fail_On_Warnings _ -> "A list of analyzer codes to fail on. Passing an empty list will fail on any warning"

    type ConfigDTO = {
        Project : string option
        Analyzers : string option
        FailOnWarnings : string list option
    }
    with
        static member Empty = {
            Project = None
            Analyzers = None
            FailOnWarnings = None
        }

        static member FromArgs (args : AnalyzeArgs list) =
            (ConfigDTO.Empty, args)
            ||> List.fold(fun state item ->
                match item with
                | Project p->
                    {state with Project = Some p}
                | Analyzers a->
                    {state with Analyzers = Some a}
                | Fail_On_Warnings w ->
                    {state with FailOnWarnings = Some w}

            )

    type ProjectPath =
    | ProjectPath of FileInfo
    module ProjectPath =
        let create (filePath : string) = result {
            let! filePath =
                filePath
                |> String.ofNullOrWhiteSpace
                |> Result.requireSome "Project path needs to be set"
            let! filePath =
                let fullPath = Path.GetFullPath filePath
                fullPath
                |> File.exists
                |> Result.requireSome (sprintf "Project (%s) could not be found" fullPath)
            let! filePath =
                FileInfo.create filePath
                |> Result.mapError string

            return ProjectPath filePath
        }
        let value (ProjectPath p) = p

    let getDefaultAnalyzerPath () =
        // idealy this should be in the same directory as the paket.depedencies
        "./packages/Analyzers"
        |> Path.GetFullPath

    type Config = {
        Project : ProjectPath
        Analyzers : Analyzer list
        FailOnWarnings : string list option
    }
    with
        static member FromDTO (dto : ConfigDTO) =
            let ctor project analyzers failOn=
                { Project = project; Analyzers = analyzers; FailOnWarnings = failOn}
            let projectPathR =
                dto.Project
                |> Result.requireSome "Project path not specified"
                |> Result.bind ProjectPath.create
            let analyzersR =
                let analyzerPath =
                    dto.Analyzers
                    |> Option.defaultValue (getDefaultAnalyzerPath ())
                analyzerPath
                |> FsAutoComplete.Analyzers.loadAnalyzers
                |> Result.requireNotEmpty' (sprintf "No analyzers found in path %s " analyzerPath)
            let failOnR =
                dto.FailOnWarnings
                |> Result.Ok
            ctor <!> projectPathR <*> analyzersR <*> failOnR

    let writeInColor color s =
        let oldColor = Console.ForegroundColor
        Console.ForegroundColor <- color
        printfn "%s" s
        Console.ForegroundColor <- oldColor
    let errorLogger = writeInColor ConsoleColor.Red
    let error msg = Printf.kprintf errorLogger msg
    let warnLogger = writeInColor ConsoleColor.Yellow
    let warn msg = Printf.kprintf warnLogger msg
    let infoLogger = writeInColor ConsoleColor.Gray
    let info msg = Printf.kprintf infoLogger msg

    let consoleReporter (analyzerResults : AnalyzerResult list) =
        analyzerResults
        |> Seq.iter(fun analyzerResult ->
            analyzerResult.Messages
            |> Seq.iter(fun message ->
                let logger =
                    match message.Severity with
                    | FSharp.Analyzers.SDK.Severity.Error -> error
                    | FSharp.Analyzers.SDK.Severity.Warning -> warn
                    | FSharp.Analyzers.SDK.Severity.Info -> info

                logger "%A: %A %s %s: %s" message.Range message.Severity message.Code message.Type message.Message
            )
        )
        ()

    let determineIfFailure (codeList : string list option) analyzerResults = asyncResult {
        let messages =
            analyzerResults
            |> List.collect(fun ar ->
                ar.Messages
            )
        let getSeverity severity =
            messages
            |> List.filter(fun m -> m.Severity = severity)

        do! getSeverity FSharp.Analyzers.SDK.Severity.Error
            |> Result.requireEmpty "Failing due to analyzer finding errors"
            |> Async.singleton

        match codeList with
        | Some x when x |> List.isEmpty ->
            do! getSeverity FSharp.Analyzers.SDK.Severity.Warning
                |> Result.requireEmpty "Failing due --fail-on-warnings set to fail on any warning found"
                |> Async.singleton
        | Some codes  ->
            let found =
                getSeverity FSharp.Analyzers.SDK.Severity.Warning
                |> List.filter(fun ar -> codes |> Seq.exists ((=) ar.Code))
            let warningsFound=
                found
                |> List.map(fun m -> m.Code)
                |> List.distinct
                |> String.join ","
            do! getSeverity FSharp.Analyzers.SDK.Severity.Warning
                |> List.filter(fun ar -> codes |> Seq.exists ((=) ar.Code))
                |> Result.requireEmpty (sprintf "Failing due to --fail-on-warnings found %s" warningsFound)
                |> Async.singleton
        | None ->
            do! AsyncResult.retn ()

        return ()
    }

    let execute (config : Config) = asyncResult {
        let projectPath = config.Project |> ProjectPath.value
        let! parsedProject =
            Checker.parseProject projectPath.FullName
            |> AsyncResult.mapError (String.join " ")

        let analyzerResults = parsedProject |> Checker.runAnalyzers config.Analyzers
        consoleReporter analyzerResults
        do! determineIfFailure config.FailOnWarnings analyzerResults
    }

    let executeFromArgs (args : AnalyzeArgs list) =
        ConfigDTO.FromArgs args
        |> Config.FromDTO
        |> Async.singleton
        |> AsyncResult.bind execute
