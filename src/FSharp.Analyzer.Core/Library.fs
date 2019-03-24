namespace FSharp.Analyzer




module Checker =
    open System
    open System.IO
    open FSharp.Compiler.Range
    open FSharp.Compiler.SourceCodeServices
    open FsAutoComplete.Utils
    open FSharp.Analyzers.SDK
    open System.Collections.Concurrent
    open FsAutoComplete
    open FsToolkit.ErrorHandling
    open FsToolkit.ErrorHandling.CE.AsyncResult

    let checker =
        FSharpChecker.Create(
            projectCacheSize = 200,
            keepAllBackgroundResolutions = true,
            keepAssemblyContents = true,
            ImplicitlyStartBackgroundWork = true)

    let private deduplicateReferences (opts: FSharp.Compiler.SourceCodeServices.FSharpProjectOptions, projectFiles, logMap) =
        let projs =
            opts.ReferencedProjects |> Array.map fst

        let references =
            opts.OtherOptions
            |> Array.choose (fun n -> if n.StartsWith "-r:" then Some (n.Substring(3)) else None)
            |> Array.groupBy (Path.GetFullPathSafe)
            |> Array.map (fun (_,lst) ->
                match lst |> Array.tryFind (fun n -> projs |> Array.contains n) with
                | Some s -> s
                | None -> Array.head lst )

        let oos = [|
            yield! (opts.OtherOptions |> Array.filter (fun n -> not (n.StartsWith "-r:")))
            yield! (references |> Array.map (sprintf "-r:%s"))
        |]
        let opts = {opts with OtherOptions = oos}
        opts, projectFiles, logMap

    let private removeDeprecatedArgs (opts: FSharp.Compiler.SourceCodeServices.FSharpProjectOptions, projectFiles, logMap) =
        let oos = opts.OtherOptions |> Array.filter (fun n -> n <> "--times" && n <> "--no-jit-optimize")
        let opts = {opts with OtherOptions = oos}
        opts, projectFiles, logMap

    let private bindExtraOptions (opts: FSharp.Compiler.SourceCodeServices.FSharpProjectOptions, projectFiles, logMap) =
        match opts.ExtraProjectInfo with
        | None ->
            Result.Error (GenericError(opts.ProjectFileName, "expected ExtraProjectInfo after project parsing, was None"))
        | Some x ->
            match x with
            | :? ExtraProjectInfoData as extraInfo ->
                Ok (opts, extraInfo, projectFiles, logMap)
            | x ->
                Result.Error (GenericError(opts.ProjectFileName, (sprintf "expected ExtraProjectInfo after project parsing, was %A" x)))

    type FileParseResult = {
        FileInfo : IO.FileInfo
        Source : string []
        FSharpParseFileResults : FSharpParseFileResults
        FSharpCheckFileResults : FSharpCheckFileResults
    }

    let parseProjectFiles filterOutFiles (options : FSharpProjectOptions, extraProjectInfoData : ExtraProjectInfoData, projectFiles : string list, logMap) = asyncResult {
        let parseProjectFile filePath = async {
            // printfn "Processing %s" filePath
            let filePath = IO.Path.GetFullPath filePath
            let source = IO.File.ReadAllLines filePath
            let! result = checker.ParseAndCheckFileInProject(filePath, 0, String.Join("\n",source), options) |> Async.Catch

            match result with
            | Choice1Of2 (fileResult, FSharpCheckFileAnswer.Succeeded checkFileResult) ->
                let data = {
                    FileInfo = IO.FileInfo filePath
                    Source = source
                    FSharpParseFileResults = fileResult
                    FSharpCheckFileResults = checkFileResult
                }
                return Result.Ok(data)
            | Choice1Of2 (fileResult, FSharpCheckFileAnswer.Aborted ) ->
                let parseErrors = fileResult.Errors |> Array.map (fun e -> e.Message)
                return Result.Error (sprintf "Check aborted Errors: %A" parseErrors)
            | Choice2Of2 e ->
                return Result.Error (string e)
        }

        let! result =
            projectFiles
            |> List.filter(fun (p : string) -> filterOutFiles |> List.exists (p.Contains) |> not)
            |> List.traverseAsyncResultA parseProjectFile
        return result
    }

    let filterOutFiles = [
        "AssemblyInfo.fs"
    ]
    let parseProject projectPath = asyncResult {
        let projectPath = IO.Path.GetFullPath projectPath
        let cache = ConcurrentDictionary()
        let! result =
            ProjectCrackerDotnetSdk.load ignore cache projectPath
            |> Result.map deduplicateReferences
            |> Result.map removeDeprecatedArgs
            |> Result.bind bindExtraOptions
            |> Async.singleton
            |> AsyncResult.mapError (string >> List.singleton)
            |> AsyncResult.bind (parseProjectFiles filterOutFiles)
        // printfn "project crack result: %A" result

        return result
    }

    type AnalyzerResult = {
        FileInfo : FileInfo
        Messages : Message list
    }


    let runAnalyzers (analyzers : Analyzer list) (parserResults : FileParseResult list) =

        analyzers
        |> List.collect(fun analyzer ->
            parserResults
            |> List.choose(fun (parseResult) ->

                match (parseResult.FSharpParseFileResults.ParseTree, parseResult.FSharpCheckFileResults.ImplementationFile) with
                | Some pt, Some tast ->
                    let context  = {
                        FileName = parseResult.FileInfo.Name
                        Content = parseResult.Source
                        ParseTree = pt
                        TypedTree = tast
                        Symbols = parseResult.FSharpCheckFileResults.PartialAssemblySignature.Entities |> Seq.toList
                    }
                    let messages = analyzer context
                    {
                        FileInfo = parseResult.FileInfo
                        Messages = messages
                    }
                    |> Some

                | _ ->
                    None
            )
        )


