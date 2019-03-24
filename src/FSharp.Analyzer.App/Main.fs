namespace FSharp.Analyzer.App


module Main =
    open System
    open Argu
    open FsToolkit.ErrorHandling
    open FsToolkit.ErrorHandling.CE.AsyncResult

    type CLIArguments =
        | Info
        | Version
        | [<CliPrefix(CliPrefix.None)>] Analyze of ParseResults<AnalyzeCommand.AnalyzeArgs>
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Info -> "More detailed information about the application"
                | Version -> "Version of application"
                | Analyze _-> "Analyze an fsharp project"

    let executeCommand (command : Async<Result<unit, string>>) =
        let result = command |> Async.RunSynchronously
        match result with
        | Ok () -> 0
        | Error err ->
            eprintfn "%s" err
            1

    [<EntryPoint>]
    let main (argv : string array) =
        let errorHandler = ProcessExiter(colorizer = function ErrorCode.HelpText -> None | _ -> Some ConsoleColor.Red)
        let appname = AssemblyInfo.assembly.Force() |> AssemblyInfo.getName

        let parser =
            ArgumentParser.Create<CLIArguments>(
                programName =  appname.Name,
                errorHandler = errorHandler)
        let results = parser.Parse(argv)
        if results.Contains Version then
            AssemblyInfo.formatVersion () |> printfn "%s"
            0
        elif results.Contains Info then
            AssemblyInfo.formatInfo () |> printfn "%s"
            0
        else
            let command = results.GetSubCommand ()
            match command with
            | Analyze args ->
                args.GetAllResults ()
                |> AnalyzeCommand.executeFromArgs
                |> executeCommand
            | Info | Version -> 0

