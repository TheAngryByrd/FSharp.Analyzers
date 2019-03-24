namespace FSharp.Analyzer.App


module Main =
    open System
    open Argu

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
        elif results.Contains Info then
            AssemblyInfo.formatInfo () |> printfn "%s"
        else
            let command = results.GetSubCommand ()
            match command with
            | Analyze args ->
                args.GetAllResults ()
                |> AnalyzeCommand.Config.FromArgs
                |> AnalyzeCommand.execute
        0
