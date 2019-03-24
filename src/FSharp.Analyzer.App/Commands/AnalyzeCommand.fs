namespace FSharp.Analyzer.App
open System.IO


module AnalyzeCommand =
    open Argu

    type AnalyzeArgs =
        | Project of string
        | Analyzers of string
    with
        interface IArgParserTemplate with
            member s.Usage =
                match s with
                | Project _ -> "The folder path to the project to analyze"
                | Analyzers _ -> "The folder path where this app should look for analyzers.  Defaults to packages/Analyzers"

    type Config = {
        Project : string option
        Analyzers : string option
    }
    with
        static member Empty = {
            Project = None
            Analyzers = None
        }

        static member FromArgs (args : AnalyzeArgs list) =
            (Config.Empty, args)
            ||> List.fold(fun state item ->
                match item with
                | Project p->
                    {state with Project = Some p}
                | Analyzers a->
                    {state with Analyzers = Some a}
            )




    let execute (config : Config) =
        ()
