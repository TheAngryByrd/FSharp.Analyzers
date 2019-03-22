namespace Tests


open Expecto
open FSharp.Analyzer

module Result =
    let getOrFail r =
        match r with
        | Ok x -> x
        | Error s -> failwithf "%A" s

module Tests =
    [<Tests>]
    let tests =
      testList "samples" [
        testCaseAsync "Get typechecker results" <| async {
            let filepath = __SOURCE_DIRECTORY__ + "../../../testData/OptionValue/OptionValue.fsproj"
            let! results = Checker.parseProject filepath
            Expecto.Expect.isOk results "Should be ok"
        }
        testCaseAsync "Load analyzers" <| async {
            let dir = __SOURCE_DIRECTORY__ + "../../../packages/analyzers"
            let analyzers = FsAutoComplete.Analyzers.loadAnalyzers dir
            Expect.equal 1 (analyzers |> List.length) "Should have loaded one analyzer"
        }
        testCaseAsync "Run analyzer" <| async {

            let projectPath = __SOURCE_DIRECTORY__ + "../../../testData/OptionValue/OptionValue.fsproj"
            let! results = Checker.parseProject projectPath
            printfn "results %A" results
            let analyzerDir = __SOURCE_DIRECTORY__ + "../../../packages/analyzers"
            // let analyzers = FsAutoComplete.Analyzers.loadAnalyzers analyzerDir
            let analyzers = [
                SampleAnalyzer.optionValueAnalyzer
            ]
            printfn "analyzers %A" analyzers
            let analyzerResults = results |> Result.getOrFail |> Checker.runAnalyzers analyzers
            printfn "analyzerResults %A" analyzerResults
            Expect.equal 1 (analyzers |> List.length) "Should have loaded one analyzer"
        }

      ]
