namespace FsAutoComplete

open System
open System.IO

open FSharp.Compiler.SourceCodeServices


module Utils =
    open System
    open System.IO
    type Path with
        static member GetFullPathSafe (path: string) =
            try Path.GetFullPath path
            with _ -> path

        static member GetFileNameSafe (path: string) =
            try Path.GetFileName path
            with _ -> path

    module String =
        let split (splitter: char) (s: string) = s.Split([| splitter |], StringSplitOptions.RemoveEmptyEntries) |> List.ofArray

    let inline combinePaths path1 (path2 : string) = Path.Combine(path1, path2.TrimStart [| '\\'; '/' |])

    let inline (</>) path1 path2 = combinePaths path1 path2

    let runningOnMono =
      try not << isNull <| Type.GetType "Mono.Runtime"
      with _ -> false

    let isWindows =
    #if NETSTANDARD1_6
        System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform(
            System.Runtime.InteropServices.OSPlatform.Windows)
    #else
        match System.Environment.OSVersion.Platform with
        | PlatformID.Win32NT | PlatformID.Win32S | PlatformID.Win32Windows | PlatformID.WinCE -> true
        | _ -> false
    #endif
    let chooseByPrefix (prefix: string) (s: string) =
        if s.StartsWith(prefix) then Some (s.Substring(prefix.Length))
        else None

    let chooseByPrefix2 prefixes (s: string) =
        prefixes
        |> List.tryPick (fun prefix -> chooseByPrefix prefix s)

    let splitByPrefix (prefix: string) (s: string) =
        if s.StartsWith(prefix) then Some (prefix, s.Substring(prefix.Length))
        else None

    let splitByPrefix2 prefixes (s: string) =
        prefixes
        |> List.tryPick (fun prefix -> splitByPrefix prefix s)

    let runProcess (log: string -> unit) (workingDir: string) (exePath: string) (args: string) =
        let psi = System.Diagnostics.ProcessStartInfo()
        psi.FileName <- exePath
        psi.WorkingDirectory <- workingDir
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.Arguments <- args
        psi.CreateNoWindow <- true
        psi.UseShellExecute <- false

        use p = new System.Diagnostics.Process()
        p.StartInfo <- psi

        p.OutputDataReceived.Add(fun ea -> log (ea.Data))

        p.ErrorDataReceived.Add(fun ea -> log (ea.Data))

        // printfn "running: %s %s" psi.FileName psi.Arguments

        p.Start() |> ignore
        p.BeginOutputReadLine()
        p.BeginErrorReadLine()
        p.WaitForExit()

        let exitCode = p.ExitCode

        exitCode, (workingDir, exePath, args)


module FscArguments =
    open Utils
    let outType rsp =
        match List.tryPick (chooseByPrefix "--target:") rsp with
        | Some "library" -> ProjectOutputType.Library
        | Some "exe" -> ProjectOutputType.Exe
        | Some v -> ProjectOutputType.Custom v
        | None -> ProjectOutputType.Exe // default if arg is not passed to fsc

    let private outputFileArg = ["--out:"; "-o:"]

    let private makeAbs projDir (f: string) =
      if Path.IsPathRooted f then f else Path.Combine(projDir, f)

    let outputFile projDir rsp =
      rsp
      |> List.tryPick (chooseByPrefix2 outputFileArg)
      |> Option.map (makeAbs projDir)

    let isCompileFile (s:string) =
      s.EndsWith(".fs") || s.EndsWith (".fsi")

    let compileFiles =
      //TODO filter the one without initial -
      List.filter isCompileFile

    let references =
      //TODO valid also --reference:
      List.choose (chooseByPrefix "-r:")

    let useFullPaths projDir (s: string) =
        match s |> splitByPrefix2 outputFileArg with
        | Some (prefix, v) ->
            prefix + (v |> makeAbs projDir)
        | None ->
            if isCompileFile s then
                s |> makeAbs projDir |> Path.GetFullPath
            else
                s

module DotnetProjInfoInspectHelpers =

  let msbuildPropBool (s: string) =
    match s.Trim() with
    | "" -> None
    | Dotnet.ProjInfo.Inspect.MSBuild.ConditionEquals "True" -> Some true
    | _ -> Some false

  let msbuildPropStringList (s: string) =
    match s.Trim() with
    | "" -> []
    | Dotnet.ProjInfo.Inspect.MSBuild.StringList list  -> list
    | _ -> []

module MSBuildPrj = Dotnet.ProjInfo.Inspect


exception ProjectInspectException of GetProjectOptionsErrors

type NavigateProjectSM =
    | NoCrossTargeting of NoCrossTargetingData
    | CrossTargeting of string list
and NoCrossTargetingData = { FscArgs: string list; P2PRefs: MSBuildPrj.ResolvedP2PRefsInfo list; Properties: Map<string,string> }

module MSBuildKnownProperties =
    let TargetFramework = "TargetFramework"

module ProjectCrackerDotnetSdk =

  open DotnetProjInfoInspectHelpers

  let msbuildPropProjectOutputType (s: string) =
    match s.Trim() with
    | MSBuildPrj.MSBuild.ConditionEquals "Exe" -> ProjectOutputType.Exe
    | MSBuildPrj.MSBuild.ConditionEquals "Library" -> ProjectOutputType.Library
    | x -> ProjectOutputType.Custom x

  let getExtraInfo targetPath props =
    let msbuildPropBool prop =
        props |> Map.tryFind prop |> Option.bind msbuildPropBool
    let msbuildPropStringList prop =
        props |> Map.tryFind prop |> Option.map msbuildPropStringList
    let msbuildPropString prop =
        props |> Map.tryFind prop

    { ProjectSdkTypeDotnetSdk.IsTestProject = msbuildPropBool "IsTestProject" |> Option.defaultValue false
      Configuration = msbuildPropString "Configuration" |> Option.defaultValue ""
      IsPackable = msbuildPropBool "IsPackable" |> Option.defaultValue false
      TargetFramework = msbuildPropString MSBuildKnownProperties.TargetFramework |> Option.defaultValue ""
      TargetFrameworkIdentifier = msbuildPropString "TargetFrameworkIdentifier" |> Option.defaultValue ""
      TargetFrameworkVersion = msbuildPropString "TargetFrameworkVersion" |> Option.defaultValue ""
      TargetPath = targetPath

      MSBuildAllProjects = msbuildPropStringList "MSBuildAllProjects" |> Option.defaultValue []
      MSBuildToolsVersion = msbuildPropString "MSBuildToolsVersion" |> Option.defaultValue ""

      ProjectAssetsFile = msbuildPropString "ProjectAssetsFile" |> Option.defaultValue ""
      RestoreSuccess = msbuildPropBool "RestoreSuccess" |> Option.defaultValue false

      Configurations = msbuildPropStringList "Configurations" |> Option.defaultValue []
      TargetFrameworks = msbuildPropStringList "TargetFrameworks" |> Option.defaultValue []

      RunArguments = msbuildPropString "RunArguments"
      RunCommand = msbuildPropString "RunCommand"

      IsPublishable = msbuildPropBool "IsPublishable" }

  type private ProjectParsingSdk = DotnetSdk | VerboseSdk

  type ParsedProject = string * FSharpProjectOptions * ((string * string) list)
  type ParsedProjectCache = Collections.Concurrent.ConcurrentDictionary<string, ParsedProject>

  let private getProjectOptionsFromProjectFile notifyState (cache: ParsedProjectCache) parseAsSdk (file : string) =

    let rec projInfoOf additionalMSBuildProps (file: string) : ParsedProject =
        let projDir = Path.GetDirectoryName file

        notifyState (WorkspaceProjectState.Loading file)

        match parseAsSdk with
        | ProjectParsingSdk.DotnetSdk ->
            let projectAssetsJsonPath = Path.Combine(projDir, "obj", "project.assets.json")
            if not(File.Exists(projectAssetsJsonPath)) then
                raise (ProjectInspectException (ProjectNotRestored file))
        | ProjectParsingSdk.VerboseSdk ->
            ()

        let getFscArgs =
            match parseAsSdk with
            | ProjectParsingSdk.DotnetSdk ->
                Dotnet.ProjInfo.Inspect.getFscArgs
            | ProjectParsingSdk.VerboseSdk ->
                let asFscArgs props =
                    let fsc = Microsoft.FSharp.Build.Fsc()
                    Dotnet.ProjInfo.FakeMsbuildTasks.getResponseFileFromTask props fsc
                Dotnet.ProjInfo.Inspect.getFscArgsOldSdk (asFscArgs >> Ok)

        let getP2PRefs = Dotnet.ProjInfo.Inspect.getResolvedP2PRefs
        let additionalInfo = //needed for extra
            [ "OutputType"
              "IsTestProject"
              "Configuration"
              "IsPackable"
              MSBuildKnownProperties.TargetFramework
              "TargetFrameworkIdentifier"
              "TargetFrameworkVersion"
              "MSBuildAllProjects"
              "ProjectAssetsFile"
              "RestoreSuccess"
              "Configurations"
              "TargetFrameworks"
              "RunArguments"
              "RunCommand"
              "IsPublishable"
            ]
        let gp () = Dotnet.ProjInfo.Inspect.getProperties (["TargetPath"; "IsCrossTargetingBuild"; "TargetFrameworks"] @ additionalInfo)

        let results, log =
            let loggedMessages = System.Collections.Concurrent.ConcurrentQueue<string>()

            let runCmd exePath args = Utils.runProcess loggedMessages.Enqueue projDir exePath (args |> String.concat " ")

            let msbuildExec =
                let msbuildPath =
                    match parseAsSdk with
                    | ProjectParsingSdk.DotnetSdk ->
                        Dotnet.ProjInfo.Inspect.MSBuildExePath.DotnetMsbuild "dotnet"
                    | ProjectParsingSdk.VerboseSdk ->
                        Dotnet.ProjInfo.Inspect.MSBuildExePath.Path "msbuild"
                Dotnet.ProjInfo.Inspect.msbuild msbuildPath runCmd

            let additionalArgs = additionalMSBuildProps |> List.map (Dotnet.ProjInfo.Inspect.MSBuild.MSbuildCli.Property)

            let inspect =
                match parseAsSdk with
                | ProjectParsingSdk.DotnetSdk ->
                    Dotnet.ProjInfo.Inspect.getProjectInfos
                | ProjectParsingSdk.VerboseSdk ->
                    Dotnet.ProjInfo.Inspect.getProjectInfos

            let infoResult =
                file
                |> inspect loggedMessages.Enqueue msbuildExec [getFscArgs; getP2PRefs; gp] additionalArgs

            infoResult, (loggedMessages.ToArray() |> Array.toList)

        let todo =
            match results with
            | Ok [getFscArgsResult; getP2PRefsResult; gpResult] ->
                match getFscArgsResult, getP2PRefsResult, gpResult with
                | Error(MSBuildPrj.MSBuildSkippedTarget), Error(MSBuildPrj.MSBuildSkippedTarget), Ok (MSBuildPrj.GetResult.Properties props) ->
                    // Projects with multiple target frameworks, fails if the target framework is not choosen
                    let prop key = props |> Map.ofList |> Map.tryFind key

                    match prop "IsCrossTargetingBuild", prop "TargetFrameworks" with
                    | Some (MSBuildPrj.MSBuild.ConditionEquals "true"), Some (MSBuildPrj.MSBuild.StringList tfms) ->
                        CrossTargeting tfms
                    | _ ->
                        failwithf "error getting msbuild info: some targets skipped, found props: %A" props
                | Ok (MSBuildPrj.GetResult.FscArgs fa), Ok (MSBuildPrj.GetResult.ResolvedP2PRefs p2p), Ok (MSBuildPrj.GetResult.Properties p) ->
                    NoCrossTargeting { FscArgs = fa; P2PRefs = p2p; Properties = p |> Map.ofList }
                | r ->
                    failwithf "error getting msbuild info: %A" r
            | Ok r ->
                failwithf "error getting msbuild info: internal error, more info returned than expected %A" r
            | Error r ->
                match r with
                | Dotnet.ProjInfo.Inspect.GetProjectInfoErrors.MSBuildSkippedTarget ->
                    failwithf "Unexpected MSBuild result, all targets skipped"
                | Dotnet.ProjInfo.Inspect.GetProjectInfoErrors.UnexpectedMSBuildResult(r) ->
                    failwithf "Unexpected MSBuild result %s" r
                | Dotnet.ProjInfo.Inspect.GetProjectInfoErrors.MSBuildFailed(exitCode, (workDir, exePath, args)) ->
                    let logMsg = [ yield "Log: "; yield! log ] |> String.concat (Environment.NewLine)
                    let msbuildErrorMsg =
                        [ sprintf "MSBuild failed with exitCode %i" exitCode
                          sprintf "Working Directory: '%s'" workDir
                          sprintf "Exe Path: '%s'" exePath
                          sprintf "Args: '%s'" args ]
                        |> String.concat " "

                    failwithf "%s%s%s" msbuildErrorMsg (Environment.NewLine) logMsg

        match todo with
        | CrossTargeting (tfm :: _) ->
            // Atm setting a preferenece is not supported in FSAC
            // As workaround, lets choose the first of the target frameworks and use that
            file |> projInfo [MSBuildKnownProperties.TargetFramework, tfm]
        | CrossTargeting [] ->
            failwithf "Unexpected, found cross targeting but empty target frameworks list"
        | NoCrossTargeting { FscArgs = rsp; P2PRefs = p2ps; Properties = props } ->

            //TODO cache projects info of p2p ref
            let p2pProjects =
                p2ps
                // do not follow others lang project, is not supported by FCS anyway
                |> List.filter (fun p2p -> p2p.ProjectReferenceFullPath.ToLower().EndsWith(".fsproj"))
                |> List.map (fun p2p ->
                    let followP2pArgs =
                        p2p.TargetFramework
                        |> Option.map (fun tfm -> MSBuildKnownProperties.TargetFramework, tfm)
                        |> Option.toList
                    p2p.ProjectReferenceFullPath |> projInfo followP2pArgs )

            let tar =
                match props |> Map.tryFind "TargetPath" with
                | Some t -> t
                | None -> failwith "error, 'TargetPath' property not found"

            let rspNormalized =
                //workaround, arguments in rsp can use relative paths
                rsp |> List.map (FscArguments.useFullPaths projDir)

            let sdkTypeData, log =
                match parseAsSdk with
                | ProjectParsingSdk.DotnetSdk ->
                    let extraInfo = getExtraInfo tar props
                    ProjectSdkType.DotnetSdk(extraInfo), []
                | ProjectParsingSdk.VerboseSdk ->
                    //compatibility with old behaviour, so output is exactly the same
                    let mergedLog =
                        [ yield (file, "")
                          yield! p2pProjects |> List.collect (fun (_,_,x) -> x) ]
                    ProjectSdkType.Verbose { TargetPath = tar }, mergedLog

            let po =
                {
                    ProjectId = Some file
                    ProjectFileName = file
                    SourceFiles = [||]
                    OtherOptions = rspNormalized |> Array.ofList
                    ReferencedProjects = p2pProjects |> List.map (fun (x,y,_) -> (x,y)) |> Array.ofList
                    IsIncompleteTypeCheckEnvironment = false
                    UseScriptResolutionRules = false
                    LoadTime = DateTime.Now
                    UnresolvedReferences = None
                    OriginalLoadReferences = []
                    Stamp = None
                    ExtraProjectInfo =
                        Some (box {
                            ExtraProjectInfoData.ProjectSdkType = sdkTypeData
                            ExtraProjectInfoData.ProjectOutputType = FscArguments.outType rspNormalized
                        })
                }

            tar, po, log

    and projInfo additionalMSBuildProps file : ParsedProject =
        let key = sprintf "%s;%A" file additionalMSBuildProps
        match cache.TryGetValue(key) with
        | true, alreadyParsed ->
            alreadyParsed
        | false, _ ->
            let p = file |> projInfoOf additionalMSBuildProps
            cache.AddOrUpdate(key, p, fun _ _ -> p)


    let _, po, log = projInfo [] file
    po, log

  let private (|ProjectExtraInfoBySdk|_|) po =
      match po.ExtraProjectInfo with
      | None -> None
      | Some x ->
          match x with
          | :? ExtraProjectInfoData as extraInfo ->
              Some extraInfo
          | _ -> None

  let private loadBySdk notifyState (cache: ParsedProjectCache) parseAsSdk file =
      try
        let po, log = getProjectOptionsFromProjectFile notifyState cache parseAsSdk file

        let compileFiles =
            let sources = FscArguments.compileFiles (po.OtherOptions |> List.ofArray)
            match po with
            | ProjectExtraInfoBySdk extraInfo ->
                match extraInfo.ProjectSdkType with
                | ProjectSdkType.Verbose _ ->
                    //compatibility with old behaviour (projectcracker), so test output is exactly the same
                    //the temp source files (like generated assemblyinfo.fs) are not added to sources
                    let isTempFile (name: string) =
                        let tempPath = Path.GetTempPath()
                        let s = name.ToLower()
                        s.StartsWith(tempPath.ToLower())
                    sources
                    |> List.filter (not << isTempFile)
                | ProjectSdkType.ProjectJson
                | ProjectSdkType.DotnetSdk _ ->
                    sources
            | _ -> sources

        Ok (po, Seq.toList compileFiles, (log |> Map.ofList))
      with
        | ProjectInspectException d -> Error d
        | e -> Error (GenericError(file, e.Message))

  let load notifyState (cache: ParsedProjectCache) file =
      loadBySdk notifyState cache ProjectParsingSdk.DotnetSdk file

  let loadVerboseSdk notifyState (cache: ParsedProjectCache) file =
      loadBySdk notifyState cache ProjectParsingSdk.VerboseSdk file
