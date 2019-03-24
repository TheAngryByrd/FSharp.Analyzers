namespace FSharp.Analyzer.App
open System.Reflection

module AssemblyInfo =
    let metaDataValue  (mda : AssemblyMetadataAttribute) = mda.Value

    let getMetaDataAttribute (assembly : Assembly) key =
        assembly.GetCustomAttributes(typedefof<AssemblyMetadataAttribute>)
        |> Seq.cast<AssemblyMetadataAttribute>
        |> Seq.find(fun x -> x.Key = key)

    let getReleaseDate assembly =
        "ReleaseDate"
        |> getMetaDataAttribute assembly
        |> metaDataValue

    let getGitHash assembly =
        "GitHash"
        |> getMetaDataAttribute assembly
        |> metaDataValue

    let getName (assembly : Assembly) =
        assembly.GetName ()
    let getVersion (assembly : Assembly) =
        (getName assembly).Version

    let assembly = lazy(Assembly.GetEntryAssembly())

    let formatVersion () =
        assembly.Force()
        |> getVersion
        |> sprintf "%A"

    let formatInfo () =
        let assembly = assembly.Force()
        let name = getName assembly
        let version = getVersion assembly
        let releaseDate = getReleaseDate assembly
        let githash  = getGitHash assembly
        sprintf "%s - %A - %s - %s" name.Name version releaseDate githash
