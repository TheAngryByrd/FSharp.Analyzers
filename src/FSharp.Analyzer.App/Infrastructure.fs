namespace FSharp.Analyzer.App

[<AutoOpen>]
module Infrastructure =
    module String =
        open System

        let ofNullOrWhiteSpace s =
            if String.IsNullOrWhiteSpace s |> not then Some s else None

        let join (serparator : string) (value : #seq<string>) =
            String.Join(serparator, value)

    module File =
        open System.IO

        let exists s =
            if File.Exists s then Some s else None

    module FileInfo =
        open System.IO

        let create (path : string) =
            try
                FileInfo path |> Ok
            with
            | e -> Error e

    module Result =
        let requireNotEmpty' error xs =
            if Seq.isEmpty xs then Error error else Ok xs
