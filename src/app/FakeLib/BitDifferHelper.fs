/// Contains tasks to generate Assembly level diffs using BitDiffer (https://github.com/grennis/bitdiffer)
module Fake.BitDifferHelper

open System
open System.IO
open System.Net
open System.Text
open Fake.Git.CommandHelper

/// The format of the output
type BitDifferFormat =
    | Html
    | Xml

/// The github project compilation target
type GitHubTarget =
    | ProjectOrSolution of string
    | Command of string * string list * (string -> string)
    | Solution

// A github commit to diff
type GitHubCommit = {
    /// The commit to diff against
    Commit: string;
    /// The compilation target. If not specified, will use the first *.sln found
    CompileTarget : GitHubTarget;
    /// The build output target. If not specified, a diff will be performed on all assemblies in the build output directories
    OutputTarget: string;
}

/// Diff the build output of two github commits
type GitHub = {
    /// The github repository url
    Url: Uri;
    /// A temporary directory in which to diff the commits. If the directory already exists, will be deleted first.
    TempDir: string;
    /// The first commit to diff against
    FirstCommit: GitHubCommit;
    /// The second commit to diff against
    SecondCommit: GitHubCommit;
}

/// Diff the assemblies in two nuget package versions
type Nuget = {
    /// The nuget package id
    Package: string;
    /// A temporary directory in which to diff the packages. If the directory already exists, will be deleted first.
    TempDir: string;
    /// The first package version to diff against
    FirstVersion: string;
    /// The second package version to diff against
    SecondVersion: string;
    /// The framework version of the package
    FrameworkVersion: string;
    /// The nuget package sources. Defaults to nuget v2 and v3 feeds if empty
    Sources: string list;
}

/// Diff two different assemblies
type Assemblies = {
    /// The path to the first assembly
    FirstPath: string;
    /// The path to the second assembly
    SecondPath: string;
}

/// Diff the assemblies in two different directories
type Directories = {
    /// The path to the first directory
    FirstDir: string;
    /// The path to the second directory
    SecondDir: string;
    /// Whether sub-directories should be recursed when searching for assemblies to diff.
    Recurse: bool;
}

/// The diff operation to perform
type Diff =
    | GitHub of GitHub
    | Nuget of Nuget
    | Assemblies of Assemblies
    | Directories of Directories

/// The isolation level in which the diff will take place
type IsolationLevel =
    | Auto
    | High
    | Medium
    | Low

/// The execution context in which the diff will take place
type Context =
    | Reflection
    | Execution

/// Diff operation parameters
[<CLIMutable>]
type BitDifferParams = {
    /// The path to BitDiffer.Console.exe
    ToolPath: string;
    /// The format of the output
    Format: BitDifferFormat;
    /// The path of the output file
    OutputFile : string;
    /// The path of a log file
    Log: string;
    /// The path of the raw file
    Raw: string;
    /// Whether to report all items, or just the changed items
    All: bool;
    /// Whether public members should be excluded
    ExcludePublic: bool;
    /// Whether protected members should be excluded
    ExcludeProtected: bool;
    /// Whether internal members should be excluded
    ExcludeInternal: bool;
    /// Whether private members should be excluded
    ExcludePrivate: bool;
    /// Whether method and property implementations are included in the diff. Note that this may not be able to show how the implementation has changed, only that it has.
    IgnoreMethodsAndProperties: bool;
    /// Whether assembly attributes are included in the diff
    IgnoreAssemblyAttributes: bool;
    /// Whether the diff operation should be multi threaded.
    MultiThreaded: bool;
    /// Whether assemblies should attempt to be loaded from the GAC before being loaded locally
    PreferGAC: bool;
    /// The isolation level in which the diff will take place
    IsolationLevel: IsolationLevel;
    /// The execution context in which the diff will take place
    Context: Context;
    /// A collection of directories that contain reference assemblies. Reference assemblies can be located
    /// in the same directory as a targeted assembly or in the reference assembly directories
    ReferenceDirectories: string list;
}

let private tempDir = Path.GetTempPath() </> Path.GetRandomFileName()

let BitDifferDefaults = {
    ToolPath = findToolInSubPath "BitDiffer.Console.exe" currentDirectory
    Format = Html
    OutputFile = currentDirectory @@ "diff.html"
    Log = ""
    Raw = ""
    All = false
    ExcludePublic = false
    ExcludeInternal = false
    ExcludeProtected = false
    ExcludePrivate = false
    IgnoreMethodsAndProperties = false
    IgnoreAssemblyAttributes = false
    MultiThreaded = true
    PreferGAC = false
    IsolationLevel = Auto
    Context = Reflection
    ReferenceDirectories = []
}

let private downloadNugetPackages nuget =
    DeleteDir nuget.TempDir 
    CreateDir nuget.TempDir
    let versions = [nuget.FirstVersion; nuget.SecondVersion] 
    versions
    |> Seq.map(fun v -> sprintf "%s/%s" nuget.TempDir v)
    |> Seq.iter CreateDir

    let nugetExe = 
        let path = findNuget (currentDirectory @@ "tools" @@ "NuGet")
        match fileExists path with
        | true -> path
        | false -> 
            let targetLocation = nuget.TempDir @@ "nuget.exe"
            trace (sprintf "Nuget not found. Downloading to %s" targetLocation)
            let url = "http://dist.nuget.org/win-x86-commandline/latest/nuget.exe" 
            use webClient = new WebClient()
            webClient.DownloadFile(url, targetLocation)
            trace "nuget downloaded"
            targetLocation

    let sources = 
        if List.isEmpty nuget.Sources then ["https://www.nuget.org/api/v2/";  "https://api.nuget.org/v3/index.json"]
        else nuget.Sources
        |> List.map (fun s -> sprintf "-Source %s" s)
        |> String.concat " "

    let packageVersionPath dir packageVersion =
            let desiredFrameworkVersion = Directory.GetDirectories dir
                                          |> Array.tryFind (fun f -> nuget.FrameworkVersion = Path.GetFileName f)
            match desiredFrameworkVersion with
            | Some f ->  f |> Path.GetFullPath
            | _ -> failwith (sprintf "Nuget package %s, version %s, does not contain framework version %s in %s" 
                                      nuget.Package 
                                      packageVersion 
                                      nuget.FrameworkVersion
                                      dir)

    versions
    |> Seq.map(fun v -> 
        let workingDir = nuget.TempDir @@ v
        let exitCode = ExecProcess(fun p ->            
                            p.FileName <- nugetExe
                            p.WorkingDirectory <- workingDir
                            p.Arguments <- (sprintf "install %s -Version %s %s -ExcludeVersion -NonInteractive" nuget.Package v sources)
                        ) (TimeSpan.FromMinutes 5.)

        if exitCode <> 0 then failwith (sprintf "Error downloading nuget package version: %s" v)

        // assumes DLLs are in the lib folder
        let packageDirs = Directory.GetDirectories workingDir
                          |> Array.filter (fun f -> nuget.Package <> Path.GetFileName f)
                          |> Array.map(fun f -> (f @@ "lib") |> Path.GetFullPath)

        let targetPath = packageVersionPath (workingDir @@ nuget.Package @@ "lib") v

        // targeting an individual assembly or the directory of assemblies
        let target = 
            let assemblyNamedAfterPackage = 
                Directory.EnumerateFiles(targetPath, "*.dll")
                |> Seq.tryPick (fun f -> 
                    let fileName = Path.GetFileNameWithoutExtension f
                    if String.Equals(fileName, nuget.Package, StringComparison.OrdinalIgnoreCase) 
                    then Some(f) 
                    else Option.None)
            match assemblyNamedAfterPackage with
            | Some a -> a
            | _ -> targetPath
        

        // copy all dependent package assemblies into target dir
        for packageDir in packageDirs do
            let path = packageVersionPath packageDir v
            path |> Directory.GetFiles |> CopyFiles targetPath

        target
    )

let private cloneAndBuildGitRepo (git:GitHub) =    
    let fullTempPath = git.TempDir |> Path.GetFullPath
    let repo = fullTempPath @@ "repo"

    DeleteDir fullTempPath
    CreateDir fullTempPath
    CreateDir repo

    directRunGitCommandAndFail repo (sprintf "clone %s ." git.Url.AbsoluteUri)

    let checkoutAndBuild (commit:GitHubCommit) =
        directRunGitCommandAndFail repo (sprintf "checkout %s" commit.Commit)
        let outputPath = fullTempPath @@ commit.Commit

        let compileProjectOrSolution ps =
            MSBuildHelper.MSBuildLoggers <- []
            MSBuildRelease outputPath "Rebuild" [ps] |> ignore
            outputPath

        let out = match commit.CompileTarget with
                    | ProjectOrSolution ps -> 
                        let projectOrSolution = repo @@ ps
                        compileProjectOrSolution projectOrSolution
                    | Command (c, a, f) ->
                        let failIfError exitCode = 
                            if exitCode > 0 then 
                                let message = sprintf "Command %s failed" c
                                traceError message
                                failwith message

                        ExecProcess(fun p ->
                            p.WorkingDirectory <- repo
                            p.FileName <- c
                            p.Arguments <- String.concat " " a
                        ) (TimeSpan.FromMinutes 5.)
                        |> failIfError 

                        let buildOutputPath = f(repo)
                        CopyDir outputPath buildOutputPath (fun s -> true)
                        outputPath
                           
                    | Solution ->
                        let sln = match TryFindFirstMatchingFile "*.sln" repo with
                                    | Some s -> Path.GetFullPath s
                                    | _ -> failwith (sprintf "Cannot find a sln file in %s" repo)

                        compileProjectOrSolution sln

        if String.IsNullOrEmpty commit.OutputTarget then out
        else out @@ commit.OutputTarget

    [git.FirstCommit; git.SecondCommit] 
    |> List.map checkoutAndBuild

/// Generates a diff between assembly files, assembly directories, assemblies in nuget packages
/// ## Parameters
///  - `diff` - The diff operation to perform
///  - `setParams` - A function to set the diff operation parameters
let Generate(diff: Diff, (setParams : BitDifferParams -> BitDifferParams)) =
    let mode = 
        match diff with 
        | GitHub g ->
            let checkouts = cloneAndBuildGitRepo g
            let targets =
                checkouts
                |> Seq.map(fun p -> sprintf "\"%s\"" p)
                |> String.concat " "
            if Seq.forall (fun t -> Path.HasExtension t && Path.GetExtension t = ".dll") checkouts 
            then targets
            else sprintf "-dirs %s" targets
        | Nuget n -> 
            let packages = downloadNugetPackages n
            let targets =
                packages
                |> Seq.map(fun p -> sprintf "\"%s\"" p)
                |> String.concat " "
            if Seq.forall (fun t -> Path.HasExtension t && Path.GetExtension t = ".dll") packages 
            then targets
            else sprintf "-dirs %s" targets
        | Assemblies a -> sprintf " \"%s\" \"%s\"" a.FirstPath a.SecondPath
        | Directories d -> 
            let dirs = sprintf "\"%s\" \"%s\"" d.FirstDir d.SecondDir
            match d.Recurse with
            | true -> sprintf "-dirs -recurse %s" dirs
            | false -> sprintf "-dirs %s" dirs
            
    let appendIfNotNullOrEmpty v f b =
        if (String.IsNullOrEmpty v = false) then Printf.bprintf b f v 
        b    

    let appendIfNotEmpty l b =
        if List.isEmpty l = false then
            l
            |> String.concat ";" 
            |> Printf.bprintf b "-refdirs \"%s\" "
        b

    let appendIsolation i (b:StringBuilder) =
        match i with
        | Auto -> ""
        | High -> "-isolation \"high\" "
        | Medium -> "-isolation \"medium\" "
        | Low -> "-isolation \"low\" "
        |> b.Append
        
    let appendExecution e b =
        match e with
        | Reflection -> ()
        | Execution -> Printf.bprintf b "-execution "
        b

    let differ param = 
        let args = new StringBuilder()
                    |> appendIfNotNullOrEmpty param.OutputFile "-out \"%s\" " 
                    |> appendIfNotNullOrEmpty param.Raw "-raw \"%s\" " 
                    |> appendIfNotNullOrEmpty param.Log "-log \"%s\" " 
                    |> appendIfTrue param.All "-all " 
                    |> appendIfTrue param.ExcludePublic "-xpublic " 
                    |> appendIfTrue param.ExcludeProtected "-xprotected " 
                    |> appendIfTrue param.ExcludeInternal "-xinternal " 
                    |> appendIfTrue param.ExcludePrivate "-xprivate " 
                    |> appendIfTrue param.IgnoreMethodsAndProperties "-noimpl " 
                    |> appendIfTrue param.IgnoreAssemblyAttributes "-noattrs " 
                    |> appendIfFalse param.MultiThreaded "-nomulti " 
                    |> appendIfTrue param.PreferGAC "-gacfirst " 
                    |> appendIsolation param.IsolationLevel
                    |> appendExecution param.Context
                    |> appendIfNotEmpty param.ReferenceDirectories
                    |> appendWithoutQuotes mode
                    |> toText

        ExecProcess(fun p -> 
            p.FileName <- param.ToolPath
            p.Arguments <- args
        ) (TimeSpan.FromMinutes 1.)

    BitDifferDefaults 
    |> setParams
    |> differ

