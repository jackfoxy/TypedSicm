
module Build.Tasks

open BlackFox.Fake
open System.IO
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.DotNet.Testing
open Fake.JavaScript
open Fake.Tools.Git
open Fake.Tools

// Information about the project is used
//  - for version and project name in generated AssemblyInfo file
//  - by the generated NuGet package
//  - to run tests and to publish documentation on GitHub gh-pages
//  - for documentation, you also need to edit info in "docsrc/tools/generate.fsx"

// The name of the project
// (used by attributes in AssemblyInfo, name of a NuGet package and directory in 'src')
let project = "TypedSicm"

// Short summary of the project
// (used as description in AssemblyInfo and as a short summary for NuGet package)
let summary = "Typed Sicm"

// Longer description of the project
// (used as a description for NuGet package; line breaks are automatically cleaned up)
let description = "Structure and Interpretation of Classical Mechanics, 2nd edition, in F#"

// List of author names (for NuGet package)
let author = "Jack Fox"

// Tags for your project (for NuGet package)
let tags = "F# sicm mechanics physics"

// File system information
let solutionFile  = "TypedSicm.sln"

// Default target configuration
let configuration = "Release"

// Pattern specifying assemblies to be tested using Expecto
let testAssemblies = "tests/**/bin/Release/net472/*Tests.exe"

// Git configuration (used for publishing documentation in gh-pages branch)
// The profile where the project is posted
let gitOwner = "jackfoxy"
let gitHome = sprintf "%s/%s" "https://github.com" gitOwner

// The name of the project on GitHub
let gitName = "TypedSicm"

let website = sprintf "/%s" "TypedSicm"

let testFolder = sprintf "../tests/%s.Tests" project

// --------------------------------------------------------------------------------------
// END TODO: The rest of the file includes standard build steps
// --------------------------------------------------------------------------------------

// Read additional information from the release notes document
let releaseNotes = ReleaseNotes.load "RELEASE_NOTES.md"

// Helper active pattern for project types
let (|Fsproj|Csproj|Vbproj|Shproj|) (projFileName:string) =
    match projFileName with
    | f when f.EndsWith("fsproj") -> Fsproj
    | f when f.EndsWith("csproj") -> Csproj
    | f when f.EndsWith("vbproj") -> Vbproj
    | f when f.EndsWith("shproj") -> Shproj
    | _                           -> failwith (sprintf "Project file %s not supported. Unknown project type." projFileName)
    
let serverPath = Path.getFullName "../src/Server"

let projectsToBuild =
    [|
        Path.getFullName "./src/Server"
        Path.getFullName <| sprintf "./src/%s" project
        Path.getFullName <| sprintf "./src/%sConsole" project
        Path.getFullName <| sprintf "./tests/%s.Tests" project
        Path.getFullName "./tests/Benchmark.Tests"
    |]

let platformTool tool winTool =
    let tool = if Environment.isUnix then tool else winTool
    match ProcessUtils.tryFindFileOnPath tool with
    | Some t -> t
    | _ ->
        let errorMsg =
            tool + " was not found in path. " +
            "Please install it and make sure it's available from your path. " +
            "See https://safe-stack.github.io/docs/quickstart/#install-pre-requisites for more info"
        failwith errorMsg

type JsPackageManager = 
    | NPM
    | YARN
    member this.RestoreTool =
        match this with
        | NPM -> platformTool "npm" "npm.cmd"
        | YARN -> platformTool "yarn" "yarn.cmd"
    member this.RunTool =
        match this with
        | NPM -> platformTool "npx" "npx.cmd"
        | YARN -> platformTool "yarn" "yarn.cmd"
    member this.ArgsInstall =
        match this with
        | NPM -> "install"
        | YARN -> "install --frozen-lockfile"

let getJsPackageManager () = 
    match Environment.environVarOrDefault "jsPackageManager" "yarn" with
    | "npm" -> NPM
    | "yarn" | _ -> YARN

let nodeTool = platformTool "node" "node.exe"

let runTool cmd args workingDir =
    let arguments = args |> String.split ' ' |> Arguments.OfArgs
    Command.RawCommand (cmd, arguments)
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory workingDir
    |> CreateProcess.ensureExitCode
    |> Proc.run
    |> ignore

let runDotNet cmd workingDir =
    let result =
        DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir

let openBrowser url =
    //https://github.com/dotnet/corefx/issues/10361
    Command.ShellCommand url
    |> CreateProcess.fromCommand
    |> CreateProcess.ensureExitCodeWithMessage "opening browser failed"
    |> Proc.run
    |> ignore

let globToArray x =
    !! x
    |> Seq.map id
    |> Seq.toArray

let binToClean() =
    !! "**/bin"
    -- ".build/bin"
    -- "node_modules/webpack-cli/bin"
    |> Seq.map id
    |> Seq.toArray

let createAndGetDefault () =
    let clean = BuildTask.create "Clean" [] {
        [|
            globToArray "**/src/**/bin"
            globToArray "**/src/**/obj"
            globToArray "**/tests/**/bin"
            globToArray "**/tests/**/obj"
            binToClean()
            [| 
                "temp"
            |]
        |]
        |> Array.collect id
        |> Shell.cleanDirs
        }

    let cleanDocs = BuildTask.create "CleanDocs" [] {
        Shell.cleanDirs ["../docs/reference"; "docs"]
        }
        
    let installClient = BuildTask.create "InstallClient" [] {
        let jsPackageManager = getJsPackageManager ()

        printfn "Node version:"
        runTool nodeTool "--version" __SOURCE_DIRECTORY__
        printfn "Npm version:"
        runTool jsPackageManager.RestoreTool "--version"  __SOURCE_DIRECTORY__
        runTool jsPackageManager.RestoreTool "install" __SOURCE_DIRECTORY__
    }

    // Generate assembly info files with the right version & up-to-date information
    let assemblyInfo = BuildTask.create "AssemblyInfo" [clean] {
        let getAssemblyInfoAttributes projectName =
            [   AssemblyInfo.Title (projectName)
                AssemblyInfo.Product project
                AssemblyInfo.Description summary
                AssemblyInfo.Version releaseNotes.AssemblyVersion
                AssemblyInfo.FileVersion releaseNotes.AssemblyVersion
                AssemblyInfo.Configuration configuration ]

        let getProjectDetails (projectPath :string) =
            let projectName = System.IO.Path.GetFileNameWithoutExtension(projectPath)
            ( projectPath,
                projectName,
                System.IO.Path.GetDirectoryName(projectPath),
                (getAssemblyInfoAttributes projectName)
            )

        !! "src/**/*.??proj"
        |> Seq.map getProjectDetails
        |> Seq.iter (fun (projFileName, _, folderName, attributes) ->
            match projFileName with
            | Fsproj -> AssemblyInfoFile.createFSharp (folderName </> "AssemblyInfo.fs") attributes
            | Csproj -> AssemblyInfoFile.createCSharp ((folderName </> "Properties") </> "AssemblyInfo.cs") attributes
            | Vbproj -> AssemblyInfoFile.createVisualBasic ((folderName </> "My Project") </> "AssemblyInfo.vb") attributes
            | Shproj -> ()
            )
    }

    let buildConfiguration = DotNet.Custom <| Environment.environVarOrDefault "configuration" configuration

    // --------------------------------------------------------------------------------------
    // Build library & test project

    let build = BuildTask.create "Build" [installClient;assemblyInfo] {
        let jsPackageManager = getJsPackageManager ()
        
        projectsToBuild
        |> Array.iter (fun x -> 
            DotNet.build (fun p ->
                { p with
                    Configuration = buildConfiguration }) x
        )
                
        runTool jsPackageManager.RunTool "webpack-cli -p" __SOURCE_DIRECTORY__
    }

    // Copies binaries from default VS location to expected bin folder
    // But keeps a subdirectory structure for each project in the
    // src folder to support multiple project outputs
    //Target.create "CopyBinaries" (fun _ ->
    let copyBinaries = BuildTask.create "CopyBinaries" [build] {
        !! "**/src/**/*.??proj"
        -- "**/src/**/*.shproj"
        |> Seq.map (fun f -> ((System.IO.Path.GetDirectoryName f) </> "bin" </> configuration, "bin" </> (System.IO.Path.GetFileNameWithoutExtension f)))
        |> Seq.filter (fun (fromDir, toDir) -> fromDir.ToLower().Contains("client") |> not)
        |> Seq.iter (fun (fromDir, toDir) -> Shell.copyDir toDir fromDir (fun _ -> true))
    }

    let run = BuildTask.create "Run" [build] {
        let jsPackageManager = getJsPackageManager ()
        
        let server = async {
            runDotNet "watch run" serverPath
        }
        let client = async {
            runTool jsPackageManager.RunTool "webpack-dev-server" __SOURCE_DIRECTORY__
        }
        let browser = async {
            do! Async.Sleep 5000
            openBrowser "http://localhost:8080"
        }

        let vsCodeSession = Environment.hasEnvironVar "vsCodeSession"
        let safeClientOnly = Environment.hasEnvironVar "safeClientOnly"

        let tasks =
            [ if not safeClientOnly then yield server
              yield client
              if not vsCodeSession then yield browser ]

        tasks
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore
    }


    // --------------------------------------------------------------------------------------
    // Run the unit tests using test runner

    let tests() = 
        !! testAssemblies
        |> Seq.filter (fun x -> x.ToLower().Contains("benchmark") |> not)
        |> Expecto.run id

    let runTests = BuildTask.create "RunTests" [copyBinaries] {
        tests()
    }

    let runTestsOnly = BuildTask.create "RunTestsOnly" [] {
        "tests/TypedSicm.Tests/TypedSicm.Tests.fsproj"
        |> DotNet.build (fun p -> 
            { p with
                Configuration = buildConfiguration })
        tests()
    }

    let runBenchmarks = BuildTask.create "RunBenchmarks" [copyBinaries] {
        !! testAssemblies
        |> Seq.filter (fun x -> x.ToLower().Contains("benchmark") )
        |> Expecto.run id
    }

    let runBenchmarksOnly = BuildTask.create "RunBenchmarksOnly" [] {
        !! testAssemblies
        |> Seq.filter (fun x -> x.ToLower().Contains("benchmark") )
        |> Expecto.run id
    }

    // --------------------------------------------------------------------------------------
    // Build a NuGet package

    let nuGet = BuildTask.create "NuGet" [copyBinaries] {
        let release = releaseNotes.Notes |> String.toLines

        Paket.pack(fun p -> 
            { p with
                OutputPath = "bin"
                Version = releaseNotes.NugetVersion
                ReleaseNotes = release})
    }

    let publishNuget = BuildTask.create "PublishNuget" [] {
        Paket.push(fun p ->
            { p with
                PublishUrl = "https://www.nuget.org"
                WorkingDir = "bin" })

    }

    // --------------------------------------------------------------------------------------
    // Generate the documentation

    // Paths with template/source/output locations
    let bin        = __SOURCE_DIRECTORY__ @@ "..\\bin"
    let content    = __SOURCE_DIRECTORY__ @@ "..\\docsrc\\content"
    let output     = __SOURCE_DIRECTORY__ @@ "..\\docs"
    let files      = __SOURCE_DIRECTORY__ @@ "..\\docsrc\\files"
    let templates  = __SOURCE_DIRECTORY__ @@ "..\\docsrc\\tools\\templates"
    let formatting = __SOURCE_DIRECTORY__ @@ "..\\packages\\formatting\\FSharp.Formatting"
    let docTemplate = "docpage.cshtml"

    let github_release_user = Environment.environVarOrDefault "github_release_user" gitOwner
    let githubLink = sprintf "https://github.com/%s/%s" github_release_user gitName

    let info =
        [ 
            "project-name", project
            "project-author", author
            "project-summary", summary
            "project-github", githubLink
            "project-nuget", sprintf "http://nuget.org/packages%s" website 
        ]

    let root = website

    let referenceBinaries = []

    let layoutRootsAll = new System.Collections.Generic.Dictionary<string, string list>()
    layoutRootsAll.Add("en",[   templates; 
                                formatting @@ "templates"
                                formatting @@ "templates/reference" ])

    let copyFiles () =
        Shell.copyRecursive files output true 
        |> Trace.logItems "Copying file: "
        Directory.ensure (output @@ "content")
        Shell.copyRecursive (formatting @@ "styles") (output @@ "content") true 
        |> Trace.logItems "Copying styles and scripts: "
        
    let replace (t : string) r (lines:seq<string>) =
        seq {
            for s in lines do
                if s.Contains(t) then 
                    yield s.Replace(t, r)
                else yield s }

    let postProcessDocs doc =
        let dirInfo = DirectoryInfo.ofPath output

        let filePath = System.IO.Path.Combine(dirInfo.FullName, doc)
        let newContent =
            File.ReadAllLines filePath
            |> Array.toSeq
            //|> replace "t1X2B62Xt1" "t<sub>1</sub> → t<sub>1</sub>"
            //|> replace "t1Xt2X2B62Xt1xXt2" "t<sub>1</sub> t<sub>2</sub> → t<sub>1</sub>&#39; t<sub>2</sub>"
            //|> replace "t2X2B62Xt2" "t<sub>2</sub> → t<sub>2</sub>"
            //|> replace "v1Xt2X2B62Xv1Xt2x" "v<sub>1</sub> t<sub>2</sub> → v<sub>1</sub> t<sub>2</sub>&#39;"
            //|> replace "Yt12Y" "t<sub>12</sub>"
            //|> replace "Xv2X2B62" " v<sub>2</sub> →"
            //|> replace "Yv2Y" "v<sub>2</sub>"
            //|> replace @"22A6" "⊢"
            //|> replace @"21B6" "↦"
        File.WriteAllLines(filePath, newContent)

        let filePath = System.IO.Path.Combine(dirInfo.FullName, "index.html")
        let newContent =
            File.ReadAllLines filePath
            |> Array.toSeq
            //|> replace "<h2>global Namespace</h2>" ""
        File.WriteAllLines(filePath, newContent)

    let postProcessReferenceDocs doc =
        let dirInfo = DirectoryInfo.ofPath <| sprintf "%s/%s" output "reference"

        let filePath = System.IO.Path.Combine(dirInfo.FullName, doc)
        let newContent =
            File.ReadAllLines filePath
            |> Array.toSeq
            //|> replace "<h2>global Namespace</h2>" ""
        File.WriteAllLines(filePath, newContent)

    let docsOnly = BuildTask.create "DocsOnly" [cleanDocs] {
        File.delete "docsrc/content/release-notes.md"
        Shell.copyFile "docsrc/content/" "RELEASE_NOTES.md"
        Shell.rename "docsrc/content/release-notes.md" "docsrc/content/RELEASE_NOTES.md"

        File.delete "docsrc/content/license.md"
        Shell.copyFile "docsrc/content/" "LICENSE.txt"
        Shell.rename "docsrc/content/license.md" "docsrc/content/LICENSE.txt"

        DirectoryInfo.getSubDirectories (DirectoryInfo.ofPath templates)
        |> Seq.iter (fun d ->
                        let name = d.Name
                        if name.Length = 2 || name.Length = 3 then
                            layoutRootsAll.Add(
                                    name, [ templates @@ name
                                            formatting @@ "templates"
                                            formatting @@ "templates/reference" ]))
        copyFiles ()
    
        for dir in  [ content; ] do
            let langSpecificPath(lang, path:string) =
                path.Split([|'/'; '\\'|], System.StringSplitOptions.RemoveEmptyEntries)
                |> Array.exists(fun i -> i = lang)
            let layoutRoots =
                let key = layoutRootsAll.Keys |> Seq.tryFind (fun i -> langSpecificPath(i, dir))
                match key with
                | Some lang -> layoutRootsAll.[lang]
                | None -> layoutRootsAll.["en"] // "en" is the default language

            FSFormatting.createDocs (fun args ->
                { args with
                    Source = content
                    OutputDirectory = output 
                    LayoutRoots = layoutRoots
                    ProjectParameters = ("root", root)::info
                    Template = docTemplate } )

        //postProcessDocs ""
    }

    let docs = BuildTask.createEmpty "Docs" [copyBinaries; docsOnly]

    let referenceDocs = BuildTask.create "ReferenceDocs" [cleanDocs; copyBinaries] {
        Directory.ensure (output @@ "reference")
        
        let binaries () =
            let manuallyAdded = 
                referenceBinaries 
                |> List.map (fun b -> bin @@ b)
           
            let conventionBased = 
                DirectoryInfo.getSubDirectories <| DirectoryInfo bin
                |> Array.filter (fun d -> 
                    d.Name = gitName
                )
                |> Array.collect (fun d ->
                    printfn "%s" d.FullName
                    let name, dInfo = 
                            d.Name, 
                                (DirectoryInfo.getSubDirectories d 
                                 |> Array.filter(fun x -> 
                                    x.FullName.ToLower().Contains("netcoreapp3.0")
                                    || x.FullName.ToLower().Contains("netcoreapp3.1")
                                    || x.FullName.ToLower().Contains("netstandard2.0")
                                 )
                                ).[0]
                    dInfo.GetFiles()
                    |> Array.filter (fun x -> 
                        x.Name.ToLower() = (sprintf "%s.dll" name).ToLower())
                    |> Array.map (fun x -> x.FullName) 
                    )
                |> List.ofArray
        
            conventionBased @ manuallyAdded
        
        binaries()
        |> FSFormatting.createDocsForDlls (fun args ->
            { args with
                OutputDirectory = output @@ "reference"
                LayoutRoots =  layoutRootsAll.["en"]
                ProjectParameters = ("root", root)::info
                SourceRepository = githubLink @@ "tree/master" }
                   )

        //postProcessReferenceDocs ""
    }

    let generateDocs = BuildTask.createEmpty "GenerateDocs" [docs; referenceDocs] 

    let release = BuildTask.create "Release" [] {
        Git.Staging.stageAll ""
        Git.Commit.exec "" (sprintf "Bump version to %s" releaseNotes.NugetVersion)
        Git.Branches.push ""

        Git.Branches.tag "" releaseNotes.NugetVersion
        Git.Branches.pushTag "" "origin" releaseNotes.NugetVersion
    }

    //BuildTask.createEmpty "All" [runTests; runBenchmarks; generateDocs; nuGet]
    BuildTask.createEmpty "All" [runTests; runBenchmarks; generateDocs; nuGet]

let listAvailable() = BuildTask.listAvailable()
