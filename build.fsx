#!/usr/bin/env -S dotnet fsi
#r "nuget: Fake.DotNet.Cli"
#r "nuget: Fake.IO.FileSystem"
#r "nuget: Fake.Core.ReleaseNotes"
#r "nuget: Fake.Core.Target"
#r "nuget: Fake.Tools.Git"
#r "nuget: Microsoft.Extensions.Configuration.Json"
#r "nuget: Microsoft.Extensions.Configuration.Binder"

open Fake.Core
open Fake.Core.TargetOperators
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.Tools
open System

let gitName = "MassTransit.fs"
let gitOwner = "et1975"
let gitHome = sprintf "https://github.com/%s" gitOwner
let gitIO = sprintf "https://%s.github.io/%s" gitOwner gitName

let gitRepo = sprintf "git@github.com:%s/%s.git" gitOwner gitName
let gitContent = sprintf "https://raw.githubusercontent.com/%s/%s" gitOwner gitName

let release = ReleaseNotes.load "RELEASE_NOTES.md"
let ver =
    match Environment.environVarOrNone "BUILD_NUMBER" with
    | Some n -> { release.SemVer with Patch = uint32 n; Original = None }
    | _ -> SemVer.parse "0.0.0"


System.Environment.GetCommandLineArgs() 
|> Array.skip 2 // fsi.exe; build.fsx
|> Array.toList
|> Context.FakeExecutionContext.Create false __SOURCE_FILE__
|> Context.RuntimeContext.Fake
|> Context.setExecutionContext

Target.create "clean" (fun _ ->
    !! "**/bin"
    ++ "**/obj"
    ++ "out"
    |> Seq.iter Shell.cleanDir
)

Target.create "restore" (fun _ ->
    DotNet.restore id "."
)

Target.create "build" (fun _ ->
    let args = "--no-restore"
    DotNet.publish (fun a -> a.WithCommon (fun c -> { c with CustomParams = Some args})) "."
)

Target.create "test" (fun _ ->
    let args = "--no-restore"
    DotNet.test (fun a -> a.WithCommon (fun c -> { c with CustomParams = Some args})) "."
)

Target.create "package" (fun _ ->
    let args = sprintf "/p:Version=%s --no-restore" ver.AsString
    DotNet.pack (fun a -> a.WithCommon (fun c -> { c with CustomParams = Some args })) "."
)

Target.create "publish" (fun _ ->
    let exec dir =
        DotNet.exec (fun a -> a.WithCommon (fun c -> { c with WorkingDirectory=dir }))
    let args = sprintf "push %s.%s.nupkg -s %s -k %s"
                       "MassTransitFs" ver.AsString
                       (Environment.environVar "NUGET_REPO_URL")
                       (Environment.environVar "NUGET_REPO_KEY")
    let result = exec ("src/MassTransit.fs/bin/Release") "nuget" args
    if (not result.OK) then failwithf "%A" result.Errors
)

Target.create "meta" (fun _ ->
    [ "<Project xmlns=\"http://schemas.microsoft.com/developer/msbuild/2003\">"
      "<PropertyGroup>"
      "<Copyright>MassTransit.fs contributors. All rights reserved.</Copyright>"
      "<Authors>Eugene Tolmachev</Authors>"
      "<PackageId>MassTransitFs</PackageId>"
      sprintf "<PackageProjectUrl>%s</PackageProjectUrl>" gitIO
      sprintf "<RepositoryUrl>%s/%s</RepositoryUrl>" gitHome gitName
      "<PackageLicense>MIT</PackageLicense>"
      sprintf "<PackageReleaseNotes>%s</PackageReleaseNotes>" (List.head release.Notes)
      sprintf "<PackageIconUrl>%s/master/docs/img/logo.png</PackageIconUrl>" gitContent
      "<PackageTags>MassTransit;fsharp</PackageTags>"
      sprintf "<Version>%s</Version>" (string ver)
      sprintf "<FsDocsLogoSource>%s/master/docs/img/logo.png</FsDocsLogoSource>" gitContent
      sprintf "<FsDocsLicenseLink>%s/%s/blob/master/LICENSE</FsDocsLicenseLink>" gitHome gitName
      sprintf "<FsDocsReleaseNotesLink>%s/%s/blob/master/RELEASE_NOTES.md</FsDocsReleaseNotesLink>" gitHome gitName
      "<FsDocsNavbarPosition>fixed-right</FsDocsNavbarPosition>"
      "<FsDocsWarnOnMissingDocs>true</FsDocsWarnOnMissingDocs>"
      "<FsDocsTheme>default</FsDocsTheme>"
      "</PropertyGroup>"
      "</Project>"]
    |> File.write false "Directory.Build.props"
)

Target.create "generateDocs" (fun _ ->
   Shell.cleanDir ".fsdocs"
   DotNet.exec id "fsdocs" "build --clean" |> ignore
)

Target.create "watchDocs" (fun _ ->
   Shell.cleanDir ".fsdocs"
   DotNet.exec id "fsdocs" "watch" |> ignore
)

Target.create "releaseDocs" (fun _ ->
    let tempDocsDir = "tmp/gh-pages"
    Shell.cleanDir tempDocsDir
    Git.Repository.cloneSingleBranch "" gitRepo "gh-pages" tempDocsDir
    Git.Repository.fullclean tempDocsDir
    Shell.copyRecursive "output" tempDocsDir true |> Trace.tracefn "%A"
    Git.Staging.stageAll tempDocsDir
    Git.Commit.exec tempDocsDir (sprintf "Update generated documentation for version %s" release.NugetVersion)
    Git.Branches.push tempDocsDir
)

Target.create "release" ignore
Target.create "ci" ignore

"clean"
  ==> "restore"
  ==> "meta"
  ==> "build"
  ==> "test"
  ==> "generateDocs"
  ==> "package"
  ==> "publish"

"releaseDocs"
  <== ["test"; "generateDocs" ]

"release"
  <== [ "publish" ]

"ci"
  <== [ "test" ]

Target.runOrDefaultWithArguments "test"