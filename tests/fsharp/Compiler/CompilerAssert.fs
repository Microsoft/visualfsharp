﻿// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace FSharp.Compiler.UnitTests

open System
open System.IO
open System.Text
open System.Diagnostics
open FSharp.Compiler.Text
open FSharp.Compiler.SourceCodeServices
open FSharp.Compiler.Interactive.Shell

open NUnit.Framework
open System.Reflection.Emit

[<RequireQualifiedAccess>]
module CompilerAssert =

    let checker = FSharpChecker.Create()

    let private config = TestFramework.initializeSuite ()

    let private defaultProjectOptions =
        {
            ProjectFileName = "Z:\\test.fsproj"
            ProjectId = None
            SourceFiles = [|"test.fs"|]
#if !NETCOREAPP
            OtherOptions = [||]
#else
            OtherOptions = 
                // Hack: Currently a hack to get the runtime assemblies for netcore in order to compile.
                let assemblies =
                    typeof<obj>.Assembly.Location
                    |> Path.GetDirectoryName
                    |> Directory.EnumerateFiles
                    |> Seq.toArray
                    |> Array.filter (fun x -> x.ToLowerInvariant().Contains("system."))
                    |> Array.map (fun x -> sprintf "-r:%s" x)
                Array.append [|"--targetprofile:netcore"; "--noframework"|] assemblies
#endif
            ReferencedProjects = [||]
            IsIncompleteTypeCheckEnvironment = false
            UseScriptResolutionRules = false
            LoadTime = DateTime()
            UnresolvedReferences = None
            OriginalLoadReferences = []
            ExtraProjectInfo = None
            Stamp = None
        }
        
    let private lockObj = obj ()

    let private compile source f =
        lock lockObj <| fun () ->
            let inputFilePath = Path.ChangeExtension(Path.GetTempFileName(), ".fs")
            let outputFilePath = Path.ChangeExtension (Path.GetTempFileName(), ".exe")
            let runtimeConfigFilePath = Path.ChangeExtension (outputFilePath, ".runtimeconfig.json")
            let tmpFsCoreFilePath = Path.Combine (Path.GetDirectoryName(outputFilePath), Path.GetFileName(config.FSCOREDLLPATH))
            try
                File.Copy (config.FSCOREDLLPATH, tmpFsCoreFilePath)
                File.WriteAllText (inputFilePath, source)
                File.WriteAllText (runtimeConfigFilePath, """
{
  "runtimeOptions": {
    "tfm": "netcoreapp2.1",
    "framework": {
      "name": "Microsoft.NETCore.App",
      "version": "2.1.0"
    }
  }
}
                """)

                let args =
                    defaultProjectOptions.OtherOptions
                    |> Array.append [| "fsc.exe"; inputFilePath; "-o:" + outputFilePath; "--target:exe"; "--nowin32manifest" |]
                let errors, _ = checker.Compile args |> Async.RunSynchronously

                if errors.Length > 0 then
                    Assert.Fail (sprintf "Compile had warnings and/or errors: %A" errors)

                f (errors, outputFilePath)

            finally
                try File.Delete inputFilePath with | _ -> ()
                try File.Delete outputFilePath with | _ -> ()
                try File.Delete runtimeConfigFilePath with | _ -> ()
                try File.Delete tmpFsCoreFilePath with | _ -> ()

    let Pass (source: string) =
        lock lockObj <| fun () ->
            let parseResults, fileAnswer = checker.ParseAndCheckFileInProject("test.fs", 0, SourceText.ofString source, defaultProjectOptions) |> Async.RunSynchronously

            Assert.True(parseResults.Errors.Length = 0, sprintf "Parse errors: %A" parseResults.Errors)

            match fileAnswer with
            | FSharpCheckFileAnswer.Aborted _ -> Assert.Fail("Type Checker Aborted")
            | FSharpCheckFileAnswer.Succeeded(typeCheckResults) ->

            Assert.True(typeCheckResults.Errors.Length = 0, sprintf "Type Check errors: %A" typeCheckResults.Errors)

    let TypeCheckSingleError (source: string) (expectedErrorNumber: int) (expectedErrorRange: int * int * int * int) (expectedErrorMsg: string) =
        lock lockObj <| fun () ->
            let parseResults, fileAnswer = checker.ParseAndCheckFileInProject("test.fs", 0, SourceText.ofString source, defaultProjectOptions) |> Async.RunSynchronously

            Assert.True(parseResults.Errors.Length = 0, sprintf "Parse errors: %A" parseResults.Errors)

            match fileAnswer with
            | FSharpCheckFileAnswer.Aborted _ -> Assert.Fail("Type Checker Aborted")
            | FSharpCheckFileAnswer.Succeeded(typeCheckResults) ->

            Assert.True(typeCheckResults.Errors.Length = 1, sprintf "Expected one type check error: %A" typeCheckResults.Errors)
            typeCheckResults.Errors
            |> Array.iter (fun info ->
                Assert.AreEqual(FSharpErrorSeverity.Error, info.Severity)
                Assert.AreEqual(expectedErrorNumber, info.ErrorNumber, "expectedErrorNumber")
                Assert.AreEqual(expectedErrorRange, (info.StartLineAlternate, info.StartColumn, info.EndLineAlternate, info.EndColumn), "expectedErrorRange")
                Assert.AreEqual(expectedErrorMsg, info.Message, "expectedErrorMsg")
            )

    let CompileSuccessfully (source: string) =
        compile source (fun _ -> ())

    let CompileAndRunSuccessfully (source: string) =
        compile source (fun (_, outputExe) ->
            let pInfo = ProcessStartInfo ()
#if NETCOREAPP
            pInfo.FileName <- config.DotNetExe
            pInfo.Arguments <- outputExe
#else
            pInfo.FileName <- outputExe
#endif

            pInfo.RedirectStandardError <- true
            pInfo.UseShellExecute <- false
            
            let p = Process.Start(pInfo)

            p.WaitForExit()
            let errors = p.StandardError.ReadToEnd ()
            if not (String.IsNullOrWhiteSpace errors) then
                Assert.Fail errors
        )
 
    let RunScript (source: string) (expectedErrorMessages: string list) =
        lock lockObj <| fun () ->
            // Intialize output and input streams
            use inStream = new StringReader("")
            use outStream = new StringWriter()
            use errStream = new StringWriter()

            // Build command line arguments & start FSI session
            let argv = [| "C:\\fsi.exe" |]
    #if !NETCOREAPP
            let allArgs = Array.append argv [|"--noninteractive"|]
    #else
            let allArgs = Array.append argv [|"--noninteractive"; "--targetprofile:netcore"|]
    #endif

            let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
            use fsiSession = FsiEvaluationSession.Create(fsiConfig, allArgs, inStream, outStream, errStream, collectible = true)
            
            let ch, errors = fsiSession.EvalInteractionNonThrowing source

            let errorMessages = ResizeArray()
            errors
            |> Seq.iter (fun error -> errorMessages.Add(error.Message))

            match ch with
            | Choice2Of2 ex -> errorMessages.Add(ex.Message)
            | _ -> ()

            if expectedErrorMessages.Length <> errorMessages.Count then
                Assert.Fail(sprintf "Expected error messages: %A \n\n Actual error messages: %A" expectedErrorMessages errorMessages)
            else
                (expectedErrorMessages, errorMessages)
                ||> Seq.iter2 (fun expectedErrorMessage errorMessage ->
                    Assert.AreEqual(expectedErrorMessage, errorMessage)
                )
        