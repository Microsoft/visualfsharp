﻿// Copyright (c) Microsoft Corporation.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.Editor

open System
open System.Composition
open System.Collections.Immutable
open System.Threading
open System.Threading.Tasks

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Diagnostics
open Microsoft.CodeAnalysis.Host.Mef
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.SolutionCrawler

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.FSharp.Compiler.Range

open Microsoft.VisualStudio.FSharp.LanguageService

[<DiagnosticAnalyzer(FSharpCommonConstants.FSharpLanguageName)>]
type internal FSharpDocumentDiagnosticAnalyzer() =
    inherit DocumentDiagnosticAnalyzer()

    static member ConvertError(filePath: string, sourceText: SourceText, error: FSharpErrorInfo) =
        let id = "FS" + error.ErrorNumber.ToString("0000")
        let emptyString = LocalizableString.op_Implicit("")
        let description = LocalizableString.op_Implicit(error.Message)
        let severity = if error.Severity = FSharpErrorSeverity.Error then DiagnosticSeverity.Error else DiagnosticSeverity.Warning
        let descriptor = new DiagnosticDescriptor(id, emptyString, description, error.Subcategory, severity, true, emptyString, String.Empty, null)
                
        // F# compiler report errors at start/end of file if parsing fails. It should be corrected to match Roslyn boundaries
        let linePositionSpan = LinePositionSpan(
                                LinePosition(Math.Max(0, error.StartLineAlternate - 1), error.StartColumn),
                                LinePosition(Math.Max(0, error.EndLineAlternate - 1), error.EndColumn))
        let textSpan = sourceText.Lines.GetTextSpan(linePositionSpan)
        let correctedTextSpan = if textSpan.End < sourceText.Length then textSpan else TextSpan.FromBounds(sourceText.Length - 1, sourceText.Length)

        Diagnostic.Create(descriptor, Location.Create(filePath, correctedTextSpan , linePositionSpan))

    static member GetDiagnostics(filePath: string, sourceText: SourceText, textVersionHash: int, options: FSharpProjectOptions, addSemanticErrors: bool) =
        let parseResults = FSharpChecker.Instance.ParseFileInProject(filePath, sourceText.ToString(), options) |> Async.RunSynchronously
        let errors =
            if addSemanticErrors then
                let checkResultsAnswer = FSharpChecker.Instance.CheckFileInProject(parseResults, filePath, textVersionHash, sourceText.ToString(), options) |> Async.RunSynchronously
                match checkResultsAnswer with
                | FSharpCheckFileAnswer.Aborted -> failwith "Compilation isn't complete yet"
                | FSharpCheckFileAnswer.Succeeded(results) -> results.Errors
            else
                parseResults.Errors
            
        (errors |> Seq.map(fun (error) -> FSharpDocumentDiagnosticAnalyzer.ConvertError(filePath, sourceText, error))).ToImmutableArray()


    // We are constructing our own descriptors at run-time. Compiler service is already doing error formatting and localization.
    override this.SupportedDiagnostics
        with get() =
            let dummyDescriptor = DiagnosticDescriptor("0", String.Empty, String.Empty, String.Empty, DiagnosticSeverity.Error, true, null, null)
            ImmutableArray.Create<DiagnosticDescriptor>(dummyDescriptor)


    override this.AnalyzeSyntaxAsync(document: Document, cancellationToken: CancellationToken): Task<ImmutableArray<Diagnostic>> =
        let computation = async {
            match FSharpLanguageService.GetOptions(document.Project.Id) with
            | Some(options) ->
                let! sourceText = document.GetTextAsync(cancellationToken) |> Async.AwaitTask
                let! textVersion = document.GetTextVersionAsync(cancellationToken) |> Async.AwaitTask
                return FSharpDocumentDiagnosticAnalyzer.GetDiagnostics(document.FilePath, sourceText, textVersion.GetHashCode(), options, false)
            | None -> return ImmutableArray<Diagnostic>.Empty
        }

        Async.StartAsTask(computation, TaskCreationOptions.None, cancellationToken)
             .ContinueWith(CommonRoslynHelpers.GetCompletedTaskResult, cancellationToken)


    override this.AnalyzeSemanticsAsync(document: Document, cancellationToken: CancellationToken): Task<ImmutableArray<Diagnostic>> =
        let computation = async {
            match FSharpLanguageService.GetOptions(document.Project.Id) with
            | Some(options) ->
                let! sourceText = document.GetTextAsync(cancellationToken) |> Async.AwaitTask
                let! textVersion = document.GetTextVersionAsync(cancellationToken) |> Async.AwaitTask
                return FSharpDocumentDiagnosticAnalyzer.GetDiagnostics(document.FilePath, sourceText, textVersion.GetHashCode(), options, true)
            | None -> return ImmutableArray<Diagnostic>.Empty
        }

        Async.StartAsTask(computation, TaskCreationOptions.None, cancellationToken)
             .ContinueWith(CommonRoslynHelpers.GetCompletedTaskResult, cancellationToken)
