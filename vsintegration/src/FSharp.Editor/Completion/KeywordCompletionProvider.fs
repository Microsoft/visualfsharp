// Copyright (c) Microsoft Corporation.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.Editor

open System
open System.Threading
open System.Threading.Tasks

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Classification
open Microsoft.CodeAnalysis.Completion
open Microsoft.CodeAnalysis.Options
open Microsoft.CodeAnalysis.Text

open Microsoft.FSharp.Compiler
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

type internal FSharpKeywordCompletionProvider
    (
        workspace: Workspace,
        checkerProvider: FSharpCheckerProvider,
        projectInfoManager: ProjectInfoManager
    ) =
    
    inherit CompletionProvider()

    let completionItems =
        Lexhelp.Keywords.keywordsWithDescription
        |> List.filter (fun (keyword, _) -> not (PrettyNaming.IsOperatorName keyword))
        |> List.map (fun (keyword, description) ->
             CommonCompletionItem.Create(keyword, Nullable(Glyph.Keyword)).AddProperty("description", description))

    static member ShouldTriggerCompletionAux(sourceText: SourceText, caretPosition: int, trigger: CompletionTriggerKind, getInfo: (unit -> DocumentId * string * string list)) =
        // Skip if we are at the start of a document
        if caretPosition = 0 then
            false
        
        // Skip if it was triggered by an operation other than insertion
        elif not (trigger = CompletionTriggerKind.Insertion) then
            false
        
        // Skip if we are not on a completion trigger
        else
            let triggerPosition = caretPosition - 1
            let c = sourceText.[triggerPosition]
            
            // never show keyword after dot
            if c = '.' then 
                false
            
            // Trigger completion if we are on a valid classification type
            else
                let documentId, filePath, defines = getInfo()
                let textLines = sourceText.Lines
                let triggerLine = textLines.GetLineFromPosition(triggerPosition)

                let classifiedSpanOption =
                    CommonHelpers.getColorizationData(documentId, sourceText, triggerLine.Span, Some(filePath), defines, CancellationToken.None)
                    |> Seq.tryFind(fun classifiedSpan -> classifiedSpan.TextSpan.Contains(triggerPosition))
                
                match classifiedSpanOption with
                | None -> false
                | Some(classifiedSpan) ->
                    match classifiedSpan.ClassificationType with
                    | ClassificationTypeNames.Comment
                    | ClassificationTypeNames.StringLiteral
                    | ClassificationTypeNames.ExcludedCode
                    | ClassificationTypeNames.NumericLiteral -> false
                    | _ -> true // anything else is a valid classification type

    override this.ShouldTriggerCompletion(sourceText: SourceText, caretPosition: int, trigger: CompletionTrigger, _: OptionSet) =
        let getInfo() = 
            let documentId = workspace.GetDocumentIdInCurrentContext(sourceText.Container)
            let document = workspace.CurrentSolution.GetDocument(documentId)
            let defines = projectInfoManager.GetCompilationDefinesForEditingDocument(document)  
            (documentId, document.FilePath, defines)

        FSharpKeywordCompletionProvider.ShouldTriggerCompletionAux(sourceText, caretPosition, trigger.Kind, getInfo)
    
    override this.ProvideCompletionsAsync(context: Completion.CompletionContext) =
        asyncMaybe {
            let document = context.Document
            let! sourceText = context.Document.GetTextAsync(context.CancellationToken)
            let defines = projectInfoManager.GetCompilationDefinesForEditingDocument(document)
            do! Option.guard (CompletionUtils.shouldProvideCompletion(document.Id, document.FilePath, defines, sourceText, context.Position))
            let! options = projectInfoManager.TryGetOptionsForEditingDocumentOrProject(document)
            let! parseResults = checkerProvider.Checker.ParseFileInProject(document.FilePath, sourceText.ToString(), options) |> liftAsync
            let textLines = sourceText.Lines
            let caretLinePos = textLines.GetLinePosition(context.Position)
            let lineStr = textLines.[caretLinePos.Line].ToString()

            match UntypedParseImpl.TryGetCompletionContext(Pos.fromZ caretLinePos.Line caretLinePos.Character, Some parseResults, lineStr) with
            | Some (  CompletionContext.Invalid 
                    | CompletionContext.Inherit _ 
                    | CompletionContext.RecordField _ 
                    | CompletionContext.RangeOperator _ 
                    | CompletionContext.ParameterList _ 
                    | CompletionContext.AttributeApplication) -> ()
            | _ -> context.AddItems(completionItems)
            
        } |> Async.Ignore |> CommonRoslynHelpers.StartAsyncUnitAsTask context.CancellationToken

        //context.AddItems(completionItems)

    override this.GetDescriptionAsync(_: Document, completionItem: Completion.CompletionItem, _: CancellationToken): Task<CompletionDescription> =
        Task.FromResult(CompletionDescription.FromText(completionItem.Properties.["description"]))
