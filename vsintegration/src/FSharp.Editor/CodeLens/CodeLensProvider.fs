﻿// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace rec Microsoft.VisualStudio.FSharp.Editor


open System
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Utilities
open Microsoft.CodeAnalysis
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio
open Microsoft.VisualStudio.LanguageServices
open System.Collections.Generic
open Microsoft.CodeAnalysis.Editor.Shared.Utilities
open Microsoft.VisualStudio.Text.Tagging

        
[<Export(typeof<IWpfTextViewCreationListener>)>]
[<Export(typeof<IViewTaggerProvider>)>]
[<TagType(typeof<CodeLensGeneralTag>)>]
[<ContentType(FSharpConstants.FSharpContentTypeName)>]
[<TextViewRole(PredefinedTextViewRoles.Document)>]
type internal CodeLensProvider  
    [<ImportingConstructor>]
    (
        textDocumentFactory: ITextDocumentFactoryService,
        checkerProvider: FSharpCheckerProvider,
        projectInfoManager: FSharpProjectOptionsManager,
        typeMap: Lazy<ClassificationTypeMap>,
        gotoDefinitionService: FSharpGoToDefinitionService
    ) =
        
    let lineLensProvider = ResizeArray()
    let taggers = ResizeArray()
    let componentModel = Package.GetGlobalService(typeof<ComponentModelHost.SComponentModel>) :?> ComponentModelHost.IComponentModel
    let workspace = componentModel.GetService<VisualStudioWorkspace>()

    /// Returns an provider for the textView if already one has been created. Else create one.
    let addCodeLensProviderOnce wpfView buffer =
        let res = taggers |> Seq.tryFind(fun (view, _) -> view = wpfView)
        match res with
        | Some (_, (tagger, _)) -> tagger
        | None ->
            let documentId = 
                lazy (
                    match textDocumentFactory.TryGetTextDocument(buffer) with
                    | true, textDocument ->
                         Seq.tryHead (workspace.CurrentSolution.GetDocumentIdsWithFilePath(textDocument.FilePath))
                    | _ -> None
                    |> Option.get
                )

            let tagger = CodeLensGeneralTagger(wpfView, buffer)
            let service = FSharpCodeLensService(workspace, documentId, buffer, checkerProvider.Checker, projectInfoManager, typeMap, gotoDefinitionService, tagger)
            let provider = (wpfView, (tagger, service))
            wpfView.Closed.Add (fun _ -> taggers.Remove provider |> ignore)
            taggers.Add((wpfView, (tagger, service)))
            tagger
    
    /// Returns an provider for the textView if already one has been created. Else create one.
    let addLineLensProviderOnce wpfView buffer =
        let res = lineLensProvider |> Seq.tryFind(fun (view, _) -> view = wpfView)
        match res with
        | None ->
            let documentId = 
                lazy (
                    match textDocumentFactory.TryGetTextDocument(buffer) with
                    | true, textDocument ->
                         Seq.tryHead (workspace.CurrentSolution.GetDocumentIdsWithFilePath(textDocument.FilePath))
                    | _ -> None
                    |> Option.get
                )
            let service = FSharpCodeLensService(workspace, documentId, buffer, checkerProvider.Checker, projectInfoManager, typeMap, gotoDefinitionService, LineLensDisplayService(wpfView, buffer))
            let provider = (wpfView, service)
            wpfView.Closed.Add (fun _ -> lineLensProvider.Remove provider |> ignore)
            lineLensProvider.Add(provider)
        | _ -> ()

    [<Export(typeof<AdornmentLayerDefinition>); Name("CodeLens");
      Order(Before = PredefinedAdornmentLayers.Text);
      TextViewRole(PredefinedTextViewRoles.Document)>]
    member val CodeLensAdornmentLayerDefinition : AdornmentLayerDefinition = null with get, set
    
    [<Export(typeof<AdornmentLayerDefinition>); Name("LineLens");
      Order(Before = PredefinedAdornmentLayers.Text);
      TextViewRole(PredefinedTextViewRoles.Document)>]
    member val LineLensAdornmentLayerDefinition : AdornmentLayerDefinition = null with get, set

    interface IViewTaggerProvider with
        override __.CreateTagger(view, buffer) = 
            if Settings.CodeLens.Enabled && not Settings.CodeLens.ReplaceWithLineLens then
                let wpfView =
                    match view with
                    | :? IWpfTextView as view -> view
                    | _ -> failwith "error"
            
                box(addCodeLensProviderOnce wpfView buffer) :?> _
            else
                null

    interface IWpfTextViewCreationListener with
        override __.TextViewCreated view =
            if Settings.CodeLens.Enabled && Settings.CodeLens.ReplaceWithLineLens then
                addLineLensProviderOnce view (view.TextBuffer) |> ignore
