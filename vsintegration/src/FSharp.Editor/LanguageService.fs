// Copyright (c) Microsoft Corporation.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace Microsoft.VisualStudio.FSharp.Editor

open System
open System.Collections.Generic
open System.Runtime.InteropServices
open System.Linq
open System.IO

open Microsoft.FSharp.Compiler.CompileOps
open Microsoft.FSharp.Compiler.SourceCodeServices

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Editor.Options
open Microsoft.VisualStudio
open Microsoft.VisualStudio.Editor
open Microsoft.VisualStudio.TextManager.Interop
open Microsoft.VisualStudio.LanguageServices
open Microsoft.VisualStudio.LanguageServices.Implementation.LanguageService
open Microsoft.VisualStudio.LanguageServices.Implementation.ProjectSystem
open Microsoft.VisualStudio.LanguageServices.Implementation.DebuggerIntelliSense
open Microsoft.VisualStudio.LanguageServices.Implementation.TaskList
open Microsoft.VisualStudio.LanguageServices.Implementation
open Microsoft.VisualStudio.LanguageServices.ProjectSystem
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio.Shell.Interop
open Microsoft.VisualStudio.FSharp.LanguageService

// Workaround to access non-public settings persistence type.
// GetService( ) with this will work as long as the GUID matches the real type.
[<Guid(FSharpCommonConstants.svsSettingsPersistenceManagerGuidString)>]
type internal SVsSettingsPersistenceManager = class end

[<Guid(FSharpCommonConstants.languageServiceGuidString)>]
[<ProvideLanguageExtension(typeof<FSharpLanguageService>, ".fs")>]
[<ProvideLanguageExtension(typeof<FSharpLanguageService>, ".fsi")>]
[<ProvideLanguageExtension(typeof<FSharpLanguageService>, ".fsx")>]
[<ProvideLanguageExtension(typeof<FSharpLanguageService>, ".fsscript")>]
[<ProvideLanguageExtension(typeof<FSharpLanguageService>, ".ml")>]
[<ProvideLanguageExtension(typeof<FSharpLanguageService>, ".mli")>]
[<ProvideEditorExtension(FSharpCommonConstants.editorFactoryGuidString, ".fs", 97)>]
[<ProvideEditorExtension(FSharpCommonConstants.editorFactoryGuidString, ".fsi", 97)>]
[<ProvideEditorExtension(FSharpCommonConstants.editorFactoryGuidString, ".fsx", 97)>]
[<ProvideEditorExtension(FSharpCommonConstants.editorFactoryGuidString, ".fsscript", 97)>]
[<ProvideEditorExtension(FSharpCommonConstants.editorFactoryGuidString, ".ml", 97)>]
[<ProvideEditorExtension(FSharpCommonConstants.editorFactoryGuidString, ".mli", 97)>]
type internal FSharpLanguageService(package : FSharpPackage) =
    inherit AbstractLanguageService<FSharpPackage, FSharpLanguageService>(package)

    static let optionsCache = Dictionary<ProjectId, FSharpProjectOptions>()
    static let checker = lazy FSharpChecker.Create()
    static member Checker with get() = checker.Value

    static member GetOptions(projectId: ProjectId) =
        if optionsCache.ContainsKey(projectId) then
            Some(optionsCache.[projectId])
        else
            None

    static member UpdateOptions(projectId: ProjectId, options: FSharpProjectOptions) =
        optionsCache.[projectId] <- options

    member this.SyncProject(project: AbstractProject, projectContext: IWorkspaceProjectContext, site: IProjectSite) =
        let hashSetIgnoreCase x = new HashSet<string>(x, StringComparer.OrdinalIgnoreCase)
        let updatedFiles = site.SourceFilesOnDisk() |> hashSetIgnoreCase
        let workspaceFiles = project.GetCurrentDocuments() |> Seq.map(fun file -> file.FilePath) |> hashSetIgnoreCase

        let mutable updated = false
        for file in updatedFiles do
            if not(workspaceFiles.Contains(file)) then
                projectContext.AddSourceFile(file)
                updated <- true
        for file in workspaceFiles do
            if not(updatedFiles.Contains(file)) then
                projectContext.RemoveSourceFile(file)
                updated <- true

        // update the cached options
        if updated then
            let checkOptions = ProjectSitesAndFiles.GetProjectOptionsForProjectSite(site, "")
            FSharpLanguageService.UpdateOptions(project.Id, checkOptions)

    override this.ContentTypeName = FSharpCommonConstants.FSharpContentTypeName
    override this.LanguageName = FSharpCommonConstants.FSharpLanguageName
    override this.RoslynLanguageName = FSharpCommonConstants.FSharpLanguageName

    override this.LanguageServiceId = new Guid(FSharpCommonConstants.languageServiceGuidString)
    override this.DebuggerLanguageId = DebuggerEnvironment.GetLanguageID()

    override this.CreateContext(_,_,_,_,_) = raise(System.NotImplementedException())

    override this.SetupNewTextView(textView) =
        base.SetupNewTextView(textView)
        let workspace = this.Package.ComponentModel.GetService<VisualStudioWorkspaceImpl>()

        // FSROSLYNTODO: Hide navigation bars for now. Enable after adding tests
        workspace.Options <- workspace.Options.WithChangedOption(NavigationBarOptions.ShowNavigationBar, FSharpCommonConstants.FSharpLanguageName, false)

        match textView.GetBuffer() with
        | (VSConstants.S_OK, textLines) ->
            let filename = VsTextLines.GetFilename textLines
            match VsRunningDocumentTable.FindDocumentWithoutLocking(package.RunningDocumentTable,filename) with
            | Some (hier, _) ->
                match hier with
                | :? IProvideProjectSite as siteProvider when not (IsScript(filename)) -> 
                    this.SetupProjectFile(siteProvider, workspace)
                | _ -> 
                    let editorAdapterFactoryService = this.Package.ComponentModel.GetService<IVsEditorAdaptersFactoryService>()
                    let fileContents = VsTextLines.GetFileContents(textLines, editorAdapterFactoryService)
                    this.SetupStandAloneFile(filename, fileContents, workspace, hier)
            | _ -> ()
        | _ -> ()

    member this.SetupProjectFile(siteProvider: IProvideProjectSite, workspace: VisualStudioWorkspaceImpl) =
        let site = siteProvider.GetProjectSite()
        let projectGuid = Guid(site.ProjectGuid)
        let projectFileName = site.ProjectFileName()
        let projectId = workspace.ProjectTracker.GetOrCreateProjectIdForPath(projectFileName, projectFileName)

        let options = ProjectSitesAndFiles.GetProjectOptionsForProjectSite(site, site.ProjectFileName())
        FSharpLanguageService.UpdateOptions(projectId, options)

        match workspace.ProjectTracker.GetProject(projectId) with
        | null ->
            let projectContextFactory = this.Package.ComponentModel.GetService<IWorkspaceProjectContextFactory>();
            let errorReporter = ProjectExternalErrorReporter(projectId, "FS", this.SystemServiceProvider)

            let projectContext = projectContextFactory.CreateProjectContext(FSharpCommonConstants.FSharpLanguageName, projectFileName, projectFileName, projectGuid, siteProvider, null, errorReporter)
            let project = projectContext :?> AbstractProject

            this.SyncProject(project, projectContext, site)
            site.AdviseProjectSiteChanges(FSharpCommonConstants.FSharpLanguageServiceCallbackName, AdviseProjectSiteChanges(fun () -> this.SyncProject(project, projectContext, site)))
            site.AdviseProjectSiteClosed(FSharpCommonConstants.FSharpLanguageServiceCallbackName, AdviseProjectSiteChanges(fun () -> project.Disconnect()))
        | _ -> ()

    member this.SetupStandAloneFile(fileName: string, fileContents: string, workspace: VisualStudioWorkspaceImpl, hier: IVsHierarchy) =
        let options = FSharpLanguageService.Checker.GetProjectOptionsFromScript(fileName, fileContents, DateTime.Now, [| |]) |> Async.RunSynchronously
        let projectId = workspace.ProjectTracker.GetOrCreateProjectIdForPath(options.ProjectFileName, options.ProjectFileName)

        FSharpLanguageService.UpdateOptions(projectId, options)

        if obj.ReferenceEquals(workspace.ProjectTracker.GetProject(projectId), null) then
            let projectContextFactory = this.Package.ComponentModel.GetService<IWorkspaceProjectContextFactory>();
            let errorReporter = ProjectExternalErrorReporter(projectId, "FS", this.SystemServiceProvider)

            let projectContext = projectContextFactory.CreateProjectContext(FSharpCommonConstants.FSharpLanguageName, options.ProjectFileName, options.ProjectFileName, projectId.Id, hier, null, errorReporter)
            projectContext.AddSourceFile(fileName)
            
            let project = projectContext :?> AbstractProject
            let document = project.GetCurrentDocumentFromPath(fileName)

            document.Closing.Add(fun _ -> project.Disconnect())


and [<Guid(FSharpCommonConstants.packageGuidString)>]
    [<ProvideLanguageService(languageService = typeof<FSharpLanguageService>,
                             strLanguageName = FSharpCommonConstants.FSharpLanguageName,
                             languageResourceID = 100,
                             MatchBraces = true,
                             MatchBracesAtCaret = true,
                             ShowCompletion = true,
                             ShowMatchingBrace = true,
                             ShowSmartIndent = true,
                             EnableAsyncCompletion = true,
                             QuickInfo = true,
                             DefaultToInsertSpaces  = true,
                             CodeSense = true,
                             DefaultToNonHotURLs = true,
                             EnableCommenting = true,
                             CodeSenseDelay = 100)>]
        internal FSharpPackage() =
    inherit AbstractPackage<FSharpPackage, FSharpLanguageService>()

    override this.RoslynLanguageName = FSharpCommonConstants.FSharpLanguageName

    override this.CreateWorkspace() = this.ComponentModel.GetService<VisualStudioWorkspaceImpl>()

    override this.CreateLanguageService() = new FSharpLanguageService(this)

    override this.CreateEditorFactories() = Seq.empty<IVsEditorFactory>

    override this.RegisterMiscellaneousFilesWorkspaceInformation(_) = ()
