// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

/// The configuration of the compiler (TcConfig and TcConfigBuilder)
module internal FSharp.Compiler.CompilerConfig

open System
open FSharp.Compiler.IO
open Internal.Utilities
open Internal.Utilities.Library
open FSharp.Compiler
open FSharp.Compiler.Xml
open FSharp.Compiler.AbstractIL
open FSharp.Compiler.AbstractIL.IL
open FSharp.Compiler.AbstractIL.ILBinaryReader
open FSharp.Compiler.AbstractIL.ILPdbWriter
open FSharp.Compiler.DependencyManager
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.ErrorLogger
open FSharp.Compiler.Features
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text

exception FileNameNotResolved of (*filename*) string * (*description of searched locations*) string * range
exception LoadedSourceNotFoundIgnoring of (*filename*) string * range

/// Represents a reference to an F# assembly. May be backed by a real assembly on disk (read by Abstract IL), or a cross-project
/// reference in FSharp.Compiler.Service.
type IRawFSharpAssemblyData =

    ///  The raw list AutoOpenAttribute attributes in the assembly
    abstract GetAutoOpenAttributes: ILGlobals -> string list

    ///  The raw list InternalsVisibleToAttribute attributes in the assembly
    abstract GetInternalsVisibleToAttributes: ILGlobals  -> string list

    ///  The raw IL module definition in the assembly, if any. This is not present for cross-project references
    /// in the language service
    abstract TryGetILModuleDef: unit -> ILModuleDef option

    abstract HasAnyFSharpSignatureDataAttribute: bool

    abstract HasMatchingFSharpSignatureDataAttribute: ILGlobals -> bool

    ///  The raw F# signature data in the assembly, if any
    abstract GetRawFSharpSignatureData: range * ilShortAssemName: string * fileName: string -> (string * (unit -> ReadOnlyByteMemory)) list

    ///  The raw F# optimization data in the assembly, if any
    abstract GetRawFSharpOptimizationData: range * ilShortAssemName: string * fileName: string -> (string * (unit -> ReadOnlyByteMemory)) list

    ///  The table of type forwarders in the assembly
    abstract GetRawTypeForwarders: unit -> ILExportedTypesAndForwarders

    /// The identity of the module
    abstract ILScopeRef: ILScopeRef

    abstract ILAssemblyRefs: ILAssemblyRef list

    abstract ShortAssemblyName: string

type TimeStampCache =
    new: defaultTimeStamp: DateTime -> TimeStampCache
    member GetFileTimeStamp: string -> DateTime
    member GetProjectReferenceTimeStamp: IProjectReference -> DateTime

and IProjectReference =

    /// The name of the assembly file generated by the project
    abstract FileName: string

    /// Evaluate raw contents of the assembly file generated by the project
    abstract EvaluateRawContents: CompilationThreadToken -> Cancellable<IRawFSharpAssemblyData option>

    /// Get the logical timestamp that would be the timestamp of the assembly file generated by the project.
    ///
    /// For project references this is maximum of the timestamps of all dependent files.
    /// The project is not actually built, nor are any assemblies read, but the timestamps for each dependent file
    /// are read via the FileSystem.  If the files don't exist, then a default timestamp is used.
    ///
    /// The operation returns None only if it is not possible to create an IncrementalBuilder for the project at all, e.g. if there
    /// are fatal errors in the options for the project.
    abstract TryGetLogicalTimeStamp: TimeStampCache -> System.DateTime option

type AssemblyReference =
    | AssemblyReference of range * string  * IProjectReference option

    member Range: range

    member Text: string

    member ProjectReference: IProjectReference option

    member SimpleAssemblyNameIs: string -> bool

type UnresolvedAssemblyReference = UnresolvedAssemblyReference of string * AssemblyReference list

[<RequireQualifiedAccess>]
type CompilerTarget =
    | WinExe
    | ConsoleExe
    | Dll
    | Module
    member IsExe: bool

[<RequireQualifiedAccess>]
type CopyFSharpCoreFlag = Yes | No

/// Represents the file or string used for the --version flag
type VersionFlag =
    | VersionString of string
    | VersionFile of string
    | VersionNone
    member GetVersionInfo: implicitIncludeDir:string -> ILVersionInfo
    member GetVersionString: implicitIncludeDir:string -> string

type Directive =
    | Resolution
    | Include

type LStatus =
    | Unprocessed
    | Processed

type PackageManagerLine =
    { Directive: Directive
      LineStatus: LStatus
      Line: string
      Range: range }

    static member AddLineWithKey: string -> Directive -> string -> range -> Map<string, PackageManagerLine list> -> Map<string, PackageManagerLine list>
    static member RemoveUnprocessedLines: string -> Map<string, PackageManagerLine list> -> Map<string, PackageManagerLine list>
    static member SetLinesAsProcessed: string -> Map<string, PackageManagerLine list> -> Map<string, PackageManagerLine list>
    static member StripDependencyManagerKey: string -> string -> string

[<RequireQualifiedAccess>]
type MetadataAssemblyGeneration =
    | None
    /// Includes F# signature and optimization metadata as resources in the emitting assembly.
    /// Implementation assembly will still be emitted normally, but will emit the reference assembly with the specified output path. 
    | ReferenceOut of outputPath: string
    /// Includes F# signature and optimization metadata as resources in the emitting assembly.
    /// Only emits the assembly as a reference assembly.
    | ReferenceOnly
    /// Do not include F# optimization metadata as a resource in the emitting assembly.
    /// Means we do not necessarily need to type-check implementation files if they have a backing signature file.
    ///     Instead, a dummy implementation file will be created.
    | MetadataOnly
    /// This is only for used for testing.
    | TestSigOfImpl

[<NoEquality; NoComparison>]
type TcConfigBuilder =
    { mutable primaryAssembly: PrimaryAssembly
      mutable noFeedback: bool
      mutable stackReserveSize: int32 option
      mutable implicitIncludeDir: string
      mutable openDebugInformationForLaterStaticLinking: bool
      defaultFSharpBinariesDir: string
      mutable compilingFslib: bool
      mutable useIncrementalBuilder: bool
      mutable includes: string list
      mutable implicitOpens: string list
      mutable useFsiAuxLib: bool
      mutable framework: bool
      mutable resolutionEnvironment: LegacyResolutionEnvironment
      mutable implicitlyResolveAssemblies: bool
      /// Set if the user has explicitly turned indentation-aware syntax on/off
      mutable light: bool option
      mutable conditionalCompilationDefines: string list
      /// Sources added into the build with #load
      mutable loadedSources: (range * string * string) list
      mutable compilerToolPaths: string  list
      mutable referencedDLLs: AssemblyReference  list
      mutable packageManagerLines: Map<string, PackageManagerLine list>
      mutable projectReferences: IProjectReference list
      mutable knownUnresolvedReferences: UnresolvedAssemblyReference list
      reduceMemoryUsage: ReduceMemoryFlag
      mutable subsystemVersion: int * int
      mutable useHighEntropyVA: bool
      mutable inputCodePage: int option
      mutable embedResources: string list
      mutable errorSeverityOptions: FSharpDiagnosticOptions
      mutable mlCompatibility:bool
      mutable checkOverflow:bool
      mutable showReferenceResolutions:bool
      mutable outputDir: string option
      mutable outputFile: string option
      mutable platform: ILPlatform option
      mutable prefer32Bit: bool
      mutable useSimpleResolution: bool
      mutable target: CompilerTarget
      mutable debuginfo: bool
      mutable testFlagEmitFeeFeeAs100001: bool
      mutable dumpDebugInfo: bool
      mutable debugSymbolFile: string option
      mutable typeCheckOnly: bool
      mutable parseOnly: bool
      mutable importAllReferencesOnly: bool
      mutable simulateException: string option
      mutable printAst: bool
      mutable tokenizeOnly: bool
      mutable testInteractionParser: bool
      mutable reportNumDecls: bool
      mutable printSignature: bool
      mutable printSignatureFile: string
      mutable printAllSignatureFiles: bool
      mutable xmlDocOutputFile: string option
      mutable stats: bool
      mutable generateFilterBlocks: bool
      mutable signer: string option
      mutable container: string option
      mutable delaysign: bool
      mutable publicsign: bool
      mutable version: VersionFlag
      mutable metadataVersion: string option
      mutable standalone: bool
      mutable extraStaticLinkRoots: string list
      mutable noSignatureData: bool
      mutable onlyEssentialOptimizationData: bool
      mutable useOptimizationDataFile: bool
      mutable jitTracking: bool
      mutable portablePDB: bool
      mutable embeddedPDB: bool
      mutable embedAllSource: bool
      mutable embedSourceList: string list
      mutable sourceLink: string
      mutable ignoreSymbolStoreSequencePoints: bool
      mutable internConstantStrings: bool
      mutable extraOptimizationIterations: int
      mutable win32icon: string
      mutable win32res: string
      mutable win32manifest: string
      mutable includewin32manifest: bool
      mutable linkResources: string list
      mutable legacyReferenceResolver: LegacyReferenceResolver
      mutable showFullPaths: bool
      mutable errorStyle: ErrorStyle
      mutable utf8output: bool
      mutable flatErrors: bool
      mutable maxErrors: int
      mutable abortOnError: bool
      mutable baseAddress: int32 option
      mutable checksumAlgorithm: HashAlgorithm
 #if DEBUG
      mutable showOptimizationData: bool
#endif
      mutable showTerms    : bool
      mutable writeTermsToFiles: bool
      mutable doDetuple    : bool
      mutable doTLR        : bool
      mutable doFinalSimplify: bool
      mutable optsOn       : bool
      mutable optSettings  : Optimizer.OptimizationSettings
      mutable emitTailcalls: bool
      mutable deterministic: bool
      mutable concurrentBuild: bool
      mutable emitMetadataAssembly: MetadataAssemblyGeneration
      mutable preferredUiLang: string option
      mutable lcid        : int option
      mutable productNameForBannerText: string
      mutable showBanner : bool
      mutable showTimes: bool
      mutable showLoadedAssemblies: bool
      mutable continueAfterParseFailure: bool
#if !NO_EXTENSIONTYPING
      mutable showExtensionTypeMessages: bool
#endif
      mutable pause: bool
      mutable alwaysCallVirt: bool
      mutable noDebugData: bool

      /// If true, indicates all type checking and code generation is in the context of fsi.exe
      isInteractive: bool
      isInvalidationSupported: bool
      mutable emitDebugInfoInQuotations: bool
      mutable exename: string option
      mutable copyFSharpCore: CopyFSharpCoreFlag
      mutable shadowCopyReferences: bool
      mutable useSdkRefs: bool
      mutable fxResolver: FxResolver option
      rangeForErrors: range
      sdkDirOverride: string option

      /// A function to call to try to get an object that acts as a snapshot of the metadata section of a .NET binary,
      /// and from which we can read the metadata. Only used when metadataOnly=true.
      mutable tryGetMetadataSnapshot : ILReaderTryGetMetadataSnapshot

      /// if true - 'let mutable x = Span.Empty', the value 'x' is a stack referring span. Used for internal testing purposes only until we get true stack spans.
      mutable internalTestSpanStackReferring : bool

      /// Prevent erasure of conditional attributes and methods so tooling is able analyse them.
      mutable noConditionalErasure: bool

      mutable pathMap : PathMap

      mutable langVersion : LanguageVersion

      mutable xmlDocInfoLoader : IXmlDocumentationInfoLoader option
    }

    static member CreateNew:
        legacyReferenceResolver: LegacyReferenceResolver *
        defaultFSharpBinariesDir: string *
        reduceMemoryUsage: ReduceMemoryFlag *
        implicitIncludeDir: string *
        isInteractive: bool *
        isInvalidationSupported: bool *
        defaultCopyFSharpCore: CopyFSharpCoreFlag *
        tryGetMetadataSnapshot: ILReaderTryGetMetadataSnapshot *
        sdkDirOverride: string option *
        rangeForErrors: range
          -> TcConfigBuilder

    member DecideNames: string list -> outfile: string * pdbfile: string option * assemblyName: string

    member TurnWarningOff: range * string -> unit

    member TurnWarningOn: range * string -> unit

    member AddIncludePath: range * string * string -> unit

    member AddCompilerToolsByPath: string -> unit

    member AddReferencedAssemblyByPath: range * string -> unit

    member RemoveReferencedAssemblyByPath: range * string -> unit

    member AddEmbeddedSourceFile: string -> unit

    member AddEmbeddedResource: string -> unit

    member AddPathMapping: oldPrefix: string * newPrefix: string -> unit

    static member SplitCommandLineResourceInfo: string -> string * string * ILResourceAccess

    // Directories to start probing in for native DLLs for FSI dynamic loading
    member GetNativeProbingRoots: unit -> seq<string>

    member AddReferenceDirective: dependencyProvider: DependencyProvider * m: range * path: string * directive: Directive -> unit

    member AddLoadedSource: m: range * originalPath: string * pathLoadedFrom: string -> unit

    member FxResolver: FxResolver

    member SetUseSdkRefs: useSdkRefs: bool -> unit

    member SetPrimaryAssembly: primaryAssembly: PrimaryAssembly -> unit

/// Immutable TcConfig, modifications are made via a TcConfigBuilder
[<Sealed>]
type TcConfig =
    member primaryAssembly: PrimaryAssembly
    member noFeedback: bool
    member stackReserveSize: int32 option
    member implicitIncludeDir: string
    member openDebugInformationForLaterStaticLinking: bool
    member fsharpBinariesDir: string
    member compilingFslib: bool
    member useIncrementalBuilder: bool
    member includes: string list
    member implicitOpens: string list
    member useFsiAuxLib: bool
    member framework: bool
    member implicitlyResolveAssemblies: bool
    /// Set if the user has explicitly turned indentation-aware syntax on/off
    member light: bool option
    member conditionalCompilationDefines: string list
    member subsystemVersion: int * int
    member useHighEntropyVA: bool
    member compilerToolPaths: string list
    member referencedDLLs: AssemblyReference list
    member reduceMemoryUsage: ReduceMemoryFlag
    member inputCodePage: int option
    member embedResources: string list
    member errorSeverityOptions: FSharpDiagnosticOptions
    member mlCompatibility:bool
    member checkOverflow:bool
    member showReferenceResolutions:bool
    member outputDir: string option
    member outputFile: string option
    member platform: ILPlatform option
    member prefer32Bit: bool
    member useSimpleResolution: bool
    member target: CompilerTarget
    member debuginfo: bool
    member testFlagEmitFeeFeeAs100001: bool
    member dumpDebugInfo: bool
    member debugSymbolFile: string option
    member typeCheckOnly: bool
    member parseOnly: bool
    member importAllReferencesOnly: bool
    member simulateException: string option
    member printAst: bool
    member tokenizeOnly: bool
    member testInteractionParser: bool
    member reportNumDecls: bool
    member printSignature: bool
    member printSignatureFile: string
    member printAllSignatureFiles: bool
    member xmlDocOutputFile: string option
    member stats: bool
    member generateFilterBlocks: bool
    member signer: string option
    member container: string option
    member delaysign: bool
    member publicsign: bool
    member version: VersionFlag
    member metadataVersion: string option
    member standalone: bool
    member extraStaticLinkRoots: string list
    member noSignatureData: bool
    member onlyEssentialOptimizationData: bool
    member useOptimizationDataFile: bool
    member jitTracking: bool
    member portablePDB: bool
    member embeddedPDB: bool
    member embedAllSource: bool
    member embedSourceList: string list
    member sourceLink: string
    member ignoreSymbolStoreSequencePoints: bool
    member internConstantStrings: bool
    member extraOptimizationIterations: int
    member win32icon: string
    member win32res: string
    member win32manifest: string
    member includewin32manifest: bool
    member linkResources: string list
    member showFullPaths: bool
    member errorStyle: ErrorStyle
    member utf8output: bool
    member flatErrors: bool

    member maxErrors: int
    member baseAddress: int32 option
    member checksumAlgorithm: HashAlgorithm
#if DEBUG
    member showOptimizationData: bool
#endif
    member showTerms    : bool
    member writeTermsToFiles: bool
    member doDetuple    : bool
    member doTLR        : bool
    member doFinalSimplify: bool
    member optSettings  : Optimizer.OptimizationSettings
    member emitTailcalls: bool
    member deterministic: bool
    member concurrentBuild: bool
    member emitMetadataAssembly: MetadataAssemblyGeneration
    member pathMap: PathMap
    member preferredUiLang: string option
    member optsOn       : bool
    member productNameForBannerText: string
    member showBanner : bool
    member showTimes: bool
    member showLoadedAssemblies: bool
    member continueAfterParseFailure: bool
#if !NO_EXTENSIONTYPING
    member showExtensionTypeMessages: bool
#endif
    member pause: bool
    member alwaysCallVirt: bool
    member noDebugData: bool

    /// If true, indicates all type checking and code generation is in the context of fsi.exe
    member isInteractive: bool
    member isInvalidationSupported: bool

    member xmlDocInfoLoader: IXmlDocumentationInfoLoader option

    member FxResolver: FxResolver

    member ComputeLightSyntaxInitialStatus: string -> bool

    member GetTargetFrameworkDirectories: unit -> string list

    /// Get the loaded sources that exist and issue a warning for the ones that don't
    member GetAvailableLoadedSources: unit -> (range*string) list

    member ComputeCanContainEntryPoint: sourceFiles:string list -> bool list *bool

    /// File system query based on TcConfig settings
    member ResolveSourceFile: range * filename: string * pathLoadedFrom: string -> string

    /// File system query based on TcConfig settings
    member MakePathAbsolute: string -> string

    member resolutionEnvironment: LegacyResolutionEnvironment

    member copyFSharpCore: CopyFSharpCoreFlag

    member shadowCopyReferences: bool

    member useSdkRefs: bool

    member sdkDirOverride: string option

    member legacyReferenceResolver: LegacyReferenceResolver

    member emitDebugInfoInQuotations: bool

    member langVersion: LanguageVersion

    static member Create: TcConfigBuilder * validate: bool -> TcConfig

    member tryGetMetadataSnapshot: ILReaderTryGetMetadataSnapshot

    member targetFrameworkVersion : string

    member knownUnresolvedReferences: UnresolvedAssemblyReference list

    member packageManagerLines: Map<string, PackageManagerLine list>

    member loadedSources: (range * string * string) list

    /// Prevent erasure of conditional attributes and methods so tooling is able analyse them.
    member noConditionalErasure: bool

    /// if true - 'let mutable x = Span.Empty', the value 'x' is a stack referring span. Used for internal testing purposes only until we get true stack spans.
    member internalTestSpanStackReferring : bool

    member GetSearchPathsForLibraryFiles: unit -> string list

    member IsSystemAssembly: string -> bool

    member PrimaryAssemblyDllReference: unit -> AssemblyReference

    member CoreLibraryDllReference: unit -> AssemblyReference

    /// Allow forking and subsuequent modification of the TcConfig via a new TcConfigBuilder
    member CloneToBuilder: unit -> TcConfigBuilder

    /// Indicates if the compilation will result in F# signature data resource in the generated binary
    member GenerateSignatureData: bool

    /// Indicates if the compilation will result in an F# optimization data resource in the generated binary
    member GenerateOptimizationData: bool

    /// Check if the primary assembly is mscorlib
    member assumeDotNetFramework: bool

/// Represents a computation to return a TcConfig. Normally this is just a constant immutable TcConfig,
/// but for F# Interactive it may be based on an underlying mutable TcConfigBuilder.
[<Sealed>]
type TcConfigProvider =

    member Get: CompilationThreadToken -> TcConfig

    /// Get a TcConfigProvider which will return only the exact TcConfig.
    static member Constant: TcConfig -> TcConfigProvider

    /// Get a TcConfigProvider which will continue to respect changes in the underlying
    /// TcConfigBuilder rather than delivering snapshots.
    static member BasedOnMutableBuilder: TcConfigBuilder -> TcConfigProvider

val TryResolveFileUsingPaths: paths: string list * m: range * name: string -> string option

val ResolveFileUsingPaths: paths: string list * m: range * name: string -> string

val GetWarningNumber: m: range * warningNumber: string -> int option

/// Get the name used for FSharp.Core
val GetFSharpCoreLibraryName: unit -> string

/// Signature file suffixes
val FSharpSigFileSuffixes: string list

/// Implementation file suffixes
val FSharpImplFileSuffixes: string list

/// Script file suffixes
val FSharpScriptFileSuffixes: string list

/// File suffixes where #light is the default
val FSharpLightSyntaxFileSuffixes: string list

val doNotRequireNamespaceOrModuleSuffixes: string list

val mlCompatSuffixes: string list
