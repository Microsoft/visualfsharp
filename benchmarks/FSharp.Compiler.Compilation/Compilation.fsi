﻿namespace FSharp.Compiler.Compilation

open System.Collections.Immutable
open FSharp.Compiler
open FSharp.Compiler.AbstractIL.ILBinaryReader

[<NoEquality;NoComparison>]
type CompilationOptions =
    {
        LegacyReferenceResolver: ReferenceResolver.Resolver
        DefaultFSharpBinariesDir: string
        TryGetMetadataSnapshot: ILReaderTryGetMetadataSnapshot
        SuggestNamesForErrors: bool
        CommandLineArgs: string list
        ProjectDirectory: string
        UseScriptResolutionRules: bool
        AssemblyPath: string
        IsExecutable: bool
        KeepAssemblyContents: bool
        KeepAllBackgroundResolutions: bool
        SourceSnapshots: ImmutableArray<SourceSnapshot>
        CompilationReferences: ImmutableArray<Compilation>
    }

    static member Create: assemblyPath: string * projectDirectory: string * ImmutableArray<SourceSnapshot> * ImmutableArray<Compilation> -> CompilationOptions

and [<Sealed>] Compilation =

    member CheckAsync: filePath: string -> Async<unit>

[<RequireQualifiedAccess>]
module internal Compilation =

    val create: CompilationOptions -> FrameworkImportsCache -> Compilation
