// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module internal FSharp.Compiler.OptimizeInputs

open Internal.Utilities.Library
open FSharp.Compiler.AbstractIL.IL
open FSharp.Compiler.CheckDeclarations
open FSharp.Compiler.CompilerConfig
open FSharp.Compiler.CompilerImports
open FSharp.Compiler.IlxGen
open FSharp.Compiler.Import
open FSharp.Compiler.Optimizer
open FSharp.Compiler.TcGlobals
open FSharp.Compiler.TypedTree

val GetGeneratedILModuleName : CompilerTarget -> string -> string

val GetInitialOptimizationEnv : TcImports * TcGlobals -> IncrementalOptimizationEnv

val AddExternalCcuToOptimizationEnv : TcGlobals -> IncrementalOptimizationEnv -> ImportedAssembly -> IncrementalOptimizationEnv

val ApplyAllOptimizations : TcConfig * TcGlobals * ConstraintSolver.TcValF * string * ImportMap * bool * IncrementalOptimizationEnv * CcuThunk * TypedImplFile list -> TypedAssemblyAfterOptimization * LazyModuleInfo * IncrementalOptimizationEnv 

val CreateIlxAssemblyGenerator : TcConfig * TcImports * TcGlobals * ConstraintSolver.TcValF * CcuThunk -> IlxAssemblyGenerator

val GenerateIlxCode : IlxGenBackend * isInteractiveItExpr:bool * isInteractiveOnMono:bool * TcConfig * TopAttribs * TypedAssemblyAfterOptimization * fragName:string * IlxAssemblyGenerator -> IlxGenResults

// Used during static linking
val NormalizeAssemblyRefs : CompilationThreadToken * ILGlobals * TcImports -> (ILScopeRef -> ILScopeRef)

val GetGeneratedILModuleName : CompilerTarget -> string -> string
