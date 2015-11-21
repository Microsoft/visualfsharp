// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

module internal Microsoft.FSharp.Compiler.Driver 

open Microsoft.FSharp.Compiler.ErrorLogger
open Microsoft.FSharp.Compiler.CompileOps
open Microsoft.FSharp.Compiler.TcGlobals
open Microsoft.FSharp.Compiler.Tast
open Microsoft.FSharp.Compiler.TypeChecker

/// F# IDE tools can invoke this to obtain the set of referenced assemblies which contain type provider definitions
val internal ContainsTypeProvider : string -> bool

#if NO_COMPILER_BACKEND
#else

/// fsc.exe calls this
val mainCompile : argv : string[] * bannerAlreadyPrinted : bool * exiter : Exiter -> unit

[<RequireQualifiedAccess>]
type CompilationOutput = 
    { Errors : ErrorOrWarning[]
      Warnings : ErrorOrWarning[] }

type InProcCompiler = 
    new : unit -> InProcCompiler
    member Compile : args : string[] -> bool * CompilationOutput

#endif
