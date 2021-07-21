// Copyright (c) Microsoft Corporation. All Rights Reserved. See License.txt in the project root for license information.

/// Write Abstract IL structures at runtime using Reflection.Emit
module internal FSharp.Compiler.AbstractIL.ILRuntimeWriter    

open System.Reflection
open System.Reflection.Emit

open FSharp.Compiler.AbstractIL.IL

val mkDynamicAssemblyAndModule: assemblyName: string * optimize: bool * debugInfo: bool * collectible: bool -> AssemblyBuilder * ModuleBuilder

type cenv = 
    { ilg: ILGlobals
      emitTailcalls: bool
      tryFindSysILTypeRef: string -> ILTypeRef option
      generatePdb: bool
      resolveAssemblyRef: ILAssemblyRef -> Choice<string, Assembly> option }

type emEnv

val emEnv0: emEnv

val emitModuleFragment: 
    ilg: ILGlobals *
    emitTailcalls: bool *
    emEnv: emEnv *
    asmB: AssemblyBuilder *
    modB: ModuleBuilder *
    modul: ILModuleDef *
    debugInfo: bool *
    resolveAssemblyRef: (ILAssemblyRef -> Choice<string,Assembly> option) *
    tryFindSysILTypeRef: (string -> ILTypeRef option) ->
      emEnv * (unit -> exn option) list

val LookupTypeRef: cenv: cenv -> emEnv: emEnv -> tref: ILTypeRef -> System.Type

val LookupType: cenv: cenv -> emEnv: emEnv -> ty: ILType -> System.Type

val LookupFieldRef: emEnv: emEnv -> fref: ILFieldRef -> FieldInfo option

val LookupMethodRef: emEnv: emEnv -> mref: ILMethodRef -> MethodInfo option
