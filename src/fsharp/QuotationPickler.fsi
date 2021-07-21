// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

/// Code to pickle out quotations in the quotation binary format.
module internal FSharp.Compiler.QuotationPickler
#nowarn "1178" // The struct, record or union type 'internal_instr_extension' is not structurally comparable because the type

type TypeData 

type TypeVarData = { tvName: string }

type NamedTypeData = 
    /// Indicates an F# 4.0+ reference into the supplied table of type definition references, ultimately resolved by TypeRef/TypeDef data 
    | Idx of int
    /// Indicates an F# 3.0+ reference to a named type in an assembly loaded by name
    | Named of tcName: string *  tcAssembly:  string 

val mkVarTy : int -> TypeData 

val mkFunTy : TypeData * TypeData -> TypeData

val mkArrayTy : int * TypeData -> TypeData 

val mkILNamedTy : NamedTypeData * TypeData list -> TypeData 

type ExprData

type VarData 

type CtorData = 
    { ctorParent: NamedTypeData 
      ctorArgTypes: TypeData list }

type MethodData = 
    { methParent: NamedTypeData
      methName: string
      methArgTypes: TypeData list
      methRetType: TypeData 
      numGenericArgs: int }

type ModuleDefnData = 
    { Module: NamedTypeData
      Name: string
      IsProperty: bool }

type MethodBaseData = 
    | ModuleDefn of ModuleDefnData * (string * int) option
    | Method     of MethodData
    | Ctor       of CtorData

type PropInfoData  = NamedTypeData * string * TypeData * TypeData list

val mkVar    : int -> ExprData 
val mkThisVar    : TypeData -> ExprData 
val mkHole   : TypeData * int -> ExprData 
val mkApp    : ExprData * ExprData -> ExprData 
val mkLambda : VarData * ExprData -> ExprData 
val mkQuote  : ExprData -> ExprData 
val mkQuoteRaw40  : ExprData -> ExprData  // only available for FSharp.Core 4.4.0.0+
val mkCond   : ExprData * ExprData * ExprData -> ExprData 
val mkModuleValueApp : NamedTypeData * string * bool * TypeData list * ExprData list -> ExprData 
val mkModuleValueWApp : NamedTypeData * string * bool * string * int * TypeData list * ExprData list -> ExprData 
val mkLetRec : (VarData * ExprData) list * ExprData -> ExprData 
val mkLet : (VarData * ExprData) * ExprData -> ExprData
val mkRecdMk : NamedTypeData  * TypeData list * ExprData list -> ExprData
val mkRecdGet : NamedTypeData * string * TypeData list * ExprData list -> ExprData 
val mkRecdSet :  NamedTypeData * string * TypeData list * ExprData list -> ExprData 
val mkUnion : NamedTypeData * string * TypeData list * ExprData list -> ExprData 
val mkUnionFieldGet : NamedTypeData * string * int * TypeData list * ExprData -> ExprData  
val mkUnionCaseTagTest : NamedTypeData * string * TypeData list * ExprData -> ExprData  
val mkTuple : TypeData * ExprData list -> ExprData 
val mkTupleGet : TypeData * int * ExprData -> ExprData
val mkCoerce : TypeData * ExprData -> ExprData 
val mkNewArray : TypeData * ExprData list -> ExprData 
val mkTypeTest : TypeData * ExprData -> ExprData 
val mkAddressSet : ExprData * ExprData -> ExprData 
val mkVarSet : ExprData * ExprData -> ExprData 
val mkUnit : unit -> ExprData 
val mkNull : TypeData -> ExprData 
val mkDefaultValue : TypeData -> ExprData 
val mkBool : bool * TypeData -> ExprData 
val mkString : string  * TypeData -> ExprData 
val mkSingle : float32  * TypeData -> ExprData 
val mkDouble : float  * TypeData -> ExprData 
val mkChar : char  * TypeData -> ExprData 
val mkSByte : sbyte  * TypeData -> ExprData 
val mkByte : byte  * TypeData -> ExprData 
val mkInt16 : int16  * TypeData -> ExprData 
val mkUInt16 : uint16  * TypeData -> ExprData 
val mkInt32 : int32  * TypeData -> ExprData 
val mkUInt32 : uint32  * TypeData -> ExprData 
val mkInt64 : int64  * TypeData -> ExprData 
val mkUInt64 : uint64  * TypeData -> ExprData 
val mkAddressOf : ExprData -> ExprData
val mkSequential : ExprData * ExprData -> ExprData 
val mkForLoop : ExprData * ExprData * ExprData -> ExprData 
val mkWhileLoop : ExprData * ExprData -> ExprData 
val mkTryFinally : ExprData * ExprData -> ExprData 
val mkTryWith : ExprData * VarData * ExprData * VarData * ExprData -> ExprData 
val mkDelegate : TypeData * ExprData -> ExprData 
val mkPropGet : PropInfoData   * TypeData list * ExprData list -> ExprData   
val mkPropSet : PropInfoData   * TypeData list * ExprData list -> ExprData   
val mkFieldGet : NamedTypeData * string * TypeData list * ExprData list -> ExprData  
val mkFieldSet : NamedTypeData * string * TypeData list * ExprData list -> ExprData  
val mkCtorCall : CtorData * TypeData list * ExprData list -> ExprData 
val mkMethodCall : MethodData * TypeData list * ExprData list -> ExprData 
val mkMethodCallW : MethodData * MethodData * int * TypeData list * ExprData list -> ExprData 

val mkAttributedExpression : ExprData * ExprData -> ExprData 
val pickle : (ExprData -> byte[]) 
val isAttributedExpression : ExprData -> bool
    
val PickleDefns : ((MethodBaseData * ExprData) list -> byte[]) 
val SerializedReflectedDefinitionsResourceNameBase : string
val freshVar : string * TypeData * bool -> VarData

