// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module internal FSharp.Compiler.AbstractIL.AsciiConstants

open Internal.Utilities.Collections
open Internal.Utilities.Library
open FSharp.Compiler.AbstractIL.IL

/// Table of parsing and pretty printing data for instructions.
let noArgInstrs =
    lazy [
        ["ldc";"i4";"0"], mkLdcInt32 0
        ["ldc";"i4";"1"], mkLdcInt32 1
        ["ldc";"i4";"2"], mkLdcInt32 2
        ["ldc";"i4";"3"], mkLdcInt32 3
        ["ldc";"i4";"4"], mkLdcInt32 4
        ["ldc";"i4";"5"], mkLdcInt32 5
        ["ldc";"i4";"6"], mkLdcInt32 6
        ["ldc";"i4";"7"], mkLdcInt32 7
        ["ldc";"i4";"8"], mkLdcInt32 8
        ["ldc";"i4";"M1"], mkLdcInt32 -1
        ["ldc";"i4";"m1"], mkLdcInt32 -1
        ["stloc";"0"], mkStloc (uint16 0)
        ["stloc";"1"], mkStloc (uint16 1)
        ["stloc";"2"], mkStloc (uint16 2)
        ["stloc";"3"], mkStloc (uint16 3)
        ["ldloc";"0"], mkLdloc (uint16 0)
        ["ldloc";"1"], mkLdloc (uint16 1)
        ["ldloc";"2"], mkLdloc (uint16 2)
        ["ldloc";"3"], mkLdloc (uint16 3)
        ["ldarg";"0"], mkLdarg (uint16 0)
        ["ldarg";"1"], mkLdarg (uint16 1)
        ["ldarg";"2"], mkLdarg (uint16 2)
        ["ldarg";"3"], mkLdarg (uint16 3)
        ["ret"], I_ret
        ["add"], AI_add
        ["add";"ovf"], AI_add_ovf
        ["add";"ovf";"un"], AI_add_ovf_un
        ["and"], AI_and
        ["div"], AI_div
        ["div";"un"], AI_div_un
        ["ceq"], AI_ceq
        ["cgt"], AI_cgt
        ["cgt";"un"], AI_cgt_un
        ["clt"], AI_clt
        ["clt";"un"], AI_clt_un
        ["conv";"i1"], AI_conv DT_I1
        ["conv";"i2"], AI_conv DT_I2
        ["conv";"i4"], AI_conv DT_I4
        ["conv";"i8"], AI_conv DT_I8
        ["conv";"i"], AI_conv DT_I
        ["conv";"r4"], AI_conv DT_R4
        ["conv";"r8"], AI_conv DT_R8
        ["conv";"u1"], AI_conv DT_U1
        ["conv";"u2"], AI_conv DT_U2
        ["conv";"u4"], AI_conv DT_U4
        ["conv";"u8"], AI_conv DT_U8
        ["conv";"u"], AI_conv DT_U
        ["conv";"r"; "un"], AI_conv DT_R
        ["conv";"ovf";"i1"], AI_conv_ovf DT_I1
        ["conv";"ovf";"i2"], AI_conv_ovf DT_I2
        ["conv";"ovf";"i4"], AI_conv_ovf DT_I4
        ["conv";"ovf";"i8"], AI_conv_ovf DT_I8
        ["conv";"ovf";"i"], AI_conv_ovf DT_I
        ["conv";"ovf";"u1"], AI_conv_ovf DT_U1
        ["conv";"ovf";"u2"], AI_conv_ovf DT_U2
        ["conv";"ovf";"u4"], AI_conv_ovf DT_U4
        ["conv";"ovf";"u8"], AI_conv_ovf DT_U8
        ["conv";"ovf";"u"], AI_conv_ovf DT_U
        ["conv";"ovf";"i1"; "un"], AI_conv_ovf_un DT_I1
        ["conv";"ovf";"i2"; "un"], AI_conv_ovf_un DT_I2
        ["conv";"ovf";"i4"; "un"], AI_conv_ovf_un DT_I4
        ["conv";"ovf";"i8"; "un"], AI_conv_ovf_un DT_I8
        ["conv";"ovf";"i"; "un"], AI_conv_ovf_un DT_I
        ["conv";"ovf";"u1"; "un"], AI_conv_ovf_un DT_U1
        ["conv";"ovf";"u2"; "un"], AI_conv_ovf_un DT_U2
        ["conv";"ovf";"u4"; "un"], AI_conv_ovf_un DT_U4
        ["conv";"ovf";"u8"; "un"], AI_conv_ovf_un DT_U8
        ["conv";"ovf";"u"; "un"], AI_conv_ovf_un DT_U
        ["stelem";"i1"], I_stelem DT_I1
        ["stelem";"i2"], I_stelem DT_I2
        ["stelem";"i4"], I_stelem DT_I4
        ["stelem";"i8"], I_stelem DT_I8
        ["stelem";"r4"], I_stelem DT_R4
        ["stelem";"r8"], I_stelem DT_R8
        ["stelem";"i"], I_stelem DT_I
        ["stelem";"u"], I_stelem DT_I
        ["stelem";"u8"], I_stelem DT_I8
        ["stelem";"ref"], I_stelem DT_REF
        ["ldelem";"i1"], I_ldelem DT_I1
        ["ldelem";"i2"], I_ldelem DT_I2
        ["ldelem";"i4"], I_ldelem DT_I4
        ["ldelem";"i8"], I_ldelem DT_I8
        ["ldelem";"u8"], I_ldelem DT_I8
        ["ldelem";"u1"], I_ldelem DT_U1
        ["ldelem";"u2"], I_ldelem DT_U2
        ["ldelem";"u4"], I_ldelem DT_U4
        ["ldelem";"r4"], I_ldelem DT_R4
        ["ldelem";"r8"], I_ldelem DT_R8
        ["ldelem";"u"], I_ldelem DT_I // EQUIV
        ["ldelem";"i"], I_ldelem DT_I
        ["ldelem";"ref"], I_ldelem DT_REF
        ["mul"], AI_mul
        ["mul";"ovf"], AI_mul_ovf
        ["mul";"ovf";"un"], AI_mul_ovf_un
        ["rem"], AI_rem
        ["rem";"un"], AI_rem_un 
        ["shl"], AI_shl 
        ["shr"], AI_shr 
        ["shr";"un"], AI_shr_un
        ["sub"], AI_sub
        ["sub";"ovf"], AI_sub_ovf
        ["sub";"ovf";"un"], AI_sub_ovf_un
        ["xor"], AI_xor
        ["or"], AI_or
        ["neg"], AI_neg
        ["not"], AI_not
        ["ldnull"], AI_ldnull
        ["dup"], AI_dup
        ["pop"], AI_pop
        ["ckfinite"], AI_ckfinite
        ["nop"], AI_nop
        ["break"], I_break
        ["arglist"], I_arglist
        ["endfilter"], I_endfilter
        ["endfinally"], I_endfinally
        ["refanytype"], I_refanytype
        ["localloc"], I_localloc
        ["throw"], I_throw
        ["ldlen"], I_ldlen
        ["rethrow"], I_rethrow
    ]

#if DEBUG
let wordsOfNoArgInstr, isNoArgInstr =
    let t =
      lazy
        (let t = HashMultiMap(300, HashIdentity.Structural)
         noArgInstrs |> Lazy.force |> List.iter (fun (x, mk) -> t.Add(mk, x))
         t)
    (fun s -> (Lazy.force t).[s]),
    (fun s -> (Lazy.force t).ContainsKey s)
#endif

let mk_stind (nm, dt) =  (nm, (fun () -> I_stind(Aligned, Nonvolatile, dt)))
let mk_ldind (nm, dt) =  (nm, (fun () -> I_ldind(Aligned, Nonvolatile, dt)))

type NoArgInstr = unit -> ILInstr
type Int32Instr = int32 ->  ILInstr
type Int32Int32Instr = int32 * int32 ->  ILInstr
type Int64Instr = int64 ->  ILInstr
type DoubleInstr = ILConst ->  ILInstr
type MethodSpecInstr = ILMethodSpec * ILVarArgs ->  ILInstr
type TypeInstr = ILType ->  ILInstr
type IntTypeInstr = int * ILType ->  ILInstr
type ValueTypeInstr = ILType ->  ILInstr  (* nb. diff. interp of types to TypeInstr *)
type StringInstr = string ->  ILInstr
type TokenInstr = ILToken ->  ILInstr
type SwitchInstr = ILCodeLabel list * ILCodeLabel ->  ILInstr

type InstrTable<'T> = (string list * 'T) list
type LazyInstrTable<'T> = Lazy<InstrTable<'T>>

/// Table of parsing and pretty printing data for instructions.
let NoArgInstrs : Lazy<InstrTable<NoArgInstr>> =
    lazy [ 
        for nm, i in noArgInstrs.Force() do  
             yield (nm, (fun () -> i))
        yield mk_stind (["stind";"u"], DT_I)
        yield mk_stind (["stind";"i"], DT_I)
        yield mk_stind (["stind";"u1"], DT_I1)  
        yield mk_stind (["stind";"i1"], DT_I1)
        yield mk_stind (["stind";"u2"], DT_I2)
        yield mk_stind (["stind";"i2"], DT_I2)
        yield mk_stind (["stind";"u4"], DT_I4)
        yield mk_stind (["stind";"i4"], DT_I4)
        yield mk_stind (["stind";"u8"], DT_I8)
        yield mk_stind (["stind";"i8"], DT_I8)
        yield mk_stind (["stind";"r4"], DT_R4)
        yield mk_stind (["stind";"r8"], DT_R8)
        yield mk_stind (["stind";"ref"], DT_REF)
        yield mk_ldind (["ldind";"i"], DT_I)
        yield mk_ldind (["ldind";"i1"], DT_I1)
        yield mk_ldind (["ldind";"i2"], DT_I2)
        yield mk_ldind (["ldind";"i4"], DT_I4)
        yield mk_ldind (["ldind";"i8"], DT_I8)
        yield mk_ldind (["ldind";"u1"], DT_U1)
        yield mk_ldind (["ldind";"u2"], DT_U2)
        yield mk_ldind (["ldind";"u4"], DT_U4)
        yield mk_ldind (["ldind";"u8"], DT_I8)
        yield mk_ldind (["ldind";"r4"], DT_R4)
        yield mk_ldind (["ldind";"r8"], DT_R8)
        yield mk_ldind (["ldind";"ref"], DT_REF)
        yield ["cpblk"], (fun () -> I_cpblk(Aligned, Nonvolatile))
        yield ["initblk"], (fun () -> I_initblk(Aligned, Nonvolatile))
    ]

/// Table of parsing and pretty printing data for instructions.
let Int64Instrs : Lazy<InstrTable<Int64Instr>> =
    lazy [
        ["ldc";"i8"], (fun x -> AI_ldc (DT_I8, ILConst.I8 x)) 
    ] 

/// Table of parsing and pretty printing data for instructions.
let Int32Instrs : Lazy<InstrTable<Int32Instr>> =
    lazy [ 
        ["ldc";"i4"], mkLdcInt32
        ["ldc";"i4";"s"], mkLdcInt32 
    ] 

/// Table of parsing and pretty printing data for instructions.
let Int32Int32Instrs : Lazy<InstrTable<Int32Int32Instr>> =
    lazy [
        ["ldlen";"multi"], EI_ldlen_multi
    ]

/// Table of parsing and pretty printing data for instructions.
let DoubleInstrs : Lazy<InstrTable<DoubleInstr>> =
    lazy [
        ["ldc";"r4"], (fun x -> (AI_ldc (DT_R4, x)))
        ["ldc";"r8"], (fun x -> (AI_ldc (DT_R8, x))) 
    ]

/// Table of parsing and pretty printing data for instructions.
let StringInstrs : Lazy<InstrTable<StringInstr>> =
    lazy [
        ["ldstr"], I_ldstr
    ] 

/// Table of parsing and pretty printing data for instructions.
let TokenInstrs : Lazy<InstrTable<TokenInstr>> =
    lazy [
        ["ldtoken"], I_ldtoken
    ]

/// Table of parsing and pretty printing data for instructions.
let TypeInstrs : Lazy<InstrTable<TypeInstr>> =
    lazy [
        ["ldelema"], (fun x -> I_ldelema (NormalAddress, false, ILArrayShape.SingleDimensional, x))
        ["ldelem";"any"], (fun x -> I_ldelem_any (ILArrayShape.SingleDimensional, x))
        ["stelem";"any"], (fun x -> I_stelem_any (ILArrayShape.SingleDimensional, x))
        ["newarr"], (fun x -> I_newarr (ILArrayShape.SingleDimensional, x))
        ["castclass"], I_castclass
        ["ilzero"], EI_ilzero
        ["isinst"], I_isinst
        ["initobj";"any"], I_initobj
        ["unbox";"any"], I_unbox_any
    ] 

/// Table of parsing and pretty printing data for instructions.
let IntTypeInstrs : Lazy<InstrTable<IntTypeInstr>> =
    lazy [
        ["ldelem";"multi"], (fun (x, y) -> (I_ldelem_any (ILArrayShape.FromRank x, y)))
        ["stelem";"multi"], (fun (x, y) -> (I_stelem_any (ILArrayShape.FromRank x, y)))
        ["newarr";"multi"], (fun (x, y) -> (I_newarr (ILArrayShape.FromRank x, y)))
        ["ldelema";"multi"], (fun (x, y) -> (I_ldelema (NormalAddress, false, ILArrayShape.FromRank x, y))) 
    ] 

/// Table of parsing and pretty printing data for instructions.
let ValueTypeInstrs : Lazy<InstrTable<ValueTypeInstr>> =
    lazy [ 
        ["cpobj"], I_cpobj
        ["initobj"], I_initobj
        ["ldobj"], (fun z -> I_ldobj (Aligned, Nonvolatile, z))
        ["stobj"], (fun z -> I_stobj (Aligned, Nonvolatile, z))
        ["sizeof"], I_sizeof
        ["box"], I_box
        ["unbox"], I_unbox 
    ] 

