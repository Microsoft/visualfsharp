
//  Microsoft (R) .NET Framework IL Disassembler.  Version 4.7.3081.0
//  Copyright (c) Microsoft Corporation.  All rights reserved.



// Metadata version: v4.0.30319
.assembly extern mscorlib
{
  .publickeytoken = (B7 7A 5C 56 19 34 E0 89 )                         // .z\V.4..
  .ver 4:0:0:0
}
.assembly extern FSharp.Core
{
  .publickeytoken = (B0 3F 5F 7F 11 D5 0A 3A )                         // .?_....:
  .ver 4:7:0:0
}
.assembly Hash02
{
  .custom instance void [FSharp.Core]Microsoft.FSharp.Core.FSharpInterfaceDataVersionAttribute::.ctor(int32,
                                                                                                      int32,
                                                                                                      int32) = ( 01 00 02 00 00 00 00 00 00 00 00 00 00 00 00 00 ) 

  // --- The following custom attribute is added automatically, do not uncomment -------
  //  .custom instance void [mscorlib]System.Diagnostics.DebuggableAttribute::.ctor(valuetype [mscorlib]System.Diagnostics.DebuggableAttribute/DebuggingModes) = ( 01 00 01 00 00 00 00 00 ) 

  .hash algorithm 0x00008004
  .ver 0:0:0:0
}
.mresource public FSharpSignatureData.Hash02
{
  // Offset: 0x00000000 Length: 0x0000021C
}
.mresource public FSharpSignatureDataB.Hash02
{
  // Offset: 0x00000220 Length: 0x00000003
}
.mresource public FSharpOptimizationData.Hash02
{
  // Offset: 0x00000228 Length: 0x0000010B
}
.mresource public FSharpOptimizationDataB.Hash02
{
  // Offset: 0x00000338 Length: 0x00000006
}
.module Hash02.dll
// MVID: {5E1730AF-9642-796E-A745-0383AF30175E}
.imagebase 0x00400000
.file alignment 0x00000200
.stackreserve 0x00100000
.subsystem 0x0003       // WINDOWS_CUI
.corflags 0x00000001    //  ILONLY
// Image base: 0x06F70000


// =============== CLASS MEMBERS DECLARATION ===================

.class public abstract auto ansi sealed Hash02
       extends [mscorlib]System.Object
{
  .custom instance void [FSharp.Core]Microsoft.FSharp.Core.CompilationMappingAttribute::.ctor(valuetype [FSharp.Core]Microsoft.FSharp.Core.SourceConstructFlags) = ( 01 00 07 00 00 00 00 00 ) 
  .class abstract auto ansi sealed nested public HashMicroPerfAndCodeGenerationTests
         extends [mscorlib]System.Object
  {
    .custom instance void [FSharp.Core]Microsoft.FSharp.Core.CompilationMappingAttribute::.ctor(valuetype [FSharp.Core]Microsoft.FSharp.Core.SourceConstructFlags) = ( 01 00 07 00 00 00 00 00 ) 
    .method public static void  f4_triple() cil managed
    {
      // Code size       1 (0x1)
      .maxstack  8
      .language '{AB4F38C9-B6E6-43BA-BE3B-58080B2CCCE3}', '{994B45C4-E6E9-11D2-903F-00C04FA302A1}', '{5A869D0B-6611-11D3-BD2A-0000F80849BD}'
      .line 7,8 : 8,29 'C:\\GitHub\\dsyme\\fsharp\\tests\\fsharpqa\\source\\Optimizations\\GenericComparison\\Hash02.fsx'
      IL_0000:  ret
    } // end of method HashMicroPerfAndCodeGenerationTests::f4_triple

  } // end of class HashMicroPerfAndCodeGenerationTests

} // end of class Hash02

.class private abstract auto ansi sealed '<StartupCode$Hash02>'.$Hash02$fsx
       extends [mscorlib]System.Object
{
} // end of class '<StartupCode$Hash02>'.$Hash02$fsx


// =============================================================

// *********** DISASSEMBLY COMPLETE ***********************
