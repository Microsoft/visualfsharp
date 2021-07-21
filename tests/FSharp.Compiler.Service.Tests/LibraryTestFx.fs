// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module Tests.Service.SurfaceArea.LibraryTestFx

open System
open System.Collections.Generic
open System.IO
open System.Reflection
open System.Text.RegularExpressions

open FSharp.Compiler.IO

open NUnit.Framework

// Verifies two sequences are equal (same length, equiv elements)
let VerifySeqsEqual (seq1 : seq<'T>) (seq2 : seq<'T>) =
    Assert.Equals(seq1, seq2)

let sleep(n : int32) =
    System.Threading.Thread.Sleep(n)

module SurfaceArea =

    // gets string form of public surface area for FSharp.CompilerService
    let private getActual () =

        // get local FSharp.CompilerService
        let path = Path.Combine(Path.GetDirectoryName(typeof<int list>.Assembly.Location), "FSharp.Compiler.Service.dll")
        let name = AssemblyName.GetAssemblyName path
        let asm = Assembly.Load(name)

        // public types only
        let types = asm.ExportedTypes |> Seq.filter (fun ty -> let ti = ty.GetTypeInfo() in ti.IsPublic || ti.IsNestedPublic) |> Array.ofSeq

        // extract canonical string form for every public member of every type
        let getTypeMemberStrings (t : Type) =
            // for System.Runtime-based profiles, need to do lots of manual work
            let getMembers (t : Type) =
                let ti = t.GetTypeInfo()
                let cast (info: #MemberInfo) = (t, info :> MemberInfo)
                seq {
                    yield! ti.DeclaredEvents |> Seq.filter (fun m -> m.AddMethod.IsPublic) |> Seq.map cast
                    yield! ti.DeclaredProperties |> Seq.filter (fun m -> m.GetMethod.IsPublic) |> Seq.map cast
                    yield! ti.DeclaredMethods |> Seq.filter (fun m -> m.IsPublic) |> Seq.map cast
                    yield! ti.DeclaredFields |> Seq.filter (fun m -> m.IsPublic) |> Seq.map cast
                    yield! ti.DeclaredConstructors  |> Seq.filter (fun m -> m.IsPublic) |> Seq.map cast
                    yield! ti.DeclaredNestedTypes   |> Seq.filter (fun ty -> ty.IsNestedPublic) |> Seq.map cast
                } |> Array.ofSeq

            [| for ty,m in getMembers t do
                  yield sprintf "%s: %s" (ty.ToString()) (m.ToString())
               if not t.IsNested then
                   yield t.ToString()
            |]

        let actual =
            types 
            |> Array.collect getTypeMemberStrings
            |> Array.sort
            |> String.concat Environment.NewLine
        asm, actual

    // verify public surface area matches expected
    let verify expectedFile actualFile =
      let normalize text = Regex.Replace(text, "(\\r\\n|\\n|\\r)+", "\r\n").Trim()
      let _asm, actual = getActual ()
      let actual = normalize actual
      let expected = normalize (System.IO.File.ReadAllText expectedFile)
      match Tests.TestHelpers.assembleDiffMessage actual expected with
      | None -> ()
      | Some diff ->
          FileSystem
            .OpenFileForWriteShim(actualFile)
            .WriteAllText(actual)

          failwith
            $"surface area defined in\n\n{expectedFile}\n\ndoesn't match actual in\n\n{actualFile}\n\nCompare the files and adjust accordingly.
            {diff}"