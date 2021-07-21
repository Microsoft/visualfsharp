// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

/// Defines the global environment for all type checking.

module FSharp.Compiler.CompilerGlobalState

open System.Collections.Generic
open FSharp.Compiler.Syntax.PrettyNaming
open FSharp.Compiler.Text

/// Generates compiler-generated names. Each name generated also includes the StartLine number of the range passed in
/// at the point of first generation.
///
/// This type may be accessed concurrently, though in practice it is only used from the compilation thread.
/// It is made concurrency-safe since a global instance of the type is allocated in tast.fs, and it is good
/// policy to make all globally-allocated objects concurrency safe in case future versions of the compiler
/// are used to host multiple concurrent instances of compilation.
type NiceNameGenerator() =

    let lockObj = obj()
    let basicNameCounts = new Dictionary<string, int>(100)

    member x.FreshCompilerGeneratedName (name, m: range) =
      lock lockObj (fun () ->
        let basicName = GetBasicNameOfPossibleCompilerGeneratedName name
        let n =
            match basicNameCounts.TryGetValue basicName with
            | true, count -> count
            | _ -> 0
        let nm = CompilerGeneratedNameSuffix basicName (string m.StartLine + (match n with 0 -> "" | n -> "-" + string n))
        basicNameCounts.[basicName] <- n + 1
        nm)

    member x.Reset () =
      lock lockObj (fun () ->
        basicNameCounts.Clear()
      )

/// Generates compiler-generated names marked up with a source code location, but if given the same unique value then
/// return precisely the same name. Each name generated also includes the StartLine number of the range passed in
/// at the point of first generation.
///
/// This type may be accessed concurrently, though in practice it is only used from the compilation thread.
/// It is made concurrency-safe since a global instance of the type is allocated in tast.fs.
type StableNiceNameGenerator() =

    let lockObj = obj()

    let names = new Dictionary<string * int64, string>(100)
    let basicNameCounts = new Dictionary<string, int>(100)

    member x.GetUniqueCompilerGeneratedName (name, m: range, uniq) =
        lock lockObj (fun () ->
            let basicName = GetBasicNameOfPossibleCompilerGeneratedName name
            let key = basicName, uniq
            match names.TryGetValue key with
            | true, nm -> nm
            | _ ->
                let n =
                    match basicNameCounts.TryGetValue basicName with
                    | true, c -> c
                    | _ -> 0
                let nm = CompilerGeneratedNameSuffix basicName (string m.StartLine + (match n with 0 -> "" | n -> "-" + string n))
                names.[key] <- nm
                basicNameCounts.[basicName] <- n + 1
                nm
        )

    member x.Reset () =
      lock lockObj (fun () ->
        basicNameCounts.Clear()
        names.Clear()
      )

type internal CompilerGlobalState () =
    /// A global generator of compiler generated names
    let globalNng = NiceNameGenerator()

    /// A global generator of stable compiler generated names
    let globalStableNameGenerator = StableNiceNameGenerator ()

    /// A name generator used by IlxGen for static fields, some generated arguments and other things.
    let ilxgenGlobalNng = NiceNameGenerator ()

    member _.NiceNameGenerator = globalNng

    member _.StableNameGenerator = globalStableNameGenerator

    member _.IlxGenNiceNameGenerator = ilxgenGlobalNng

/// Unique name generator for stamps attached to lambdas and object expressions
type Unique = int64

//++GLOBAL MUTABLE STATE (concurrency-safe)
let newUnique =
    let i = ref 0L
    fun () -> System.Threading.Interlocked.Increment i

/// Unique name generator for stamps attached to to val_specs, tycon_specs etc.
//++GLOBAL MUTABLE STATE (concurrency-safe)
let newStamp =
    let i = ref 0L
    fun () -> System.Threading.Interlocked.Increment i
