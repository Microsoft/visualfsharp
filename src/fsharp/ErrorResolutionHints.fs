// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

/// Functions to format error message details
module internal FSharp.Compiler.ErrorResolutionHints

open Internal.Utilities
open Internal.Utilities.Library
open System.Collections
open System.Collections.Generic

let maxSuggestions = 5

let minThresholdForSuggestions = 0.7

let highConfidenceThreshold = 0.85

let minStringLengthForSuggestion = 3

/// We report a candidate if its edit distance is <= the threshold.
/// The threshold is set to about a quarter of the number of characters.
let IsInEditDistanceProximity idText suggestion =
    let editDistance = EditDistance.CalcEditDistance(idText, suggestion)
    let threshold =
        match idText.Length with
        | x when x < 5 -> 1
        | x when x < 7 -> 2
        | x -> x / 4 + 1
    
    editDistance <= threshold

/// Demangles a suggestion
let DemangleOperator (nm: string) =
    if nm.StartsWithOrdinal("( ") && nm.EndsWithOrdinal(" )") then
        nm.[2..nm.Length - 3]
    else 
        nm

type SuggestionBufferEnumerator(tail: int, data: KeyValuePair<float,string> []) =
    let mutable current = data.Length
    interface IEnumerator<string> with
        member _.Current 
            with get () = 
                let kvpr = &data.[current]
                kvpr.Value
    interface IEnumerator with
        member _.Current with get () = box data.[current].Value
        member _.MoveNext() =
            current <- current - 1
            current > tail || (current = tail && data.[current] <> Unchecked.defaultof<_>)
        member _.Reset () = current <- data.Length
    interface System.IDisposable with
        member _.Dispose () = ()

type SuggestionBuffer(idText: string) = 
    let data = Array.zeroCreate<KeyValuePair<float,string>>(maxSuggestions)
    let mutable tail = maxSuggestions - 1
    let uppercaseText = idText.ToUpperInvariant()
    let dotIdText = "." + idText
    let mutable disableSuggestions = idText.Length < minStringLengthForSuggestion

    let insert (k,v) = 
        let mutable pos = tail
        while pos < maxSuggestions && (let kv = &data.[pos] in kv.Key < k) do
            pos <- pos + 1

        if pos > 0 then
            if pos >= maxSuggestions || (let kv = &data.[pos] in k <> kv.Key || v <> kv.Value) then
                if tail < pos - 1 then
                    for i = tail to pos - 2 do 
                        data.[i] <- data.[i + 1]
                data.[pos - 1] <- KeyValuePair(k,v)
                if tail > 0 then tail <- tail - 1

    member _.Add (suggestion: string) =
        if not disableSuggestions then
            if suggestion = idText then // some other parse error happened
                disableSuggestions <- true

            // Because beginning a name with _ is used both to indicate an unused
            // value as well as to formally squelch the associated compiler
            // error/warning (FS1182), we remove such names from the suggestions,
            // both to prevent accidental usages as well as to encourage good taste
            if suggestion.Length >= minStringLengthForSuggestion && not (suggestion.StartsWithOrdinal "_") then
                let suggestion:string = DemangleOperator suggestion
                let suggestedText = suggestion.ToUpperInvariant()
                let similarity = EditDistance.JaroWinklerDistance uppercaseText suggestedText
                if similarity >= highConfidenceThreshold ||
                    suggestion.EndsWithOrdinal dotIdText ||
                    (similarity >= minThresholdForSuggestions && IsInEditDistanceProximity uppercaseText suggestedText)
                then
                    insert(similarity, suggestion) |> ignore
    
    member _.Disabled with get () = disableSuggestions

    member _.IsEmpty with get () = disableSuggestions || (tail = maxSuggestions - 1)

    interface IEnumerable<string> with
        member this.GetEnumerator () = 
            if this.IsEmpty then
                Seq.empty.GetEnumerator()
            else
                new SuggestionBufferEnumerator(tail, data) :> IEnumerator<string>

    interface IEnumerable with
        member this.GetEnumerator () = 
            if this.IsEmpty then
                Seq.empty.GetEnumerator() :> IEnumerator
            else
                new SuggestionBufferEnumerator(tail, data) :> IEnumerator
