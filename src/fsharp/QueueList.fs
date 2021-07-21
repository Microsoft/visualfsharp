// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace Internal.Utilities.Collections 

open System.Collections
open System.Collections.Generic

/// Iterable functional collection with O(1) append-1 time. Useful for data structures where elements get added at the
/// end but the collection must occasionally be iterated. Iteration is slower and may allocate because 
/// a suffix of elements is stored in reverse order.
///
/// The type doesn't support structural hashing or comparison.
type internal QueueList<'T>(firstElementsIn:'T list, lastElementsRevIn: 'T list, numLastElementsIn: int) = 
    let numFirstElements = List.length firstElementsIn
    // Push the lastElementsRev onto the firstElements every so often.
    let push = numLastElementsIn > numFirstElements / 5
    
    // Compute the contents after pushing.
    let firstElements = if push then List.append firstElementsIn (List.rev lastElementsRevIn) else firstElementsIn
    let lastElementsRev = if push then [] else lastElementsRevIn
    let numLastElements = if push then 0 else numLastElementsIn

    // Compute the last elements on demand.
    let lastElements() = if push then [] else List.rev lastElementsRev

    static let empty = QueueList<'T>([], [], 0)

    static member Empty : QueueList<'T> = empty

    new (xs:'T list) = QueueList(xs,[],0)
    
    member x.ToList() = if push then firstElements else List.append firstElements (lastElements())

    member x.FirstElements = firstElements

    member x.LastElements = lastElements()

    /// This operation is O(1), unless a push happens, which is rare.
    member x.AppendOne(y) = QueueList(firstElements, y :: lastElementsRev, numLastElements+1)

    member x.Append(ys:seq<_>) =
        let newElements = Seq.toList ys
        let newLength = List.length newElements
        let lastElementsRevIn = List.rev newElements @ lastElementsRev
        QueueList(firstElements, lastElementsRevIn, numLastElementsIn + newLength)
    
    /// This operation is O(n) anyway, so executing ToList() here is OK
    interface IEnumerable<'T> with 
        member x.GetEnumerator() : IEnumerator<'T> = (x.ToList() :> IEnumerable<_>).GetEnumerator()

    interface IEnumerable with 
        member x.GetEnumerator() : IEnumerator = ((x :> IEnumerable<'T>).GetEnumerator() :> IEnumerator)

module internal QueueList =
    let empty<'T> : QueueList<'T> = QueueList<'T>.Empty

    let ofSeq (x:seq<_>) = QueueList(List.ofSeq x)

    let rec iter f (x:QueueList<_>) = Seq.iter f x 

    let rec map f (x:QueueList<_>) = ofSeq (Seq.map f x)

    let rec exists f (x:QueueList<_>) = Seq.exists f x

    let rec filter f (x:QueueList<_>) = ofSeq (Seq.filter f x)

    let rec foldBack f (x:QueueList<_>) acc = List.foldBack f  x.FirstElements (List.foldBack f x.LastElements acc)

    let forall f (x:QueueList<_>) = Seq.forall f x

    let ofList (x:list<_>) = QueueList(x)

    let toList (x:QueueList<_>) = Seq.toList x

    let tryFind f (x:QueueList<_>) = Seq.tryFind f x

    let one x = QueueList [x]

    let appendOne (x:QueueList<_>) y = x.AppendOne(y)

    let append (x:QueueList<_>) (ys:QueueList<_>) = x.Append(ys)
