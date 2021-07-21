// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace Internal.Utilities.Collections

open System.Collections.Generic
                                 
// Each entry in the HashMultiMap dictionary has at least one entry. Under normal usage each entry has _only_
// one entry. So use two hash tables: one for the main entries and one for the overflow.
[<Sealed>]
type internal HashMultiMap<'Key,'Value>(size: int, comparer: IEqualityComparer<'Key>) = 

    let firstEntries = Dictionary<_,_>(size,comparer)

    let rest = Dictionary<_,_>(3,comparer)
 
    new (comparer : IEqualityComparer<'Key>) = HashMultiMap<'Key,'Value>(11, comparer)

    new (entries : seq<'Key * 'Value>, comparer : IEqualityComparer<'Key>) as x = 
        new HashMultiMap<'Key,'Value>(11, comparer)
        then entries |> Seq.iter (fun (k,v) -> x.Add(k,v))

    member x.GetRest(k) =
        match rest.TryGetValue k with
        | true, res -> res
        | _ -> []

    member x.Add(y,z) = 
        match firstEntries.TryGetValue y with
        | true, res ->
            rest.[y] <- res :: x.GetRest(y)
        | _ -> ()
        firstEntries.[y] <- z

    member x.Clear() = 
         firstEntries.Clear()
         rest.Clear()

    member x.FirstEntries = firstEntries

    member x.Rest = rest

    member x.Copy() = 
        let res = HashMultiMap<'Key,'Value>(firstEntries.Count,firstEntries.Comparer)
        for kvp in firstEntries do 
             res.FirstEntries.Add(kvp.Key,kvp.Value)

        for kvp in rest do 
             res.Rest.Add(kvp.Key,kvp.Value)
        res

    member x.Item 
        with get(y : 'Key) = 
            match firstEntries.TryGetValue y with
            | true, res -> res
            | _ -> raise (KeyNotFoundException("The item was not found in collection"))
        and set (y:'Key) (z:'Value) = 
            x.Replace(y,z)

    member x.FindAll(y) = 
        match firstEntries.TryGetValue y with
        | true, res -> res :: x.GetRest(y)
        | _ -> []

    member x.Fold f acc = 
        let mutable res = acc
        for kvp in firstEntries do
            res <- f kvp.Key kvp.Value res
            match x.GetRest(kvp.Key) with
            | [] -> ()
            | rest -> 
                for z in rest do
                    res <- f kvp.Key z res
        res

    member x.Iterate(f) =  
        for kvp in firstEntries do
            f kvp.Key kvp.Value
            match x.GetRest(kvp.Key) with
            | [] -> ()
            | rest -> 
                for z in rest do
                    f kvp.Key z

    member x.Contains(y) = firstEntries.ContainsKey(y)

    member x.ContainsKey(y) = firstEntries.ContainsKey(y)

    member x.Remove(y) = 
        match firstEntries.TryGetValue y with
        // NOTE: If not ok then nothing to remove - nop
        | true, _res ->
            // We drop the FirstEntry. Here we compute the new FirstEntry and residue MoreEntries
            match rest.TryGetValue y with
            | true, res ->
                match res with 
                | [h] -> 
                    firstEntries.[y] <- h; 
                    rest.Remove(y) |> ignore
                | h :: t -> 
                    firstEntries.[y] <- h
                    rest.[y] <- t
                | _ -> 
                    ()
            | _ ->
                firstEntries.Remove(y) |> ignore 
        | _ -> ()

    member x.Replace(y,z) = 
        firstEntries.[y] <- z

    member x.TryFind(y) =
        match firstEntries.TryGetValue y with
        | true, res -> Some res
        | _ -> None

    member x.Count = firstEntries.Count

    interface IEnumerable<KeyValuePair<'Key, 'Value>> with

        member s.GetEnumerator() = 
            let elems = List<_>(firstEntries.Count + rest.Count)
            for kvp in firstEntries do
                elems.Add(kvp)
                for z in s.GetRest(kvp.Key) do
                   elems.Add(KeyValuePair(kvp.Key, z))
            (elems.GetEnumerator() :> IEnumerator<_>)

    interface System.Collections.IEnumerable with

        member s.GetEnumerator() = ((s :> seq<_>).GetEnumerator() :> System.Collections.IEnumerator)

    interface IDictionary<'Key, 'Value> with 

        member s.Item 
            with get x = s.[x]            
            and  set x v = s.[x] <- v
            
        member s.Keys = ([| for kvp in s -> kvp.Key |] :> ICollection<'Key>)

        member s.Values = ([| for kvp in s -> kvp.Value |] :> ICollection<'Value>)

        member s.Add(k,v) = s.[k] <- v

        member s.ContainsKey(k) = s.ContainsKey(k)

        member s.TryGetValue(k,r) = match s.TryFind k with Some v-> (r <- v; true) | _ -> false

        member s.Remove(k:'Key) = 
            let res = s.ContainsKey(k) in 
            s.Remove(k); res

    interface ICollection<KeyValuePair<'Key, 'Value>> with 

        member s.Add(x) = s.[x.Key] <- x.Value

        member s.Clear() = s.Clear()            

        member s.Remove(x) = 
            match s.TryFind x.Key with
            | Some v -> 
                if Unchecked.equals v x.Value then
                    s.Remove(x.Key)
                true
            | _ -> false

        member s.Contains(x) =
            match s.TryFind x.Key with
            | Some v when Unchecked.equals v x.Value -> true
            | _ -> false

        member s.CopyTo(arr,arrIndex) = s |> Seq.iteri (fun j x -> arr.[arrIndex+j] <- x)

        member s.IsReadOnly = false

        member s.Count = s.Count

