﻿// Copyright (c) Microsoft Open Technologies, Inc.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

module FSharp.Core.PropertyTests.CollectionModulesConsistency

open System
open System.Collections.Generic

open NUnit.Framework
open FsCheck
open Utils

let append<'a when 'a : equality> (xs : list<'a>) (xs2 : list<'a>) =
    let s = xs |> Seq.append xs2 
    let l = xs |> List.append xs2
    let a = xs |> Seq.toArray |> Array.append (Seq.toArray xs2)
    Seq.toArray s = a && List.toArray l = a

[<Test>]
let ``append is consistent`` () =
    Check.QuickThrowOnFailure append<int>
    Check.QuickThrowOnFailure append<string>
    Check.QuickThrowOnFailure append<NormalFloat>

let averageFloat (xs : NormalFloat []) =
    let xs = xs |> Array.map float
    let s = run (fun () -> xs |> Seq.average)
    let l = run (fun () -> xs |> List.ofArray |> List.average)
    let a = run (fun () -> xs |> Array.average)
    s = a && l = a

[<Test>]
let ``average is consistent`` () =
    Check.QuickThrowOnFailure averageFloat

let averageBy (xs : float []) f =
    let xs = xs |> Array.map float
    let f x = (f x : NormalFloat) |> float
    let s = run (fun () -> xs |> Seq.averageBy f)
    let l = run (fun () -> xs |> List.ofArray |> List.averageBy f)
    let a = run (fun () -> xs |> Array.averageBy f)
    s = a && l = a

[<Test>]
let ``averageBy is consistent`` () =
    Check.QuickThrowOnFailure averageBy

let contains<'a when 'a : equality> (xs : 'a []) x  =
    let s = xs |> Seq.contains x
    let l = xs |> List.ofArray |> List.contains x
    let a = xs |> Array.contains x
    s = a && l = a

[<Test>]
let ``contains is consistent`` () =
    Check.QuickThrowOnFailure contains<int>
    Check.QuickThrowOnFailure contains<string>
    Check.QuickThrowOnFailure contains<float>

let choose<'a when 'a : equality> (xs : 'a []) f  =
    let s = xs |> Seq.choose f
    let l = xs |> List.ofArray |> List.choose f
    let a = xs |> Array.choose f
    Seq.toArray s = a && List.toArray l = a

[<Test>]
let ``choose is consistent`` () =
    Check.QuickThrowOnFailure contains<int>
    Check.QuickThrowOnFailure contains<string>
    Check.QuickThrowOnFailure contains<float>

let chunkBySize<'a when 'a : equality> (xs : 'a []) size =
    let s = run (fun () -> xs |> Seq.chunkBySize size |> Seq.map Seq.toArray |> Seq.toArray)
    let l = run (fun () -> xs |> List.ofArray |> List.chunkBySize size |> Seq.map Seq.toArray |> Seq.toArray)
    let a = run (fun () -> xs |> Array.chunkBySize size |> Seq.map Seq.toArray |> Seq.toArray)
    s = a && l = a

[<Test>]
let ``chunkBySize is consistent`` () =
    Check.QuickThrowOnFailure chunkBySize<int>
    Check.QuickThrowOnFailure chunkBySize<string>
    Check.QuickThrowOnFailure chunkBySize<NormalFloat>

let collect<'a> (xs : 'a []) f  =
    let s = xs |> Seq.collect f
    let l = xs |> List.ofArray |> List.collect (fun x -> f x |> List.ofArray)
    let a = xs |> Array.collect f
    Seq.toArray s = a && List.toArray l = a

[<Test>]
let ``collect is consistent`` () =
    Check.QuickThrowOnFailure collect<int>
    Check.QuickThrowOnFailure collect<string>
    Check.QuickThrowOnFailure collect<float>

let compareWith<'a>(xs : 'a []) (xs2 : 'a []) f  =
    let s = (xs, xs2) ||> Seq.compareWith f
    let l = (List.ofArray xs, List.ofArray xs2) ||> List.compareWith f
    let a = (xs, xs2) ||> Array.compareWith f
    s = a && l = a

[<Test>]
let ``compareWith is consistent`` () =
    Check.QuickThrowOnFailure compareWith<int>
    Check.QuickThrowOnFailure compareWith<string>
    Check.QuickThrowOnFailure compareWith<float>
        
let concat<'a when 'a : equality> (xs : 'a [][]) =
    let s = xs |> Seq.concat
    let l = xs |> List.ofArray |> List.map List.ofArray |> List.concat
    let a = xs |> Array.concat
    Seq.toArray s = a && List.toArray l = a

[<Test>]
let ``concat is consistent`` () =
    Check.QuickThrowOnFailure concat<int>
    Check.QuickThrowOnFailure concat<string>
    Check.QuickThrowOnFailure concat<NormalFloat>

let countBy<'a> (xs : 'a []) f =
    let s = xs |> Seq.countBy f
    let l = xs |> List.ofArray |> List.countBy f
    let a = xs |> Array.countBy f
    Seq.toArray s = a && List.toArray l = a

[<Test>]
let ``countBy is consistent`` () =
    Check.QuickThrowOnFailure countBy<int>
    Check.QuickThrowOnFailure countBy<string>
    Check.QuickThrowOnFailure countBy<float>

let distinct<'a when 'a : comparison> (xs : 'a []) =
    let s = xs |> Seq.distinct 
    let l = xs |> List.ofArray |> List.distinct
    let a = xs |> Array.distinct
    Seq.toArray s = a && List.toArray l = a

[<Test>]
let ``distinct is consistent`` () =
    Check.QuickThrowOnFailure distinct<int>
    Check.QuickThrowOnFailure distinct<string>
    Check.QuickThrowOnFailure distinct<NormalFloat>

let distinctBy<'a when 'a : equality> (xs : 'a []) f =
    let s = xs |> Seq.distinctBy f
    let l = xs |> List.ofArray |> List.distinctBy f
    let a = xs |> Array.distinctBy f
    Seq.toArray s = a && List.toArray l = a

[<Test>]
let ``distinctBy is consistent`` () =
    Check.QuickThrowOnFailure distinctBy<int>
    Check.QuickThrowOnFailure distinctBy<string>
    Check.QuickThrowOnFailure distinctBy<NormalFloat>

let exactlyOne<'a when 'a : comparison> (xs : 'a []) =
    let s = runAndCheckErrorType (fun () -> xs |> Seq.exactlyOne)
    let l = runAndCheckErrorType (fun () -> xs |> List.ofArray |> List.exactlyOne)
    let a = runAndCheckErrorType (fun () -> xs |> Array.exactlyOne)
    s = a && l = a

[<Test>]
let ``exactlyOne is consistent`` () =
    Check.QuickThrowOnFailure exactlyOne<int>
    Check.QuickThrowOnFailure exactlyOne<string>
    Check.QuickThrowOnFailure exactlyOne<NormalFloat>

let except<'a when 'a : equality> (xs : 'a []) (itemsToExclude: 'a []) =
    let s = xs |> Seq.except itemsToExclude
    let l = xs |> List.ofArray |> List.except itemsToExclude
    let a = xs |> Array.except itemsToExclude
    Seq.toArray s = a && List.toArray l = a

[<Test>]
let ``except is consistent`` () =
    Check.QuickThrowOnFailure except<int>
    Check.QuickThrowOnFailure except<string>
    Check.QuickThrowOnFailure except<NormalFloat>

let exists<'a when 'a : equality> (xs : 'a []) f =
    let s = xs |> Seq.exists f
    let l = xs |> List.ofArray |> List.exists f
    let a = xs |> Array.exists f
    s = a && l = a

[<Test>]
let ``exists is consistent`` () =
    Check.QuickThrowOnFailure exists<int>
    Check.QuickThrowOnFailure exists<string>
    Check.QuickThrowOnFailure exists<NormalFloat>

let exists2<'a when 'a : equality> (xs':('a*'a) []) f =    
    let xs = Array.map fst xs'
    let xs2 = Array.map snd xs'
    let s = runAndCheckErrorType (fun () -> Seq.exists2 f xs xs2)
    let l = runAndCheckErrorType (fun () -> List.exists2 f (List.ofSeq xs) (List.ofSeq xs2))
    let a = runAndCheckErrorType (fun () -> Array.exists2 f (Array.ofSeq xs) (Array.ofSeq xs2))
    s = a && l = a
    
[<Test>]
let ``exists2 is consistent for collections with equal length`` () =
    Check.QuickThrowOnFailure exists2<int>
    Check.QuickThrowOnFailure exists2<string>
    Check.QuickThrowOnFailure exists2<NormalFloat>

let filter<'a when 'a : equality> (xs : 'a []) predicate =
    let s = xs |> Seq.filter predicate
    let l = xs |> List.ofArray |> List.filter predicate
    let a = xs |> Array.filter predicate
    Seq.toArray s = a && List.toArray l = a

[<Test>]
let ``filter is consistent`` () =
    Check.QuickThrowOnFailure filter<int>
    Check.QuickThrowOnFailure filter<string>
    Check.QuickThrowOnFailure filter<NormalFloat>

let find<'a when 'a : equality> (xs : 'a []) predicate =
    let s = run (fun () -> xs |> Seq.find predicate)
    let l = run (fun () -> xs |> List.ofArray |> List.find predicate)
    let a = run (fun () -> xs |> Array.find predicate)
    s = a && l = a

[<Test>]
let ``find is consistent`` () =
    Check.QuickThrowOnFailure find<int>
    Check.QuickThrowOnFailure find<string>
    Check.QuickThrowOnFailure find<NormalFloat>

let findBack<'a when 'a : equality> (xs : 'a []) predicate =
    let s = run (fun () -> xs |> Seq.findBack predicate)
    let l = run (fun () -> xs |> List.ofArray |> List.findBack predicate)
    let a = run (fun () -> xs |> Array.findBack predicate)
    s = a && l = a

[<Test>]
let ``findBack is consistent`` () =
    Check.QuickThrowOnFailure findBack<int>
    Check.QuickThrowOnFailure findBack<string>
    Check.QuickThrowOnFailure findBack<NormalFloat>

let findIndex<'a when 'a : equality> (xs : 'a []) predicate =
    let s = run (fun () -> xs |> Seq.findIndex predicate)
    let l = run (fun () -> xs |> List.ofArray |> List.findIndex predicate)
    let a = run (fun () -> xs |> Array.findIndex predicate)
    s = a && l = a

[<Test>]
let ``findIndex is consistent`` () =
    Check.QuickThrowOnFailure findIndex<int>
    Check.QuickThrowOnFailure findIndex<string>
    Check.QuickThrowOnFailure findIndex<NormalFloat>

let findIndexBack<'a when 'a : equality> (xs : 'a []) predicate =
    let s = run (fun () -> xs |> Seq.findIndexBack predicate)
    let l = run (fun () -> xs |> List.ofArray |> List.findIndexBack predicate)
    let a = run (fun () -> xs |> Array.findIndexBack predicate)
    s = a && l = a

[<Test>]
let ``findIndexBack is consistent`` () =
    Check.QuickThrowOnFailure findIndexBack<int>
    Check.QuickThrowOnFailure findIndexBack<string>
    Check.QuickThrowOnFailure findIndexBack<NormalFloat>

let fold<'a,'b when 'b : equality> (xs : 'a []) f (start:'b) =
    let s = run (fun () -> xs |> Seq.fold f start)
    let l = run (fun () -> xs |> List.ofArray |> List.fold f start)
    let a = run (fun () -> xs |> Array.fold f start)
    s = a && l = a

[<Test>]
let ``fold is consistent`` () =
    Check.QuickThrowOnFailure fold<int,int>
    Check.QuickThrowOnFailure fold<string,string>
    Check.QuickThrowOnFailure fold<float,int>
    Check.QuickThrowOnFailure fold<float,string>

let fold2<'a,'b,'c when 'c : equality> (xs': ('a*'b)[]) f (start:'c) =
    let xs = xs' |> Array.map fst
    let xs2 = xs' |> Array.map snd
    let s = run (fun () -> Seq.fold2 f start xs xs2)
    let l = run (fun () -> List.fold2 f start (List.ofArray xs) (List.ofArray xs2))
    let a = run (fun () -> Array.fold2 f start xs xs2)
    s = a && l = a

[<Test>]
let ``fold2 is consistent`` () =
    Check.QuickThrowOnFailure fold2<int,int,int>
    Check.QuickThrowOnFailure fold2<string,string,string>
    Check.QuickThrowOnFailure fold2<string,int,string>
    Check.QuickThrowOnFailure fold2<string,float,int>
    Check.QuickThrowOnFailure fold2<float,float,int>
    Check.QuickThrowOnFailure fold2<float,float,string>

let foldBack<'a,'b when 'b : equality> (xs : 'a []) f (start:'b) =
    let s = run (fun () -> Seq.foldBack f xs start)
    let l = run (fun () -> List.foldBack f (xs |> List.ofArray) start)
    let a = run (fun () -> Array.foldBack f xs start)
    s = a && l = a

[<Test>]
let ``foldBack is consistent`` () =
    Check.QuickThrowOnFailure foldBack<int,int>
    Check.QuickThrowOnFailure foldBack<string,string>
    Check.QuickThrowOnFailure foldBack<float,int>
    Check.QuickThrowOnFailure foldBack<float,string>

let foldBack2<'a,'b,'c when 'c : equality> (xs': ('a*'b)[]) f (start:'c) =
    let xs = xs' |> Array.map fst
    let xs2 = xs' |> Array.map snd
    let s = run (fun () -> Seq.foldBack2 f xs xs2 start)
    let l = run (fun () -> List.foldBack2 f (List.ofArray xs) (List.ofArray xs2) start)
    let a = run (fun () -> Array.foldBack2 f xs xs2 start)
    s = a && l = a

[<Test>]
let ``foldBack2 is consistent`` () =
    Check.QuickThrowOnFailure foldBack2<int,int,int>
    Check.QuickThrowOnFailure foldBack2<string,string,string>
    Check.QuickThrowOnFailure foldBack2<string,int,string>
    Check.QuickThrowOnFailure foldBack2<string,float,int>
    Check.QuickThrowOnFailure foldBack2<float,float,int>
    Check.QuickThrowOnFailure foldBack2<float,float,string>

let forall<'a when 'a : equality> (xs : 'a []) f =
    let s = xs |> Seq.forall f
    let l = xs |> List.ofArray |> List.forall f
    let a = xs |> Array.forall f
    s = a && l = a

[<Test>]
let ``forall is consistent`` () =
    Check.QuickThrowOnFailure forall<int>
    Check.QuickThrowOnFailure forall<string>
    Check.QuickThrowOnFailure forall<NormalFloat>

let forall2<'a when 'a : equality> (xs':('a*'a) []) f =    
    let xs = Array.map fst xs'
    let xs2 = Array.map snd xs'
    let s = runAndCheckErrorType (fun () -> Seq.forall2 f xs xs2)
    let l = runAndCheckErrorType (fun () -> List.forall2 f (List.ofSeq xs) (List.ofSeq xs2))
    let a = runAndCheckErrorType (fun () -> Array.forall2 f (Array.ofSeq xs) (Array.ofSeq xs2))
    s = a && l = a
    
[<Test>]
let ``forall2 is consistent for collections with equal length`` () =
    Check.QuickThrowOnFailure forall2<int>
    Check.QuickThrowOnFailure forall2<string>
    Check.QuickThrowOnFailure forall2<NormalFloat>

let groupBy<'a when 'a : equality> (xs : 'a []) f =
    let s = run (fun () -> xs |> Seq.groupBy f |> Seq.toArray |> Array.map (fun (x,xs) -> x,xs |> Seq.toArray))
    let l = run (fun () -> xs |> List.ofArray |> List.groupBy f |> Seq.toArray |> Array.map (fun (x,xs) -> x,xs |> Seq.toArray))
    let a = run (fun () -> xs |> Array.groupBy f |> Array.map (fun (x,xs) -> x,xs |> Seq.toArray))
    s = a && l = a

[<Test>]
let ``groupBy is consistent`` () =
    Check.QuickThrowOnFailure groupBy<int>
    Check.QuickThrowOnFailure groupBy<string>
    Check.QuickThrowOnFailure groupBy<NormalFloat>

let head<'a when 'a : equality> (xs : 'a []) =
    let s = runAndCheckIfAnyError (fun () -> xs |> Seq.head)
    let l = runAndCheckIfAnyError (fun () -> xs |> List.ofArray |> List.head)
    let a = runAndCheckIfAnyError (fun () -> xs |> Array.head)
    s = a && l = a

[<Test>]
let ``head is consistent`` () =
    Check.QuickThrowOnFailure head<int>
    Check.QuickThrowOnFailure head<string>
    Check.QuickThrowOnFailure head<NormalFloat>

let indexed<'a when 'a : equality> (xs : 'a []) =
    let s = xs |> Seq.indexed
    let l = xs |> List.ofArray |> List.indexed
    let a = xs |> Array.indexed
    Seq.toArray s = a && List.toArray l = a

[<Test>]
let ``indexed is consistent`` () =
    Check.QuickThrowOnFailure indexed<int>
    Check.QuickThrowOnFailure indexed<string>
    Check.QuickThrowOnFailure indexed<NormalFloat>

let init<'a when 'a : equality> count f =
    let s = run (fun () -> Seq.init count f |> Seq.toArray)
    let l = run (fun () -> List.init count f |> Seq.toArray)
    let a = run (fun () -> Array.init count f)
    s = a && l = a

[<Test>]
let ``init is consistent`` () =
    Check.QuickThrowOnFailure init<int>
    Check.QuickThrowOnFailure init<string>
    Check.QuickThrowOnFailure init<NormalFloat>

let isEmpty<'a when 'a : equality> (xs : 'a []) =
    let s = xs |> Seq.isEmpty
    let l = xs |> List.ofArray |> List.isEmpty
    let a = xs |> Array.isEmpty
    s = a && l = a

[<Test>]
let ``isEmpty is consistent`` () =
    Check.QuickThrowOnFailure isEmpty<int>
    Check.QuickThrowOnFailure isEmpty<string>
    Check.QuickThrowOnFailure isEmpty<NormalFloat>

let item<'a when 'a : equality> (xs : 'a []) index =
    let s = runAndCheckIfAnyError (fun () -> xs |> Seq.item index)
    let l = runAndCheckIfAnyError (fun () -> xs |> List.ofArray |> List.item index)
    let a = runAndCheckIfAnyError (fun () -> xs |> Array.item index)
    s = a && l = a

[<Test>]
let ``item is consistent`` () =
    Check.QuickThrowOnFailure item<int>
    Check.QuickThrowOnFailure item<string>
    Check.QuickThrowOnFailure item<NormalFloat>

let iter<'a when 'a : equality> (xs : 'a []) f' =
    let list = System.Collections.Generic.List<'a>()
    let f x =
        list.Add x
        f' x

    let s = xs |> Seq.iter f
    let l = xs |> List.ofArray |> List.iter f
    let a =  xs |> Array.iter f

    let xs = Seq.toList xs
    list |> Seq.toList = (xs @ xs @ xs)

[<Test>]
let ``iter looks at every element exactly once and in order - consistenly over all collections`` () =
    Check.QuickThrowOnFailure iter<int>
    Check.QuickThrowOnFailure iter<string>
    Check.QuickThrowOnFailure iter<NormalFloat>

let iter2<'a when 'a : equality> (xs' : ('a*'a) []) f' =
    let xs = xs' |> Array.map fst
    let xs2 = xs' |> Array.map snd
    let list = System.Collections.Generic.List<'a*'a>()
    let f x y =
        list.Add <| (x,y)
        f' x y

    let s = Seq.iter2 f xs xs2
    let l = List.iter2 f (xs |> List.ofArray) (xs2 |> List.ofArray)
    let a = Array.iter2 f xs xs2

    let xs = Seq.toList xs'
    list |> Seq.toList = (xs @ xs @ xs)

[<Test>]
let ``iter2 looks at every element exactly once and in order - consistenly over all collections when size is equal`` () =
    Check.QuickThrowOnFailure iter2<int>
    Check.QuickThrowOnFailure iter2<string>
    Check.QuickThrowOnFailure iter2<NormalFloat>

let iteri<'a when 'a : equality> (xs : 'a []) f' =
    let list = System.Collections.Generic.List<'a>()
    let indices = System.Collections.Generic.List<int>()
    let f i x =
        list.Add x
        indices.Add i
        f' i x

    let s = xs |> Seq.iteri f
    let l = xs |> List.ofArray |> List.iteri f
    let a =  xs |> Array.iteri f

    let xs = Seq.toList xs
    list |> Seq.toList = (xs @ xs @ xs) &&
      indices |> Seq.toList = ([0..xs.Length-1] @ [0..xs.Length-1] @ [0..xs.Length-1])

[<Test>]
let ``iteri looks at every element exactly once and in order - consistenly over all collections`` () =
    Check.QuickThrowOnFailure iteri<int>
    Check.QuickThrowOnFailure iteri<string>
    Check.QuickThrowOnFailure iteri<NormalFloat>

let iteri2<'a when 'a : equality> (xs' : ('a*'a) []) f' =
    let xs = xs' |> Array.map fst
    let xs2 = xs' |> Array.map snd
    let list = System.Collections.Generic.List<'a*'a>()
    let indices = System.Collections.Generic.List<int>()
    let f i x y =
        list.Add <| (x,y)
        indices.Add i
        f' x y

    let s = Seq.iteri2 f xs xs2
    let l = List.iteri2 f (xs |> List.ofArray) (xs2 |> List.ofArray)
    let a = Array.iteri2 f xs xs2

    let xs = Seq.toList xs'
    list |> Seq.toList = (xs @ xs @ xs) &&
      indices |> Seq.toList = ([0..xs.Length-1] @ [0..xs.Length-1] @ [0..xs.Length-1])

[<Test>]
let ``iteri2 looks at every element exactly once and in order - consistenly over all collections when size is equal`` () =
    Check.QuickThrowOnFailure iteri2<int>
    Check.QuickThrowOnFailure iteri2<string>
    Check.QuickThrowOnFailure iteri2<NormalFloat>

let last<'a when 'a : equality> (xs : 'a []) =
    let s = runAndCheckIfAnyError (fun () -> xs |> Seq.last)
    let l = runAndCheckIfAnyError (fun () -> xs |> List.ofArray |> List.last)
    let a = runAndCheckIfAnyError (fun () -> xs |> Array.last)
    s = a && l = a

[<Test>]
let ``last is consistent`` () =
    Check.QuickThrowOnFailure last<int>
    Check.QuickThrowOnFailure last<string>
    Check.QuickThrowOnFailure last<NormalFloat>

let length<'a when 'a : equality> (xs : 'a []) =
    let s = xs |> Seq.length
    let l = xs |> List.ofArray |> List.length
    let a = xs |> Array.length
    s = a && l = a

[<Test>]
let ``length is consistent`` () =
    Check.QuickThrowOnFailure length<int>
    Check.QuickThrowOnFailure length<string>
    Check.QuickThrowOnFailure length<float>

let map<'a when 'a : equality> (xs : 'a []) f =
    let s = xs |> Seq.map f
    let l = xs |> List.ofArray |> List.map f
    let a = xs |> Array.map f
    Seq.toArray s = a && List.toArray l = a

[<Test>]
let ``map is consistent`` () =
    Check.QuickThrowOnFailure map<int>
    Check.QuickThrowOnFailure map<string>
    Check.QuickThrowOnFailure map<float>

let map2<'a when 'a : equality> (xs' : ('a*'a) []) f' =
    let xs = xs' |> Array.map fst
    let xs2 = xs' |> Array.map snd
    let list = System.Collections.Generic.List<'a*'a>()
    let f x y =
        list.Add <| (x,y)
        f' x y

    let s = Seq.map2 f xs xs2
    let l = List.map2 f (xs |> List.ofArray) (xs2 |> List.ofArray)
    let a = Array.map2 f xs xs2

    let xs = Seq.toList xs'    
    Seq.toArray s = a && List.toArray l = a &&
      list |> Seq.toList = (xs @ xs @ xs)

[<Test>]
let ``map2 looks at every element exactly once and in order - consistenly over all collections when size is equal`` () =
    Check.QuickThrowOnFailure map2<int>
    Check.QuickThrowOnFailure map2<string>
    Check.QuickThrowOnFailure map2<NormalFloat>

let map3<'a when 'a : equality> (xs' : ('a*'a*'a) []) f' =
    let xs = xs' |> Array.map  (fun (x,y,z) -> x)
    let xs2 = xs' |> Array.map (fun (x,y,z) -> y)
    let xs3 = xs' |> Array.map (fun (x,y,z) -> z)
    let list = System.Collections.Generic.List<'a*'a*'a>()
    let f x y z =
        list.Add <| (x,y,z)
        f' x y z

    let s = Seq.map3 f xs xs2 xs3
    let l = List.map3 f (xs |> List.ofArray) (xs2 |> List.ofArray) (xs3 |> List.ofArray)
    let a = Array.map3 f xs xs2 xs3

    let xs = Seq.toList xs'
    Seq.toArray s = a && List.toArray l = a &&
      list |> Seq.toList = (xs @ xs @ xs)

[<Test>]
let ``map3 looks at every element exactly once and in order - consistenly over all collections when size is equal`` () =
    Check.QuickThrowOnFailure map3<int>
    Check.QuickThrowOnFailure map3<string>
    Check.QuickThrowOnFailure map3<NormalFloat>

let mapFold<'a when 'a : equality> (xs : 'a []) f start =
    let s,sr = xs |> Seq.mapFold f start
    let l,lr = xs |> List.ofArray |> List.mapFold f start
    let a,ar = xs |> Array.mapFold f start
    Seq.toArray s = a && List.toArray l = a &&
      sr = lr && sr = ar

[<Test>]
let ``mapFold is consistent`` () =
    Check.QuickThrowOnFailure mapFold<int>
    Check.QuickThrowOnFailure mapFold<string>
    Check.QuickThrowOnFailure mapFold<NormalFloat>

let mapFoldBack<'a when 'a : equality> (xs : 'a []) f start =
    let s,sr = Seq.mapFoldBack f xs start
    let l,lr = List.mapFoldBack f (xs |> List.ofArray) start
    let a,ar = Array.mapFoldBack f xs start
    Seq.toArray s = a && List.toArray l = a &&
      sr = lr && sr = ar

[<Test>]
let ``mapFold2 is consistent`` () =
    Check.QuickThrowOnFailure mapFoldBack<int>
    Check.QuickThrowOnFailure mapFoldBack<string>
    Check.QuickThrowOnFailure mapFoldBack<NormalFloat>

let mapi<'a when 'a : equality> (xs : 'a []) f =
    let s = xs |> Seq.mapi f
    let l = xs |> List.ofArray |> List.mapi f
    let a = xs |> Array.mapi f
    Seq.toArray s = a && List.toArray l = a

[<Test>]
let ``mapi is consistent`` () =
    Check.QuickThrowOnFailure mapi<int>
    Check.QuickThrowOnFailure mapi<string>
    Check.QuickThrowOnFailure mapi<float>

let mapi2<'a when 'a : equality> (xs' : ('a*'a) []) f' =
    let xs = xs' |> Array.map fst
    let xs2 = xs' |> Array.map snd
    let list = System.Collections.Generic.List<'a*'a>()
    let indices = System.Collections.Generic.List<int>()
    let f i x y =
        indices.Add i
        list.Add <| (x,y)
        f' x y

    let s = Seq.mapi2 f xs xs2
    let l = List.mapi2 f (xs |> List.ofArray) (xs2 |> List.ofArray)
    let a = Array.mapi2 f xs xs2

    let xs = Seq.toList xs'    
    Seq.toArray s = a && List.toArray l = a &&
      list |> Seq.toList = (xs @ xs @ xs) &&
      (Seq.toList indices = [0..xs.Length-1] @ [0..xs.Length-1] @ [0..xs.Length-1])

[<Test>]
let ``mapi2 looks at every element exactly once and in order - consistenly over all collections when size is equal`` () =
    Check.QuickThrowOnFailure mapi2<int>
    Check.QuickThrowOnFailure mapi2<string>
    Check.QuickThrowOnFailure mapi2<NormalFloat>

let max<'a when 'a : comparison> (xs : 'a []) =
    let s = runAndCheckIfAnyError (fun () -> xs |> Seq.max)
    let l = runAndCheckIfAnyError (fun () -> xs |> List.ofArray |> List.max)
    let a = runAndCheckIfAnyError (fun () -> xs |> Array.max)
    s = a && l = a

[<Test>]
let ``max is consistent`` () =
    Check.QuickThrowOnFailure max<int>
    Check.QuickThrowOnFailure max<string>
    Check.QuickThrowOnFailure max<NormalFloat>

let maxBy<'a when 'a : comparison> (xs : 'a []) f =
    let s = runAndCheckIfAnyError (fun () -> xs |> Seq.maxBy f)
    let l = runAndCheckIfAnyError (fun () -> xs |> List.ofArray |> List.maxBy f)
    let a = runAndCheckIfAnyError (fun () -> xs |> Array.maxBy f)
    s = a && l = a

[<Test>]
let ``maxBy is consistent`` () =
    Check.QuickThrowOnFailure maxBy<int>
    Check.QuickThrowOnFailure maxBy<string>
    Check.QuickThrowOnFailure maxBy<NormalFloat>
 
let min<'a when 'a : comparison> (xs : 'a []) =
    let s = runAndCheckIfAnyError (fun () -> xs |> Seq.min)
    let l = runAndCheckIfAnyError (fun () -> xs |> List.ofArray |> List.min)
    let a = runAndCheckIfAnyError (fun () -> xs |> Array.min)
    s = a && l = a

[<Test>]
let ``min is consistent`` () =
    Check.QuickThrowOnFailure min<int>
    Check.QuickThrowOnFailure min<string>
    Check.QuickThrowOnFailure min<NormalFloat>

let sort<'a when 'a : comparison> (xs : 'a []) =
    let s = xs |> Seq.sort 
    let l = xs |> List.ofArray |> List.sort
    let a = xs |> Array.sort
    Seq.toArray s = a && List.toArray l = a

[<Test>]
let ``sort is consistent`` () =
    Check.QuickThrowOnFailure sort<int>
    Check.QuickThrowOnFailure sort<string>
    Check.QuickThrowOnFailure sort<NormalFloat>

let sortDescending<'a when 'a : comparison> (xs : 'a []) =
    let s = xs |> Seq.sortDescending 
    let l = xs |> List.ofArray |> List.sortDescending
    let a = xs |> Array.sortDescending
    Seq.toArray s = a && List.toArray l = a

[<Test>]
let ``sortDescending is consistent`` () =
    Check.QuickThrowOnFailure sortDescending<int>
    Check.QuickThrowOnFailure sortDescending<string>
    Check.QuickThrowOnFailure sortDescending<NormalFloat>

let splitInto<'a when 'a : equality> (xs : 'a []) count =
    let s = run (fun () -> xs |> Seq.splitInto count |> Seq.map Seq.toArray |> Seq.toArray)
    let l = run (fun () -> xs |> List.ofArray |> List.splitInto count |> Seq.map Seq.toArray |> Seq.toArray)
    let a = run (fun () -> xs |> Array.splitInto count |> Seq.map Seq.toArray |> Seq.toArray)
    s = a && l = a

[<Test>]
let ``splitInto is consistent`` () =
    Check.QuickThrowOnFailure splitInto<int>
    Check.QuickThrowOnFailure splitInto<string>
    Check.QuickThrowOnFailure splitInto<NormalFloat>