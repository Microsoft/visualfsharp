// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

// Various tests for the:
// Microsoft.FSharp.Core.Operators module

namespace FSharp.Core.UnitTests.Operators

open System
open FSharp.Core.UnitTests.LibraryTestFx
open NUnit.Framework
open Microsoft.FSharp.Core.Operators.Checked

[<TestFixture>]
type OperatorsModule2() =

#if IGNORED
    [<Test; Ignore( "[FSharp Bugs 1.0] #3842 - OverflowException does not pop up on Operators.int int16 int 32 int64 ")>]
    member this.int() =         
        // int 
        let result = Operators.int 10
        Assert.AreEqual(10, result)
        
        // string
        let result = Operators.int "10"
        Assert.AreEqual(10, result)
        
        // double
        let result = Operators.int 10.0
        Assert.AreEqual(10, result) 
        
        // negative
        let result = Operators.int -10
        Assert.AreEqual(-10, result) 
        
        // zero
        let result = Operators.int 0
        Assert.AreEqual(0, result) 
        
        // overflow
        CheckThrowsOverflowException(fun() -> Operators.int System.Double.MaxValue |>ignore)
        
        ()
#endif

#if IGNORED
    [<Test; Ignore( "[FSharp Bugs 1.0] #3842 - OverflowException does not pop up on Operators.int int16 int 32 int64 ")>]
    member this.int16() =         
        // int 
        let result = Operators.int16 10
        Assert.AreEqual(10, result)
        
        // double
        let result = Operators.int16 10.0
        Assert.AreEqual(10, result) 
        
        // negative
        let result = Operators.int16 -10
        Assert.AreEqual(-10, result) 
        
        // zero
        let result = Operators.int16 0
        Assert.AreEqual(0, result) 
        
        // string
        let result = Operators.int16 "10"
        Assert.AreEqual(10, result)
        
        // overflow
        CheckThrowsOverflowException(fun() -> Operators.int16 System.Double.MaxValue |>ignore)
        ()
#endif

#if IGNORED
    [<Test; Ignore( "[FSharp Bugs 1.0] #3842 - OverflowException does not pop up on Operators.int int16 int 32 int64 ")>]
    member this.int32() =         
        // int 
        let result = Operators.int32 10
        Assert.AreEqual(10, result)
        
        // double
        let result = Operators.int32 10.0
        Assert.AreEqual(10, result) 
        
        // negative
        let result = Operators.int32 -10
        Assert.AreEqual(-10, result) 
        
        // zero
        let result = Operators.int32 0
        Assert.AreEqual(0, result) 
        
        // string
        let result = Operators.int32 "10"
        Assert.AreEqual(10, result)
        
        // overflow
        CheckThrowsOverflowException(fun() -> Operators.int32 System.Double.MaxValue |>ignore)
        ()
#endif

#if IGNORED
    [<Test; Ignore( "[FSharp Bugs 1.0] #3842 - OverflowException does not pop up on Operators.int int16 int 32 int64 ")>]
    member this.int64() =         
        // int 
        let result = Operators.int64 10
        Assert.AreEqual(10, result)
        
        // double
        let result = Operators.int64 10.0
        Assert.AreEqual(10, result) 
        
        // negative
        let result = Operators.int64 -10
        Assert.AreEqual(-10, result) 
        
        // zero
        let result = Operators.int64 0
        Assert.AreEqual(0, result) 
        
        // string
        let result = Operators.int64 "10"
        Assert.AreEqual(10, result)
        
        // overflow
        CheckThrowsOverflowException(fun() -> Operators.int64 System.Double.MaxValue |>ignore)
        ()
#endif

//    [<Test>]
//    member this.invalidArg() =         
//        CheckThrowsArgumentException(fun() -> Operators.invalidArg  "A" "B" |>ignore )
//        
//        ()
        
    [<Test>]
    member this.lock() = 
        // lock         
        printfn "test8 started"
        let syncRoot = System.Object()
        let k = ref 0
        let comp _ = async { return lock syncRoot (fun () -> incr k
                                                             System.Threading.Thread.Sleep(1)
                                                             !k ) }
        let arr = Async.RunSynchronously (Async.Parallel(Seq.map comp [1..50]))
        Assert.AreEqual([|1..50|], Array.sort arr)
        
        // without lock
        let syncRoot = System.Object()
        let k = ref 0
        let comp _ = async { do incr k
                             do! Async.Sleep (10)
                             return !k }
        let arr = Async.RunSynchronously (Async.Parallel(Seq.map comp [1..100]))
        Assert.AreNotEqual ([|1..100|], Array.sort arr)
        
        ()
        
    [<Test>]
    member this.log() =  
        // double
        let result = Operators.log 10.0
        Assert.AreEqual(2.3025850929940459, result) 
        
        // negative
        let result = Operators.log -10.0
        Assert.AreEqual(Double.NaN, result) 
        
        // zero
        let result = Operators.log 0.0
        Assert.AreEqual(Double.NegativeInfinity , result) 
        
        ()
        
    [<Test>]
    member this.log10() =  
        // double
        let result = Operators.log10 10.0
        Assert.AreEqual(1.0, result) 
        
        // negative
        let result = Operators.log10 -10.0
        Assert.AreEqual(System.Double.NaN, result)
        
        // zero
        let result = Operators.log10 0.0
        Assert.AreEqual(Double.NegativeInfinity, result) 
        
        ()
        
    [<Test>]
    member this.max() =  
        // value type
        let result = Operators.max 10 8
        Assert.AreEqual(10, result) 
        
        // negative
        let result = Operators.max -10.0 -8.0
        Assert.AreEqual(-8.0, result) 
        
        // zero
        let result = Operators.max 0 0
        Assert.AreEqual(0, result) 
        
        // reference type
        let result = Operators.max "A" "ABC"
        Assert.AreEqual("ABC", result) 
        
        // overflow
        CheckThrowsOverflowException(fun() -> Operators.max 10 System.Int32.MaxValue+1 |>ignore)
        
        ()
        
    [<Test>]
    member this.min() =  
        // value type
        let result = Operators.min 10 8
        Assert.AreEqual(8, result) 
        
        // negative
        let result = Operators.min -10.0 -8.0
        Assert.AreEqual(-10.0, result) 
        
        // zero
        let result = Operators.min 0 0
        Assert.AreEqual(0, result) 
        
        // reference type
        let result = Operators.min "A" "ABC"
        Assert.AreEqual("A", result) 
        
        // overflow
        CheckThrowsOverflowException(fun() -> Operators.min 10 System.Int32.MinValue - 1 |>ignore)
        
        ()
        
    [<Test>]
    member this.nan() =  
        // value type
        let result = Operators.nan 
        Assert.AreEqual(System.Double.NaN, nan) 
        
        ()
        
    [<Test>]
    member this.nanf() =  
        // value type
        let result = Operators.nanf 
        Assert.AreEqual(System.Single.NaN, result) 
        
        ()

#if IGNORED
    [<Test; Ignore( "[FSharp Bugs 1.0] #3842 - OverflowException does not pop up on Operators.int int16 int 32 int64 ")>]
    member this.nativeint() =  
        // int 
        let result = Operators.nativeint 10
        Assert.AreEqual(10n, result)
        
        // double
        let result = Operators.nativeint 10.0
        Assert.AreEqual(10n, result) 
        
        // int64
        let result = Operators.nativeint 10L
        Assert.AreEqual(10n, result)         
       
        // negative
        let result = Operators.nativeint -10
        Assert.AreEqual(-10n, result) 
        
        // zero
        let result = Operators.nativeint 0
        Assert.AreEqual(0n, result) 
        
        // overflow
        CheckThrowsOverflowException(fun() -> Operators.nativeint System.Double.MaxValue |>ignore)
        
        ()
#endif

    [<Test>]
    member this.not() =  
        let result = Operators.not true
        Assert.IsFalse(result)
        
        let result = Operators.not false
        Assert.IsTrue(result) 
        
        ()
        
//    [<Test>]
//    member this.nullArg() =  
//        CheckThrowsArgumentNullException(fun() -> Operators.nullArg "A" |> ignore)
//          
//        ()
        
    [<Test>]
    member this.pown() =  
        // int 
        let result = Operators.pown 10 2
        Assert.AreEqual(100, result)
        
        // double
        let result = Operators.pown 10.0 2
        Assert.AreEqual(100.0, result) 
        
        // int64
        let result = Operators.pown 10L 2
        Assert.AreEqual(100L, result) 
        
        // decimal
        let result = Operators.pown 10M 2
        Assert.AreEqual(100M, result) 
        
        // negative
        let result = Operators.pown -10 2
        Assert.AreEqual(100, result) 
        
        // zero
        let result = Operators.pown 0 2
        Assert.AreEqual(0, result) 
        
        // overflow
        let result = Operators.pown System.Double.MaxValue System.Int32.MaxValue
        Assert.AreEqual(Double.PositiveInfinity, result) 
        
        CheckThrowsOverflowException(fun() -> Operators.pown System.Int32.MaxValue System.Int32.MaxValue |>ignore)
        
        ()
        
    [<Test>]
    member this.raise() =  
        CheckThrowsArgumentException(fun()-> Operators.raise <| new ArgumentException("Invalid Argument ")  |> ignore)
          
        ()
        
    
    [<Test>]
    member this.ref() =
        // value type
        let result = Operators.ref 0    
        let funInt (x:int) =   
            result := !result + x              
            () 
        Array.iter funInt [|1..10|]  
        Assert.AreEqual(!result,55)
        
        // reference type
        let result = Operators.ref ""
        let funStr (x : string) =
            result := (!result) + x   
            ()
        Array.iter funStr [|"A";"B";"C";"D"|]
        Assert.AreEqual(!result,"ABCD")
        
        ()    
    
    [<Test>]
    member this.reraise() =
        // double
        try
            ()
        with
        | _ ->    Operators.reraise()
        
        ()
    
    [<Test>]
    member this.round() =
        // double
        let result = Operators.round 10.0
        Assert.AreEqual(10.0, result) 
        
        // decimal
        let result = Operators.round 10M
        Assert.AreEqual(10M, result)
        
        ()
    
    [<Test>]
    member this.sbyte() =         
        // int 
        let result = Operators.sbyte 10
        Assert.AreEqual(10y, result)
        
        // double
        let result = Operators.sbyte 10.0
        Assert.AreEqual(10y, result) 
        
        // negative
        let result = Operators.sbyte -10
        Assert.AreEqual(-10y, result) 
        
        // zero
        let result = Operators.sbyte 0
        Assert.AreEqual(0y, result) 
        
        ()
    
    [<Test>]
    member this.sign() =         
        // int 
        let result = Operators.sign 10
        Assert.AreEqual(1, result)
        
        // double
        let result = Operators.sign 10.0
        Assert.AreEqual(1, result) 
        
        // negative
        let result = Operators.sign -10
        Assert.AreEqual(-1, result) 
        
        // zero
        let result = Operators.sign 0
        Assert.AreEqual(0, result) 
        
        ()
    
    [<Test>]
    member this.sin() = 
        
        let result = Operators.sin 0.5
        Assert.AreEqual(0.479425538604203, result)      
        
        ()
    
    [<Test>]
    member this.single() = 
        // int 
        let result = Operators.float32 10
        Assert.AreEqual(10f, result)
        
        // double
        let result = Operators.float32 10.0
        Assert.AreEqual(10f, result) 
        
        // string
        let result = Operators.float32 "10"
        Assert.AreEqual(10f, result) 
                
        ()
    
    [<Test>]
    member this.sinh() = 
     
        let result = Operators.sinh 1.0
        Assert.AreEqual(1.1752011936438014, result) 
        
        ()
    
    [<Test>]
    member this.sizeof() = 
        // value type        
        let result = Operators.sizeof<int>
        Assert.AreEqual(4, result) 
        
        // System.Int64        
        let result = Operators.sizeof<System.Int64>
        Assert.AreEqual(8, result) 
        
        // reference type        
        let result = Operators.sizeof<string>
        Assert.AreEqual(4, result) 
        
        // null        
        let result = Operators.sizeof<unit>
        Assert.AreEqual(4, result) 
        
        ()
    
    [<Test>]
    member this.snd() = 
        // value type        
        let result = Operators.snd ("ABC",100)
        Assert.AreEqual(100, result) 
        
        // reference type        
        let result = Operators.snd (100,"ABC")
        Assert.AreEqual("ABC", result) 
        
        // null        
        let result = Operators.snd (100,null)
        Assert.AreEqual(null, result) 
        
        ()
    
    [<Test>]
    member this.sqrt() = 
        // double        
        let result = Operators.sqrt 100.0
        Assert.AreEqual(10.0, result) 
        
        ()
    
    [<Test>]
    member this.stderr() =         
        let result = Operators.stderr 
        Assert.AreEqual(null, result.WriteLine("go")) 
        
        ()
    
    [<Test>]
    member this.stdin() =         
        let result = Operators.stdin 
        Assert.AreEqual(null, result.Dispose())
        
        ()   
    
    [<Test>]
    member this.stdout() =         
        let result = Operators.stdout 
        Assert.AreEqual(null, result.WriteLine("go"))
        
        ()   
    
    [<Test>]
    member this.string() =  
        // value type
        let result = Operators.string 100
        Assert.AreEqual("100", result)
        
        // reference type
        let result = Operators.string "ABC"
        Assert.AreEqual("ABC", result)
        
        ()      
    
    [<Test>]
    member this.tan() =  
        // double
        let result = Operators.tan 1.0
        Assert.AreEqual(1.5574077246549023, result)
        
        ()    
    
    [<Test>]
    member this.tanh() =  
        // this doesn't work, even though the values are roundtrippable correct
        // it also works correctly in FSI, so what gives?
        //let result = Operators.tanh 0.8
        //Assert.AreEqual(0.66403677026784891, result)
        
        let result = Operators.tanh 0.8
        // why does the following not compile?
        //Assert.IsTrue(result = 0.66403677026784891)
        // but the following does?
        // Assert.IsTrue(0.5 = 0.66403677026784891)
        // this works in FSI, but fails just like Assert.AreEqual, what is wrong here?
        Assert.IsTrue(result.Equals(0.66403677026784891))

        // double
        let result = Operators.tanh 0.5
        Assert.AreEqual(0.46211715726000974, result)
        ()    
    
    [<Test>]
    member this.truncate() =        
        // double
        let result = Operators.truncate 10.101
        Assert.AreEqual(10.0, result)
        
        // decimal
        let result = Operators.truncate 10.101M
        Assert.AreEqual(10M, result)
        
        // zero
        let result = Operators.truncate 0.101
        Assert.AreEqual(0.0, result)
        
        ()    
    
    [<Test>]
    member this.typedefof() =        
        // value type
        let result = Operators.typedefof<int>
        Assert.AreEqual("System.Int32", result.FullName)
        
        // reference type
        let result = Operators.typedefof<string>
        Assert.AreEqual("System.String", result.FullName)
        
        // unit
        let result = Operators.typedefof<unit>
        Assert.AreEqual("Microsoft.FSharp.Core.Unit", result.FullName)
        
        ()
    
    [<Test>]
    member this.typeof() =        
        // value type
        let result = Operators.typeof<int>
        Assert.AreEqual("System.Int32", result.FullName)
        
        // reference type
        let result = Operators.typeof<string>
        Assert.AreEqual("System.String", result.FullName)
        
        // unit
        let result = Operators.typeof<unit>
        Assert.AreEqual("Microsoft.FSharp.Core.Unit", result.FullName)
        
        ()
    
    [<Test>]
    member this.uint16() =        
        // int        
        let result = Operators.uint16 100
        Assert.AreEqual(100us, result)
        
        // double
        let result = Operators.uint16 (100.0:double)
        Assert.AreEqual(100us, result)
        
        // decimal
        let result = Operators.uint16 100M
        Assert.AreEqual(100us, result)
        
        ()
    
    [<Test>]
    member this.uint32() =        
        // int
        let result = Operators.uint32 100
        Assert.AreEqual(100ul, result)
        
        // double
        let result = Operators.uint32 (100.0:double)
        Assert.AreEqual(100ul, result)
        
        // decimal
        let result = Operators.uint32 100M
        Assert.AreEqual(100ul, result)
        
        ()
    
    [<Test>]
    member this.uint64() =        
        // int
        let result = Operators.uint64 100
        Assert.AreEqual(100UL, result)
        
        // double
        let result = Operators.uint64 (100.0:double)
        Assert.AreEqual(100UL, result)
        
        // decimal
        let result = Operators.uint64 100M
        Assert.AreEqual(100UL, result)
            
        ()   
    
    [<Test>]
    member this.unativeint() =        
        // int
        let result = Operators.unativeint 100
        Assert.AreEqual(100un, result)
        
        // double
        let result = Operators.unativeint (100.0:double)
        Assert.AreEqual(100un, result)
            
        ()     
    
    [<Test>]
    member this.unbox() =        
        // value type
        let oint = box 100
        let result = Operators.unbox oint
        Assert.AreEqual(100, result)
        
        // reference type
        let ostr = box "ABC"
        let result = Operators.unbox ostr
        Assert.AreEqual("ABC", result)
        
        // null 
        let onull = box null
        let result = Operators.unbox onull
        Assert.AreEqual(null, result)
            
        ()     
    
    [<Test>]
    member this.using() =
        let sr = new System.IO.StringReader("ABCD")
        Assert.AreEqual(sr.ReadToEnd(),"ABCD")
        let result = Operators.using sr (fun x -> x.ToString())        
        CheckThrowsObjectDisposedException(fun () -> sr.ReadToEnd() |> ignore)
        
        ()    
    