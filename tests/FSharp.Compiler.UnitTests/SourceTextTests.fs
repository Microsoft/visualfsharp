﻿// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace FSharp.Compiler.UnitTests

open System
open NUnit.Framework

open Microsoft.FSharp.Compiler.Text

[<TestFixture>]
module SourceTextTests =

    [<Test>]
    let StringText () =
        let text = "test\ntest2\r\ntest3\n\ntest4\ntest5\rtest6\n"
        let sourceText = SourceText.ofString text
        let lines = sourceText.GetLines()

        Assert.AreEqual("test",  lines.[0])
        Assert.AreEqual("test2", lines.[1])
        Assert.AreEqual("test3", lines.[2])
        Assert.AreEqual("",      lines.[3])
        Assert.AreEqual("test4", lines.[4])
        Assert.AreEqual("test5", lines.[5])
        Assert.AreEqual("test6", lines.[6])
        Assert.AreEqual("",      lines.[7])
        Assert.AreEqual(8, lines.Length)

        let (count, length) = sourceText.GetLastCharacterPosition()
        Assert.AreEqual(8, count)
        Assert.AreEqual(0, length)