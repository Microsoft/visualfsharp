﻿
#if INTERACTIVE
#r "../../artifacts/bin/fcs/net461/FSharp.Compiler.Service.dll" // note, build FSharp.Compiler.Service.Tests.fsproj to generate this, this DLL has a public API so can be used from F# Interactive
#r "../../artifacts/bin/fcs/net461/nunit.framework.dll"
#load "FsUnit.fs"
#load "Common.fs"
#else
module FSharp.Compiler.Service.Tests.TokenizerTests
#endif

open FSharp.Compiler.SourceCodeServices

open NUnit.Framework


let sourceTok = FSharpSourceTokenizer([], Some "C:\\test.fsx")

let rec parseLine(line: string, state: FSharpTokenizerLexState ref, tokenizer: FSharpLineTokenizer) = seq {
  match tokenizer.ScanToken(!state) with
  | Some(tok), nstate ->
      let str = line.Substring(tok.LeftColumn, tok.RightColumn - tok.LeftColumn + 1)
      yield str, tok
      state := nstate
      yield! parseLine(line, state, tokenizer)
  | None, nstate -> 
      state := nstate }

let tokenizeLines (lines:string[]) =
  [ let state = ref FSharpTokenizerLexState.Initial
    for n, line in lines |> Seq.zip [ 0 .. lines.Length-1 ] do
      let tokenizer = sourceTok.CreateLineTokenizer(line)
      yield n, parseLine(line, state, tokenizer) |> List.ofSeq ]

[<Test>]
let ``Tokenizer test 1``() =
    let tokenizedLines = 
      tokenizeLines
        [| "// Sets the hello wrold variable"
           "let hello = \"Hello world\" " |]

    let actual = 
        [ for lineNo, lineToks in tokenizedLines do
            yield lineNo, [ for str, info in lineToks do yield info.TokenName, str ] ]
    let expected = 
      [(0,
        [("LINE_COMMENT", "//"); ("LINE_COMMENT", " "); ("LINE_COMMENT", "Sets");
         ("LINE_COMMENT", " "); ("LINE_COMMENT", "the"); ("LINE_COMMENT", " ");
         ("LINE_COMMENT", "hello"); ("LINE_COMMENT", " ");
         ("LINE_COMMENT", "wrold"); ("LINE_COMMENT", " ");
         ("LINE_COMMENT", "variable")]);
       (1,
        [("LET", "let"); ("WHITESPACE", " "); ("IDENT", "hello");
         ("WHITESPACE", " "); ("EQUALS", "="); ("WHITESPACE", " ");
         ("STRING_TEXT", "\""); ("STRING_TEXT", "Hello"); ("STRING_TEXT", " ");
         ("STRING_TEXT", "world"); ("STRING", "\""); ("WHITESPACE", " ")])]

    if actual <> expected then 
        printfn "actual   = %A" actual
        printfn "expected = %A" expected
        Assert.Fail(sprintf "actual and expected did not match,actual =\n%A\nexpected=\n%A\n" actual expected)

[<Test>]
let ``Tokenizer test 2``() =
    let tokenizedLines = 
      tokenizeLines
        [| "// Tests tokenizing string interpolation"
           "let hello0 = $\"\""
           "let hello1 = $\"Hello world\"  "
           "let hello2 = $\"Hello world {1+1} = {2}\" "
           "let hello0v = @$\"\""
           "let hello1v = @$\"Hello world\"  "
           "let hello2v = @$\"Hello world {1+1} = {2}\" " 
           "let hello1t = @$\"\"\"abc\"\"\""
           "let hello2t = @$\"\"\"Hello world {1+1} = {2}\"\"\" " |]

    let actual = 
        [ for lineNo, lineToks in tokenizedLines do
            yield lineNo, [ for str, info in lineToks do yield info.TokenName, str ] ]
    let expected = 
        [(0,
          [("LINE_COMMENT", "//"); ("LINE_COMMENT", " "); ("LINE_COMMENT", "Tests");
           ("LINE_COMMENT", " "); ("LINE_COMMENT", "tokenizing"); ("LINE_COMMENT", " ");
           ("LINE_COMMENT", "string"); ("LINE_COMMENT", " ");
           ("LINE_COMMENT", "interpolation")]);
         (1,
          [("LET", "let"); ("WHITESPACE", " "); ("IDENT", "hello0"); ("WHITESPACE", " ");
           ("EQUALS", "="); ("WHITESPACE", " "); ("STRING_TEXT", "$\"");
           ("INTERP_STRING_BEGIN_END", "\"")]);
         (2,
          [("LET", "let"); ("WHITESPACE", " "); ("IDENT", "hello1"); ("WHITESPACE", " ");
           ("EQUALS", "="); ("WHITESPACE", " "); ("STRING_TEXT", "$\"");
           ("STRING_TEXT", "Hello"); ("STRING_TEXT", " "); ("STRING_TEXT", "world");
           ("INTERP_STRING_BEGIN_END", "\""); ("WHITESPACE", "  ")]);
         (3,
          [("LET", "let"); ("WHITESPACE", " "); ("IDENT", "hello2"); ("WHITESPACE", " ");
           ("EQUALS", "="); ("WHITESPACE", " "); ("STRING_TEXT", "$\"");
           ("STRING_TEXT", "Hello"); ("STRING_TEXT", " "); ("STRING_TEXT", "world");
           ("STRING_TEXT", " "); ("INTERP_STRING_BEGIN_PART", "{"); ("INT32", "1");
           ("PLUS_MINUS_OP", "+"); ("INT32", "1"); ("STRING_TEXT", "}");
           ("STRING_TEXT", " "); ("STRING_TEXT", "="); ("STRING_TEXT", " ");
           ("INTERP_STRING_PART", "{"); ("INT32", "2"); ("STRING_TEXT", "}");
           ("INTERP_STRING_END", "\""); ("WHITESPACE", " ")]);
         (4,
          [("LET", "let"); ("WHITESPACE", " "); ("IDENT", "hello0v");
           ("WHITESPACE", " "); ("EQUALS", "="); ("WHITESPACE", " ");
           ("STRING_TEXT", "@$\""); ("INTERP_STRING_BEGIN_END", "\"")]);
         (5,
          [("LET", "let"); ("WHITESPACE", " "); ("IDENT", "hello1v");
           ("WHITESPACE", " "); ("EQUALS", "="); ("WHITESPACE", " ");
           ("STRING_TEXT", "@$\""); ("STRING_TEXT", "Hello"); ("STRING_TEXT", " ");
           ("STRING_TEXT", "world"); ("INTERP_STRING_BEGIN_END", "\"");
           ("WHITESPACE", "  ")]);
         (6,
          [("LET", "let"); ("WHITESPACE", " "); ("IDENT", "hello2v");
           ("WHITESPACE", " "); ("EQUALS", "="); ("WHITESPACE", " ");
           ("STRING_TEXT", "@$\""); ("STRING_TEXT", "Hello"); ("STRING_TEXT", " ");
           ("STRING_TEXT", "world"); ("STRING_TEXT", " ");
           ("INTERP_STRING_BEGIN_PART", "{"); ("INT32", "1"); ("PLUS_MINUS_OP", "+");
           ("INT32", "1"); ("STRING_TEXT", "}"); ("STRING_TEXT", " ");
           ("STRING_TEXT", "="); ("STRING_TEXT", " "); ("INTERP_STRING_PART", "{");
           ("INT32", "2"); ("STRING_TEXT", "}"); ("INTERP_STRING_END", "\"");
           ("WHITESPACE", " ")]);
         (7,
          [("LET", "let"); ("WHITESPACE", " "); ("IDENT", "hello1t");
           ("WHITESPACE", " "); ("EQUALS", "="); ("WHITESPACE", " ");
           ("STRING_TEXT", "@$\""); ("STRING_TEXT", "\"\""); ("STRING_TEXT", "abc");
           ("STRING_TEXT", "\"\""); ("INTERP_STRING_BEGIN_END", "\"")]);
         (8,
          [("LET", "let"); ("WHITESPACE", " "); ("IDENT", "hello2t");
           ("WHITESPACE", " "); ("EQUALS", "="); ("WHITESPACE", " ");
           ("STRING_TEXT", "@$\""); ("STRING_TEXT", "\"\""); ("STRING_TEXT", "Hello");
           ("STRING_TEXT", " "); ("STRING_TEXT", "world"); ("STRING_TEXT", " ");
           ("INTERP_STRING_BEGIN_PART", "{"); ("INT32", "1"); ("PLUS_MINUS_OP", "+");
           ("INT32", "1"); ("STRING_TEXT", "}"); ("STRING_TEXT", " ");
           ("STRING_TEXT", "="); ("STRING_TEXT", " "); ("INTERP_STRING_PART", "{");
           ("INT32", "2"); ("STRING_TEXT", "}"); ("STRING_TEXT", "\"\"");
           ("INTERP_STRING_END", "\""); ("WHITESPACE", " ")])]
  
    if actual <> expected then 
        printfn "actual   = %A" actual
        printfn "expected = %A" expected
        Assert.Fail(sprintf "actual and expected did not match,actual =\n%A\nexpected=\n%A\n" actual expected)

