// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

// NOTE: the code in this file is a drop-in replacement runtime for Parsing.fs from the FsLexYacc repository

namespace  Internal.Utilities.Text.Parsing

open Internal.Utilities.Text.Lexing

open System
open System.Buffers

exception RecoverableParseError
exception Accept of obj

[<Sealed>]
type internal IParseState(ruleStartPoss:Position[], ruleEndPoss:Position[], lhsPos:Position[], ruleValues:obj[], lexbuf:LexBuffer<char>) = 
    member p.LexBuffer = lexbuf

    member p.InputRange index = ruleStartPoss.[index-1], ruleEndPoss.[index-1]

    member p.InputStartPosition index = ruleStartPoss.[index-1]

    member p.InputEndPosition index = ruleEndPoss.[index-1]

    member p.ResultStartPosition    = lhsPos.[0]

    member p.ResultEndPosition    = lhsPos.[1]

    member p.GetInput index    = ruleValues.[index-1]        

    member p.ResultRange    = (lhsPos.[0], lhsPos.[1])  

    // Side note: this definition coincidentally tests the fairly complex logic associated with an object expression implementing a generic abstract method.
    member p.RaiseError()  = raise RecoverableParseError 


[<Sealed>]
/// This context is passed to the error reporter when a syntax error occurs
type internal ParseErrorContext<'tok>
         (//lexbuf: LexBuffer<_>,
          stateStack:int list,
          parseState: IParseState, 
          reduceTokens: int list, 
          currentToken: 'tok option, 
          reducibleProductions: int list list, 
          shiftableTokens: int list , 
          message : string) =
      //member x.LexBuffer = lexbuf
      member x.StateStack  = stateStack
      member x.ReduceTokens = reduceTokens
      member x.CurrentToken = currentToken
      member x.ParseState = parseState
      member x.ReducibleProductions = reducibleProductions
      member x.ShiftTokens = shiftableTokens
      member x.Message = message


//-------------------------------------------------------------------------
// This is the data structure emitted as code by FSYACC.  

type internal Tables<'tok> = 
    { reductions: (IParseState -> obj)[]
      endOfInputTag: int
      tagOfToken: 'tok -> int
      dataOfToken: 'tok -> obj 
      actionTableElements: uint16[]  
      actionTableRowOffsets: uint16[]
      reductionSymbolCounts: uint16[]
      immediateActions: uint16[]
      gotos: uint16[]
      sparseGotoTableRowOffsets: uint16[]
      stateToProdIdxsTableElements: uint16[]  
      stateToProdIdxsTableRowOffsets: uint16[]  
      productionToNonTerminalTable: uint16[]
      /// For <c>fsyacc.exe</c>, this entry is filled in by context from the generated parser file. If no 'parse_error' function
      /// is defined by the user then <c>ParseHelpers.parse_error</c> is used by default (ParseHelpers is opened
      /// at the top of the generated parser file)
      parseError:  ParseErrorContext<'tok> -> unit
      numTerminals: int
      tagOfErrorTerminal: int }

//-------------------------------------------------------------------------
// An implementation of stacks.

// This type is in <c>System.dll</c> so for the moment we can't use it in <c>FSharp.Core.dll</c>
// type Stack<'a> = System.Collections.Generic.Stack<'a>

type Stack<'a>(n)  = 
    let mutable contents = Array.zeroCreate<'a>(n)
    let mutable count = 0

    member buf.Ensure newSize = 
        let oldSize = contents.Length
        if newSize > oldSize then 
            let old = contents
            contents <- Array.zeroCreate (max newSize (oldSize * 2))
            Array.blit old 0 contents 0 count
    
    member buf.Count = count
    member buf.Pop() = count <- count - 1
    member buf.Peep() = contents.[count - 1]
    member buf.Top(n) = [ for x in contents.[max 0 (count-n)..count - 1] -> x ] |> List.rev
    member buf.Push(x) =
        buf.Ensure(count + 1) 
        contents.[count] <- x 
        count <- count + 1
        
    member buf.IsEmpty = (count = 0)
    member buf.PrintStack() = 
        for i = 0 to (count - 1) do 
            Console.Write("{0}{1}",contents.[i],if i=count-1 then ":" else "-") 
          

#if DEBUG
module Flags = 
    let mutable debug = false
#endif

module internal Implementation = 
    
    // Definitions shared with fsyacc 
    let anyMarker = 0xffff
    let shiftFlag = 0x0000
    let reduceFlag = 0x4000
    let errorFlag = 0x8000
    let acceptFlag = 0xc000
    let actionMask = 0xc000

    let actionValue action = action &&& (~~~ actionMask)                                    
    let actionKind action = action &&& actionMask
    
    //-------------------------------------------------------------------------
    // Read the tables written by FSYACC.  

    type AssocTable(elemTab: uint16[], offsetTab: uint16[], cache: int[], cacheSize: int) =

        member t.ReadAssoc (minElemNum,maxElemNum,defaultValueOfAssoc,keyToFind) =     
            // do a binary chop on the table 
            let elemNumber : int = (minElemNum+maxElemNum)/2
            if elemNumber = maxElemNum 
            then defaultValueOfAssoc
            else 
                let x = int elemTab.[elemNumber*2]
                if keyToFind = x then int elemTab.[elemNumber*2+1]
                elif keyToFind < x then t.ReadAssoc (minElemNum ,elemNumber,defaultValueOfAssoc,keyToFind)
                else                    t.ReadAssoc (elemNumber+1,maxElemNum,defaultValueOfAssoc,keyToFind)

        member t.Read(rowNumber,keyToFind) =

            // First check the sparse lookaside table
            // Performance note: without this lookaside table the binary chop in ReadAssoc
            // takes up around 10% of of parsing time 
            // for parsing intensive samples such as the bootstrapped F# compiler.
            //
            // NOTE: using a .NET Dictionary for this int -> int table looks like it could be sub-optimal.
            // Some other better sparse lookup table may be better.
            assert (rowNumber < 0x10000)
            assert (keyToFind < 0x10000)
            let cacheKey = (rowNumber <<< 16) ||| keyToFind
            let cacheIdx = (int32 (uint32 cacheKey % uint32 cacheSize)) * 2
            let cacheKey2 = cache.[cacheIdx]
            let v = cache.[cacheIdx+1]
            if cacheKey = cacheKey2 then v 
            else
                let headOfTable = int offsetTab.[rowNumber]
                let firstElemNumber = headOfTable + 1           
                let numberOfElementsInAssoc = int elemTab.[headOfTable*2]
                let defaultValueOfAssoc = int elemTab.[headOfTable*2+1]          
                let res = t.ReadAssoc (firstElemNumber,firstElemNumber+numberOfElementsInAssoc,defaultValueOfAssoc,keyToFind)
                cache.[cacheIdx] <- cacheKey
                cache.[cacheIdx+1] <- res
                res

        // Read all entries in the association table
        // Used during error recovery to find all valid entries in the table
        member x.ReadAll(n) =       
            let headOfTable = int offsetTab.[n]
            let firstElemNumber = headOfTable + 1           
            let numberOfElementsInAssoc = int32 elemTab.[headOfTable*2]           
            let defaultValueOfAssoc = int elemTab.[headOfTable*2+1]          
            [ for i in firstElemNumber .. (firstElemNumber+numberOfElementsInAssoc-1) -> 
                (int elemTab.[i*2], int elemTab.[i*2+1]) ], defaultValueOfAssoc

    type IdxToIdxListTable(elemTab:uint16[], offsetTab:uint16[]) =

        // Read all entries in a row of the table
        member x.ReadAll(n) =       
            let headOfTable = int offsetTab.[n]
            let firstElemNumber = headOfTable + 1           
            let numberOfElements = int32 elemTab.[headOfTable]           
            [ for i in firstElemNumber .. (firstElemNumber+numberOfElements-1) -> int elemTab.[i] ]

    //-------------------------------------------------------------------------
    // interpret the tables emitted by FSYACC.  

    [<NoEquality; NoComparison>]
    [<Struct>]
    type ValueInfo = 
        val value: obj
        val startPos: Position
        val endPos: Position
        new(value,startPos,endPos) = { value=value; startPos=startPos;endPos=endPos }

    let interpret (tables: Tables<'tok>) lexer (lexbuf : LexBuffer<_>) initialState =                                                                      
#if DEBUG
        if Flags.debug then Console.WriteLine("\nParser: interpret tables")
#endif
        let stateStack : Stack<int> = new Stack<_>(100)
        stateStack.Push(initialState)
        let valueStack = new Stack<ValueInfo>(100)
        let mutable haveLookahead = false                                                                              
        let mutable lookaheadToken = Unchecked.defaultof<'tok>
        let mutable lookaheadEndPos = Unchecked.defaultof<Position>
        let mutable lookaheadStartPos = Unchecked.defaultof<Position>
        let mutable finished = false
        // After an error occurs, we suppress errors until we've shifted three tokens in a row.
        let mutable errorSuppressionCountDown = 0
        
        // When we hit the end-of-file we don't fail straight away but rather keep permitting shift
        // and reduce against the last token in the token stream 20 times or until we've accepted
        // or exhausted the stack. This allows error recovery rules of the form
        //      input : realInput EOF | realInput error EOF | error EOF
        // where consuming one EOF to trigger an error doesn't result in overall parse failure 
        // catastrophe and the loss of intermediate results.
        //
        let mutable inEofCountDown = false
        let mutable eofCountDown = 20 // Number of EOFs to supply at the end for error recovery
        // The 100 here means a maximum of 100 elements for each rule
        let ruleStartPoss = (Array.zeroCreate 100 : Position[])              
        let ruleEndPoss   = (Array.zeroCreate 100 : Position[])              
        let ruleValues    = (Array.zeroCreate 100 : obj[])              
        let lhsPos        = (Array.zeroCreate 2 : Position[])                                            
        let reductions = tables.reductions
        let cacheSize = 7919 // the 1000'th prime
        // Use a simpler hash table with faster lookup, but only one
        // hash bucket per key.
        let actionTableCache = ArrayPool<int>.Shared.Rent(cacheSize * 2)
        let gotoTableCache = ArrayPool<int>.Shared.Rent(cacheSize * 2)
        // Clear the arrays since ArrayPool does not
        Array.Clear(actionTableCache, 0, actionTableCache.Length)
        Array.Clear(gotoTableCache, 0, gotoTableCache.Length)
        use _cacheDisposal = 
            { new IDisposable with 
                member _.Dispose() = 
                    ArrayPool<int>.Shared.Return actionTableCache
                    ArrayPool<int>.Shared.Return gotoTableCache }
        let actionTable = AssocTable(tables.actionTableElements, tables.actionTableRowOffsets, actionTableCache, cacheSize)
        let gotoTable = AssocTable(tables.gotos, tables.sparseGotoTableRowOffsets, gotoTableCache, cacheSize)
        let stateToProdIdxsTable = new IdxToIdxListTable(tables.stateToProdIdxsTableElements, tables.stateToProdIdxsTableRowOffsets)

        let parseState =                                                                                            
            new IParseState(ruleStartPoss,ruleEndPoss,lhsPos,ruleValues,lexbuf)

#if DEBUG
        let report haveLookahead lookaheadToken = 
            if haveLookahead then sprintf "%+A" lookaheadToken 
            else "[TBC]"
#endif

        // Pop the stack until we can shift the 'error' token. If 'tokenOpt' is given
        // then keep popping until we can shift both the 'error' token and the token in 'tokenOpt'.
        // This is used at end-of-file to make sure we can shift both the 'error' token and the 'EOF' token.
        let rec popStackUntilErrorShifted tokenOpt =
            // Keep popping the stack until the "error" terminal is shifted
#if DEBUG
            if Flags.debug then Console.WriteLine("popStackUntilErrorShifted")
#endif
            if stateStack.IsEmpty then 
#if DEBUG
                if Flags.debug then 
                    Console.WriteLine("state stack empty during error recovery - generating parse error")
#endif
                failwith "parse error"
            
            let currState = stateStack.Peep()
#if DEBUG
            if Flags.debug then 
                Console.WriteLine("In state {0} during error recovery", currState)
#endif
            
            let action = actionTable.Read(currState, tables.tagOfErrorTerminal)
            
            if actionKind action = shiftFlag &&  
                (match tokenOpt with 
                 | None -> true
                 | Some(token) -> 
                    let nextState = actionValue action 
                    actionKind (actionTable.Read(nextState, tables.tagOfToken(token))) = shiftFlag) then

#if DEBUG
                if Flags.debug then Console.WriteLine("shifting error, continuing with error recovery")
#endif
                let nextState = actionValue action 
                // The "error" non terminal needs position information, though it tends to be unreliable.
                // Use the StartPos/EndPos from the lex buffer.
                valueStack.Push(ValueInfo(box (), lexbuf.StartPos, lexbuf.EndPos))
                stateStack.Push(nextState)
            else
                if valueStack.IsEmpty then 
                    failwith "parse error"
#if DEBUG
                if Flags.debug then 
                    Console.WriteLine("popping stack during error recovery")
#endif
                valueStack.Pop()
                stateStack.Pop()
                popStackUntilErrorShifted(tokenOpt)

        while not finished do                                                                                    
            if stateStack.IsEmpty then 
                finished <- true
            else
                let state = stateStack.Peep()
#if DEBUG
                if Flags.debug then (Console.Write("{0} value(state), state ",valueStack.Count); stateStack.PrintStack())
#endif
                let action = 
                    let immediateAction = int tables.immediateActions.[state]
                    if not (immediateAction = anyMarker) then
                        // Action has been pre-determined, no need to lookahead 
                        // Expecting it to be a Reduce action on a non-fakeStartNonTerminal ? 
                        immediateAction
                    else
                        // Lookahead required to determine action 
                        if not haveLookahead then 
                            if lexbuf.IsPastEndOfStream then 
                                // When the input runs out, keep supplying the last token for eofCountDown times
                                if eofCountDown>0 then
                                    haveLookahead <- true
                                    eofCountDown <- eofCountDown - 1
                                    inEofCountDown <- true
                                else 
                                    haveLookahead <- false
                            else 
                                lookaheadToken <- lexer lexbuf
                                lookaheadStartPos <- lexbuf.StartPos
                                lookaheadEndPos <- lexbuf.EndPos
                                haveLookahead <- true

                        let tag = 
                            if haveLookahead then tables.tagOfToken lookaheadToken 
                            else tables.endOfInputTag   
                                    
                        // printf "state %d\n" state  
                        actionTable.Read(state,tag)
                        
                let kind = actionKind action 
                if kind = shiftFlag then (
                    if errorSuppressionCountDown > 0 then 
                        errorSuppressionCountDown <- errorSuppressionCountDown - 1
#if DEBUG
                        if Flags.debug then Console.WriteLine("shifting, reduced errorRecoveryLevel to {0}\n", errorSuppressionCountDown)
#endif
                    let nextState = actionValue action                                     
                    if not haveLookahead then failwith "shift on end of input!"
                    let data = tables.dataOfToken lookaheadToken
                    valueStack.Push(ValueInfo(data, lookaheadStartPos, lookaheadEndPos))
                    stateStack.Push(nextState)                                                                
#if DEBUG
                    if Flags.debug then Console.WriteLine("shift/consume input {0}, shift to state {1}", report haveLookahead lookaheadToken, nextState)
#endif
                    haveLookahead <- false

                ) elif kind = reduceFlag then
                    let prod = actionValue action                                     
                    let reduction = reductions.[prod]                                                             
                    let n = int tables.reductionSymbolCounts.[prod]
                       // pop the symbols, populate the values and populate the locations                              
#if DEBUG
                    if Flags.debug then Console.Write("reduce popping {0} values/states, lookahead {1}", n, report haveLookahead lookaheadToken)
#endif
                    // For every range to reduce merge it
                    for i = 0 to n - 1 do
                        if valueStack.IsEmpty then failwith "empty symbol stack"
                        let topVal = valueStack.Peep()                                  // Grab topVal
                        valueStack.Pop()
                        stateStack.Pop()

                        let ruleIndex = (n-i)-1
                        ruleValues.[ruleIndex] <- topVal.value
                        ruleStartPoss.[ruleIndex] <- topVal.startPos
                        ruleEndPoss.[ruleIndex] <- topVal.endPos

                        if i = 0 then
                            // Initial range
                            lhsPos.[0] <- topVal.startPos
                            lhsPos.[1] <- topVal.endPos
                        elif topVal.startPos.FileIndex = lhsPos.[1].FileIndex && topVal.startPos.Line <= lhsPos.[1].Line then
                            // Reduce range if same file as the initial end point
                            lhsPos.[0] <- topVal.startPos

                    // Use the lookahead token to populate the locations if the rhs is empty
                    if n = 0 then
                        if haveLookahead then 
                           lhsPos.[0] <- lookaheadStartPos
                           lhsPos.[1] <- lookaheadEndPos
                        else 
                           lhsPos.[0] <- lexbuf.StartPos
                           lhsPos.[1] <- lexbuf.EndPos
                    try
                          // printf "reduce %d\n" prod
                        let redResult = reduction parseState
                        let valueInfo = ValueInfo(redResult, lhsPos.[0], lhsPos.[1])
                        valueStack.Push(valueInfo)
                        let currState = stateStack.Peep()
                        let newGotoState = gotoTable.Read(int tables.productionToNonTerminalTable.[prod], currState)
                        stateStack.Push(newGotoState)

#if DEBUG
                        if Flags.debug then Console.WriteLine(" goto state {0}", newGotoState)
#endif
                    with
                    | Accept res ->
                          finished <- true
                          valueStack.Push(ValueInfo(res, lhsPos.[0], lhsPos.[1])) 
                    | RecoverableParseError ->
#if DEBUG
                          if Flags.debug then Console.WriteLine("RecoverableParseErrorException...\n")
#endif
                          popStackUntilErrorShifted(None)
                          // User code raised a Parse_error. Don't report errors again until three tokens have been shifted 
                          errorSuppressionCountDown <- 3
                elif kind = errorFlag then (
#if DEBUG
                    if Flags.debug then Console.Write("ErrorFlag... ")
#endif
                    // Silently discard inputs and don't report errors 
                    // until three tokens in a row have been shifted 
#if DEBUG
                    if Flags.debug then printfn "error on token '%s' " (report haveLookahead lookaheadToken)
#endif
                    if errorSuppressionCountDown > 0 then 
                        // If we're in the end-of-file count down then we're very keen to 'Accept'.
                        // We can only do this by repeatedly popping the stack until we can shift both an 'error' token
                        // and an EOF token. 
                        if inEofCountDown && eofCountDown < 10 then 
#if DEBUG
                            if Flags.debug then printfn "popping stack, looking to shift both 'error' and that token, during end-of-file error recovery" 
#endif
                            popStackUntilErrorShifted(if haveLookahead then Some(lookaheadToken) else None)

                        // If we don't haveLookahead then the end-of-file count down is over and we have no further options.
                        if not haveLookahead then 
                            failwith "parse error: unexpected end of file"
                            
#if DEBUG
                        if Flags.debug then printfn "discarding token '%s' during error suppression" (report haveLookahead lookaheadToken)
#endif
                        // Discard the token
                        haveLookahead <- false
                        // Try again to shift three tokens
                        errorSuppressionCountDown <- 3
                    else (

                        let currentToken = if haveLookahead then Some(lookaheadToken) else None
                        let actions,defaultAction = actionTable.ReadAll(state) 
                        let explicit = Set.ofList [ for tag,_action in actions -> tag ]
                        
                        let shiftableTokens = 
                           [ for tag,action in actions do
                                 if (actionKind action) = shiftFlag then 
                                     yield tag
                             if actionKind defaultAction = shiftFlag  then
                                 for tag in 0 .. tables.numTerminals-1 do  
                                    if not (explicit.Contains(tag)) then 
                                         yield tag ] in

                        let stateStack = stateStack.Top(12) in
                        let reducibleProductions = 
                            [ for state in stateStack do 
                               yield stateToProdIdxsTable.ReadAll(state)  ]

                        let reduceTokens = 
                           [ for tag,action in actions do
                                if actionKind(action) = reduceFlag then
                                    yield tag
                             if actionKind(defaultAction) = reduceFlag  then
                                 for tag in 0 .. tables.numTerminals-1 do  
                                    if not (explicit.Contains(tag)) then 
                                         yield tag ] in
                        //let activeRules = stateStack |> List.iter (fun state -> 
                        let errorContext = new ParseErrorContext<'tok>(stateStack, parseState, reduceTokens, currentToken, reducibleProductions, shiftableTokens, "syntax error")
                        tables.parseError(errorContext)
                        popStackUntilErrorShifted(None)
                        errorSuppressionCountDown <- 3
#if DEBUG
                        if Flags.debug then Console.WriteLine("generated syntax error and shifted error token, haveLookahead = {0}\n", haveLookahead)
#endif
                    )
                ) elif kind = acceptFlag then 
                    finished <- true
#if DEBUG
                else
                  if Flags.debug then Console.WriteLine("ALARM!!! drop through case in parser")  
#endif
        done                                                                                                     
        // OK, we're done - read off the overall generated value
        valueStack.Peep().value

type internal Tables<'tok> with
    member tables.Interpret (lexer, lexbuf, initialState) = 
        Implementation.interpret tables lexer lexbuf initialState
    
module internal ParseHelpers = 

    let parse_error (_s:string) = ()

    let parse_error_rich = (None : (ParseErrorContext<_> -> unit) option)
