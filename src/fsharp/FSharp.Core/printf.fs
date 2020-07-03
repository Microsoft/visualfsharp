// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace Microsoft.FSharp.Core

open System
open System.Collections.Concurrent
open System.Globalization
open System.Reflection

open Microsoft.FSharp.Core
open Microsoft.FSharp.Core.Operators
open Microsoft.FSharp.Collections

open LanguagePrimitives.IntrinsicOperators

type PrintfFormat<'Printer, 'State, 'Residue, 'Result>(value:string, captures: obj[], captureTys: Type[]) =
        
    new (value) = new PrintfFormat<'Printer, 'State, 'Residue, 'Result>(value, null, null) 

    member _.Value = value

    member _.Captures = captures

    member _.CaptureTypes = captureTys

    override _.ToString() = value
    
type PrintfFormat<'Printer, 'State, 'Residue, 'Result, 'Tuple>(value:string, captures, captureTys: Type[]) = 

    inherit PrintfFormat<'Printer, 'State, 'Residue, 'Result>(value, captures, captureTys)

    new (value) = new PrintfFormat<'Printer, 'State, 'Residue, 'Result, 'Tuple>(value, null, null)

type Format<'Printer, 'State, 'Residue, 'Result> = PrintfFormat<'Printer, 'State, 'Residue, 'Result>

type Format<'Printer, 'State, 'Residue, 'Result, 'Tuple> = PrintfFormat<'Printer, 'State, 'Residue, 'Result, 'Tuple>

module internal PrintfImpl =

    /// Basic idea of implementation:
    /// Every Printf.* family should returns curried function that collects arguments and then somehow prints them.
    /// Idea - instead of building functions on fly argument by argument we instead introduce some predefined parts and then construct functions from these parts
    /// Parts include:
    /// Plain ones:
    /// 1. Final pieces (1..5) - set of functions with arguments number 1..5. 
    /// Primary characteristic - these functions produce final result of the *printf* operation
    /// 2. Chained pieces (1..5) - set of functions with arguments number 1..5. 
    /// Primary characteristic - these functions doesn not produce final result by itself, instead they tailed with some another piece (chained or final).
    /// Plain parts correspond to simple format specifiers (that are projected to just one parameter of the function, say %d or %s). However we also have 
    /// format specifiers that can be projected to more than one argument (i.e %a, %t or any simple format specified with * width or precision). 
    /// For them we add special cases (both chained and final to denote that they can either return value themselves or continue with some other piece)
    /// These primitives allow us to construct curried functions with arbitrary signatures.
    /// For example: 
    /// - function that corresponds to %s%s%s%s%s (string -> string -> string -> string -> string -> T) will be represented by one piece final 5.
    /// - function that has more that 5 arguments will include chained parts: %s%s%s%s%s%d%s  => chained2 -> final 5
    /// Primary benefits: 
    /// 1. creating specialized version of any part requires only one reflection call. This means that we can handle up to 5 simple format specifiers
    /// with just one reflection call
    /// 2. we can make combinable parts independent from particular printf implementation. Thus final result can be cached and shared. 
    /// i.e when first call to printf "%s %s" will trigger creation of the specialization. Subsequent calls will pick existing specialization
    
    [<Flags>]
    type FormatFlags = 
        | None = 0
        | LeftJustify = 1
        | PadWithZeros = 2
        | PlusForPositives = 4
        | SpaceForPositives = 8

    let inline hasFlag flags (expected: FormatFlags) = (flags &&& expected) = expected
    let inline isLeftJustify flags = hasFlag flags FormatFlags.LeftJustify
    let inline isPadWithZeros flags = hasFlag flags FormatFlags.PadWithZeros
    let inline isPlusForPositives flags = hasFlag flags FormatFlags.PlusForPositives
    let inline isSpaceForPositives flags = hasFlag flags FormatFlags.SpaceForPositives

    /// Used for width and precision to denote that user has specified '*' flag
    [<Literal>]
    let StarValue = -1
    /// Used for width and precision to denote that corresponding value was omitted in format string
    [<Literal>]
    let NotSpecifiedValue = -2

    [<System.Diagnostics.DebuggerDisplayAttribute("{ToString()}")>]
    [<NoComparison; NoEquality>]
    type FormatSpecifier =
        {
            TypeChar: char
            Precision: int
            Width: int
            Flags: FormatFlags
            InteropHoleDotNetFormat: string option
        }
        member spec.IsStarPrecision = (spec.Precision = StarValue)

        member spec.IsPrecisionSpecified = (spec.Precision <> NotSpecifiedValue)

        member spec.IsStarWidth = (spec.Width = StarValue)

        member spec.IsWidthSpecified = (spec.Width <> NotSpecifiedValue)

        member spec.ArgCount = 
            let n = 
                if spec.TypeChar = 'a' then 2 
                elif spec.IsStarWidth || spec.IsStarPrecision then
                    if spec.IsStarWidth = spec.IsStarPrecision then 3 
                    else 2
                else 1

            let n = if spec.TypeChar = '%' then n - 1 else n
                
            System.Diagnostics.Debug.Assert(n <> 0, "n <> 0")

            n

        override spec.ToString() = 
            let valueOf n = match n with StarValue -> "*" | NotSpecifiedValue -> "-" | n -> n.ToString()
            System.String.Format
                (
                    "'{0}', Precision={1}, Width={2}, Flags={3}", 
                    spec.TypeChar, 
                    (valueOf spec.Precision),
                    (valueOf spec.Width), 
                    spec.Flags
                )

        member spec.IsDecimalFormat = 
            spec.TypeChar = 'M'

        member spec.GetPadAndPrefix allowZeroPadding = 
            let padChar = if allowZeroPadding && isPadWithZeros spec.Flags then '0' else ' ';
            let prefix = 
                if isPlusForPositives spec.Flags then "+" 
                elif isSpaceForPositives spec.Flags then " "
                else ""
            padChar, prefix    

        member spec.IsGFormat = 
            spec.IsDecimalFormat || System.Char.ToLower(spec.TypeChar) = 'g'

    
    /// Set of helpers to parse format string
    module private FormatString =

        let intFromString (s: string) pos =
            let rec go acc i =
                if Char.IsDigit s.[i] then 
                    let n = int s.[i] - int '0'
                    go (acc * 10 + n) (i + 1)
                else acc, i
            go 0 pos

        let parseFlags (s: string) i = 
            let rec go flags i = 
                match s.[i] with
                | '0' -> go (flags ||| FormatFlags.PadWithZeros) (i + 1)
                | '+' -> go (flags ||| FormatFlags.PlusForPositives) (i + 1)
                | ' ' -> go (flags ||| FormatFlags.SpaceForPositives) (i + 1)
                | '-' -> go (flags ||| FormatFlags.LeftJustify) (i + 1)
                | _ -> flags, i
            go FormatFlags.None i

        let parseWidth (s: string) i = 
            if s.[i] = '*' then StarValue, (i + 1)
            elif Char.IsDigit s.[i] then intFromString s i
            else NotSpecifiedValue, i

        let parsePrecision (s: string) i = 
            if s.[i] = '.' then
                if s.[i + 1] = '*' then StarValue, i + 2
                elif Char.IsDigit s.[i + 1] then intFromString s (i + 1)
                else raise (ArgumentException("invalid precision value"))
            else NotSpecifiedValue, i
        
        let parseTypeChar (s: string) i = 
            s.[i], (i + 1)

        let parseInterpolatedHoleDotNetFormat typeChar (s: string) i =
            if typeChar = 'P' then 
                if i < s.Length && s.[i] = '(' then  
                     let i2 = s.IndexOf(")", i)
                     if i2 = -1 then 
                         None, i
                     else 
                         Some s.[i+1..i2-1], i2+1
                else
                    None, i
            else
                None, i

        // Skip %P() added for hole in "...%d{x}..."
        let skipInterpolationHole (fmt:string) i =
            if i+1 < fmt.Length && fmt.[i] = '%' && fmt.[i+1] = 'P'  then
                let i = i + 2
                if i+1 < fmt.Length && fmt.[i] = '('  && fmt.[i+1] = ')' then 
                    i+2
                else
                    i
            else i
    
        let findNextFormatSpecifier (s: string) i = 
            let rec go i (buf: Text.StringBuilder) =
                if i >= s.Length then 
                    s.Length, buf.ToString()
                else
                    let c = s.[i]
                    if c = '%' then
                        if i + 1 < s.Length then
                            let _, i1 = parseFlags s (i + 1)
                            let w, i2 = parseWidth s i1
                            let p, i3 = parsePrecision s i2
                            let typeChar, i4 = parseTypeChar s i3

                            // shortcut for the simpliest case
                            // if typeChar is not % or it has star as width\precision - resort to long path
                            if typeChar = '%' && not (w = StarValue || p = StarValue) then 
                                buf.Append('%') |> ignore
                                go i4 buf
                            else 
                                i, buf.ToString()
                        else
                            raise (ArgumentException("Missing format specifier"))
                    else 
                        buf.Append c |> ignore
                        go (i + 1) buf
            go i (Text.StringBuilder())

    [<NoComparison; NoEquality>]
    /// Represents one step in the execution of a format string
    type Step =
        | StepWithArg of prefix: string * conv1: (obj -> string) 
        | StepWithTypedArg of prefix: string * conv1: (obj -> Type -> string) 
        | StepString of prefix: string 
        | StepLittleT of prefix: string 
        | StepLittleA of prefix: string
        | StepStar1 of prefix: string * conv: (obj -> int -> string) 
        | StepPercentStar1 of prefix: string
        | StepStar2 of prefix: string * conv: (obj -> int -> int -> string)
        | StepPercentStar2 of prefix: string

        static member BlockCount(steps) =
            let mutable count = 0
            for step in steps do 
                match step with 
                | StepWithArg (prefix, _conv) ->
                    if not (String.IsNullOrEmpty prefix) then count <- count + 1
                    count <- count + 1
                | StepWithTypedArg (prefix, _conv) ->
                    if not (String.IsNullOrEmpty prefix) then count <- count + 1
                    count <- count + 1
                | StepString prefix ->
                    if not (String.IsNullOrEmpty prefix) then count <- count + 1
                | StepLittleT(prefix) -> 
                    if String.IsNullOrEmpty prefix then count <- count + 1
                    count <- count + 1
                | StepLittleA(prefix) -> 
                    if String.IsNullOrEmpty prefix then count <- count + 1
                    count <- count + 1
                | StepStar1(prefix, _conv) -> 
                    if String.IsNullOrEmpty prefix then count <- count + 1
                    count <- count + 1
                | StepPercentStar1(prefix) ->
                    if String.IsNullOrEmpty prefix then count <- count + 1
                    count <- count + 1
                | StepStar2(prefix, _conv) -> 
                    if String.IsNullOrEmpty prefix then count <- count + 1
                    count <- count + 1
                | StepPercentStar2(prefix) -> 
                    if String.IsNullOrEmpty prefix then count <- count + 1
                    count <- count + 1
            count
            

    /// Abstracts generated printer from the details of particular environment: how to write text, how to produce results etc...
    [<AbstractClass>]
    type PrintfEnv<'State, 'Residue, 'Result>(state: 'State) =
        member _.State = state

        abstract Finish: unit -> 'Result

        abstract Write: string -> unit
        
        /// Write the result of a '%t' format.  If this is a string it is written. If it is a 'unit' value
        /// the side effect has already happened
        abstract WriteT: 'Residue -> unit

        member env.WriteSkipEmpty(s: string) = 
            if not (String.IsNullOrEmpty s) then 
                env.Write s

        member env.RunSteps (args: obj[], argTys: Type[], steps: Step[]) =
            let mutable argIndex = 0
            let mutable tyIndex = 0

            for step in steps do 
                match step with 
                | StepWithArg (prefix, conv) ->
                    env.WriteSkipEmpty(prefix)
                    let arg = args.[argIndex]
                    argIndex <- argIndex + 1
                    env.Write(conv arg)

                | StepWithTypedArg (prefix, conv) ->
                    env.WriteSkipEmpty(prefix)
                    let arg = args.[argIndex]
                    let argTy = argTys.[tyIndex]
                    argIndex <- argIndex + 1
                    tyIndex <- tyIndex + 1
                    env.Write(conv arg argTy)

                | StepString prefix ->
                    env.WriteSkipEmpty(prefix)

                | StepLittleT(prefix) -> 
                    env.WriteSkipEmpty prefix
                    let farg = args.[argIndex]
                    argIndex <- argIndex + 1
                    let f = farg :?> ('State -> 'Residue)
                    env.WriteT(f env.State)

                | StepLittleA(prefix) -> 
                    env.WriteSkipEmpty prefix
                    let farg = args.[argIndex]
                    argIndex <- argIndex + 1
                    let arg = args.[argIndex]
                    argIndex <- argIndex + 1
                    let f = farg :?> ('State -> obj -> 'Residue)
                    env.WriteT(f env.State arg)

                | StepStar1(prefix, conv) -> 
                    env.WriteSkipEmpty prefix
                    let star1 = args.[argIndex] :?> int
                    argIndex <- argIndex + 1
                    let arg1 = args.[argIndex]
                    argIndex <- argIndex + 1
                    env.Write (conv arg1 star1)
       
                | StepPercentStar1(prefix) ->
                    //let _star1 = args.[argIndex] :?> int
                    argIndex <- argIndex + 1
                    env.WriteSkipEmpty prefix
                    env.Write("%")

                | StepStar2(prefix, conv) -> 
                    env.WriteSkipEmpty prefix
                    let star1 = args.[argIndex] :?> int
                    argIndex <- argIndex + 1
                    let star2 = args.[argIndex] :?> int
                    argIndex <- argIndex + 1
                    let arg1 = args.[argIndex]
                    argIndex <- argIndex + 1
                    env.Write (conv arg1 star1 star2)

                | StepPercentStar2(prefix) -> 
                    env.WriteSkipEmpty prefix
                    //let _star1 = args.[argIndex] :?> int
                    argIndex <- argIndex + 2
                    env.Write("%")
    
            env.Finish()

    /// Type of results produced by specialization.
    ///
    /// This is a function that accepts a thunk to create PrintfEnv on demand (at the very last
    /// appliction of an argument) and returns a concrete instance of an appriate curried printer.
    ///
    /// After all arguments are collected, specialization obtains concrete PrintfEnv from the thunk
    /// and uses it to output collected data.
    type PrintfFuncContext<'State, 'Residue, 'Result> = unit -> (obj list * PrintfEnv<'State, 'Residue, 'Result>)
    type PrintfFuncFactory<'Printer, 'State, 'Residue, 'Result> = PrintfFuncContext<'State, 'Residue, 'Result> -> 'Printer

    [<Literal>]
    let MaxArgumentsInSpecialization = 3

    type Specializations<'State, 'Residue, 'Result>() =
     
        static let finalizeSteps steps = steps |> List.rev |> List.toArray
        static let finalizeArgs args = args |> List.rev |> List.toArray
        
        static member Final0(steps) =
            let allSteps = finalizeSteps steps
            (fun (prev: PrintfFuncContext<'State, 'Residue, 'Result>) ->
                let (args, env) = prev()
                env.RunSteps(finalizeArgs args, null, allSteps)
            )

        static member CaptureFinal1<'A>(steps) =
            let allSteps = finalizeSteps steps
            (fun (prev: PrintfFuncContext<'State, 'Residue, 'Result>) ->
                (fun (arg1: 'A) ->
                    let (args, env) = prev()
                    let finalArgs = box arg1 :: args
                    env.RunSteps(finalizeArgs finalArgs, null, allSteps)
                )
            )

        static member CaptureFinal2<'A, 'B>(steps) =
            let allSteps = finalizeSteps steps
            (fun (prev: PrintfFuncContext<'State, 'Residue, 'Result>) ->
                (fun (arg1: 'A) (arg2: 'B) ->
                    let (args, env) = prev()
                    let finalArgs = box arg2 :: box arg1 :: args
                    env.RunSteps(finalizeArgs finalArgs, null, allSteps)
                )
            )

        static member CaptureFinal3<'A, 'B, 'C>(steps) =
            let allSteps = finalizeSteps steps
            (fun (prev: PrintfFuncContext<'State, 'Residue, 'Result>) ->
                (fun (arg1: 'A) (arg2: 'B) (arg3: 'C) ->
                    let (args, env) = prev()
                    let finalArgs = box arg3 :: box arg2 :: box arg1 :: args
                    env.RunSteps(finalizeArgs finalArgs, null, allSteps)
                )
            )

        static member Capture1<'A, 'Tail>(next) =
            (fun (prev: PrintfFuncContext<'State, 'Residue, 'Result>) ->
                (fun (arg1: 'A) ->
                    let curr() = 
                        let (args, env) = prev()
                        (box arg1 :: args), env
                    next curr : 'Tail
                )
            )

        static member CaptureLittleA<'A, 'Tail>(next) =
            (fun (prev: PrintfFuncContext<'State, 'Residue, 'Result>) ->
                (fun (f: 'State -> 'A -> 'Residue) (arg1: 'A) ->
                    let curr() = 
                        let (args, env) = prev()
                        (box arg1 :: box (fun s (arg:obj) -> f s (unbox arg)) :: args), env
                    next curr : 'Tail
                )
            )

        static member Capture2<'A, 'B, 'Tail>(next) =
            (fun (prev: PrintfFuncContext<'State, 'Residue, 'Result>) ->
                (fun (arg1: 'A) (arg2: 'B) ->
                    let curr() = 
                        let (args, env) = prev()
                        (box arg2 :: box arg1 :: args), env
                    next curr : 'Tail
                )
            )

        static member Capture3<'A, 'B, 'C, 'Tail> (next) =
            (fun (prev: PrintfFuncContext<'State, 'Residue, 'Result>) ->
                (fun (arg1: 'A) (arg2: 'B) (arg3: 'C) ->
                    let curr() = 
                        let (args, env) = prev()
                        (box arg3 :: box arg2 :: box arg1 :: args), env
                    next curr : 'Tail
                )
            )

    let inline (===) a b = Object.ReferenceEquals(a, b)

    let inline boolToString v = if v then "true" else "false"

    let inline stringToSafeString v = 
        match v with
        | null -> ""
        | _ -> v

    [<Literal>]
    let DefaultPrecision = 6

    /// A wrapper struct used to slightly strengthen the types of "ValueConverter" objects produced during composition of
    /// the dynamic implementation.  These are always functions but sometimes they take one argument, sometimes two.
    [<Struct; NoEquality; NoComparison>]
    type ValueConverter private (f: obj) =
        member x.FuncObj = f

        static member inline Make (f: obj -> string) = ValueConverter(box f)
        static member inline Make (f: obj -> int -> string) = ValueConverter(box f)
        static member inline Make (f: obj -> int-> int -> string) = ValueConverter(box f)

    let getFormatForFloat (ch: char) (prec: int) = ch.ToString() +  prec.ToString()

    let normalizePrecision prec = min (max prec 0) 99

    /// Contains helpers to convert printer functions to functions that prints value with respect to specified justification
    /// There are two kinds to printers: 
    /// 'T -> string - converts value to string - used for strings, basic integers etc..
    /// string -> 'T -> string - converts value to string with given format string - used by numbers with floating point, typically precision is set via format string 
    /// To support both categories there are two entry points:
    /// - withPadding - adapts first category
    /// - withPaddingFormatted - adapts second category
    module Padding = 

        /// pad here is function that converts T to string with respect of justification
        /// basic - function that converts T to string without applying justification rules
        /// adaptPaddedFormatted returns boxed function that has various number of arguments depending on if width\precision flags has '*' value 
        let adaptPaddedFormatted (spec: FormatSpecifier) getFormat (basic: string -> obj -> string) (pad: string -> int -> obj -> string) : ValueConverter =
            if spec.IsStarWidth then
                if spec.IsStarPrecision then
                    // width=*, prec=*
                    ValueConverter.Make (fun v width prec -> 
                        let fmt = getFormat (normalizePrecision prec)
                        pad fmt width v)
                else 
                    // width=*, prec=?
                    let prec = if spec.IsPrecisionSpecified then normalizePrecision spec.Precision else DefaultPrecision
                    let fmt = getFormat prec
                    ValueConverter.Make (fun v width -> 
                        pad fmt width v)

            elif spec.IsStarPrecision then
                if spec.IsWidthSpecified then
                    // width=val, prec=*
                    ValueConverter.Make (fun v prec -> 
                        let fmt = getFormat prec
                        pad fmt spec.Width v)
                else
                    // width=X, prec=*
                    ValueConverter.Make (fun v prec -> 
                        let fmt = getFormat prec
                        basic fmt v)                        
            else
                let prec = if spec.IsPrecisionSpecified then normalizePrecision spec.Precision else DefaultPrecision
                let fmt = getFormat prec
                if spec.IsWidthSpecified then
                    // width=val, prec=*
                    ValueConverter.Make (fun v -> 
                        pad fmt spec.Width v)
                else
                    // width=X, prec=*
                    ValueConverter.Make (fun v -> 
                        basic fmt v)

        /// pad here is function that converts T to string with respect of justification
        /// basic - function that converts T to string without applying justification rules
        /// adaptPadded returns boxed function that has various number of arguments depending on if width flags has '*' value 
        let adaptPadded (spec: FormatSpecifier) (basic: obj -> string) (pad: int -> obj -> string) : ValueConverter = 
            if spec.IsStarWidth then
                // width=*, prec=?
                ValueConverter.Make (fun v width -> 
                    pad width v)
            else
                if spec.IsWidthSpecified then
                    // width=val, prec=*
                    ValueConverter.Make (fun v -> 
                        pad spec.Width v)
                else
                    // width=X, prec=*
                    ValueConverter.Make (fun v -> 
                        basic v)

        let withPaddingFormatted (spec: FormatSpecifier) getFormat  (defaultFormat: string) (f: string ->  obj -> string) left right : ValueConverter =
            if not (spec.IsWidthSpecified || spec.IsPrecisionSpecified) then
                ValueConverter.Make (f defaultFormat)
            else
                if isLeftJustify spec.Flags then
                    adaptPaddedFormatted spec getFormat f left
                else
                    adaptPaddedFormatted spec getFormat f right

        let withPadding (spec: FormatSpecifier) (f: obj -> string) left right : ValueConverter =
            if not spec.IsWidthSpecified then
                ValueConverter.Make f
            else
                if isLeftJustify spec.Flags then
                    adaptPadded spec f left
                else
                    adaptPadded  spec f right

    /// contains functions to handle left\right justifications for non-numeric types (strings\bools)
    module Basic =
        let leftJustify (f: obj -> string) padChar = 
            fun (w: int) v -> 
                (f v).PadRight(w, padChar)
    
        let rightJustify (f: obj -> string) padChar = 
            fun (w: int) v -> 
                (f v).PadLeft(w, padChar)
    
        let withPadding (spec: FormatSpecifier) f =
            let padChar, _ = spec.GetPadAndPrefix false 
            Padding.withPadding spec f (leftJustify f padChar) (rightJustify f padChar)
    
    /// contains functions to handle left\right and no justification case for numbers
    module GenericNumber =

        let isPositive (n: obj) = 
            match n with 
            | :? int8 as n -> n >= 0y
            | :? uint8 -> true
            | :? int16 as n -> n >= 0s
            | :? uint16 -> true
            | :? int32 as n -> n >= 0
            | :? uint32 -> true
            | :? int64 as n -> n >= 0L
            | :? uint64 -> true
            | :? nativeint as n -> n >= 0n
            | :? unativeint -> true
            | :? single as n -> n >= 0.0f
            | :? double as n -> n >= 0.0
            | :? decimal as n -> n >= 0.0M
            | _ -> failwith "isPositive: unreachable"

        /// handles right justification when pad char = '0'
        /// this case can be tricky:
        /// - negative numbers, -7 should be printed as '-007', not '00-7'
        /// - positive numbers when prefix for positives is set: 7 should be '+007', not '00+7'
        let inline rightJustifyWithZeroAsPadChar (str: string) isNumber isPositive w (prefixForPositives: string) =
            System.Diagnostics.Debug.Assert(prefixForPositives.Length = 0 || prefixForPositives.Length = 1)
            if isNumber then
                if isPositive then
                    prefixForPositives + (if w = 0 then str else str.PadLeft(w - prefixForPositives.Length, '0')) // save space to 
                else
                    if str.[0] = '-' then
                        let str = str.Substring 1
                        "-" + (if w = 0 then str else str.PadLeft(w - 1, '0'))
                    else
                        str.PadLeft(w, '0')
            else
                str.PadLeft(w, ' ')
        
        /// handler right justification when pad char = ' '
        let rightJustifyWithSpaceAsPadChar (str: string) isNumber isPositive w (prefixForPositives: string) =
            System.Diagnostics.Debug.Assert(prefixForPositives.Length = 0 || prefixForPositives.Length = 1)
            (if isNumber && isPositive then prefixForPositives + str else str).PadLeft(w, ' ')
        
        /// handles left justification with formatting with 'G'\'g' - either for decimals or with 'g'\'G' is explicitly set 
        let leftJustifyWithGFormat (str: string) isNumber isInteger isPositive w (prefixForPositives: string) padChar  =
            if isNumber then
                let str = if isPositive then prefixForPositives + str else str
                // NOTE: difference - for 'g' format we use isInt check to detect situations when '5.0' is printed as '5'
                // in this case we need to override padding and always use ' ', otherwise we'll produce incorrect results
                if isInteger then
                    str.PadRight(w, ' ') // don't pad integer numbers with '0' when 'g' format is specified (may yield incorrect results)
                else
                    str.PadRight(w, padChar) // non-integer => string representation has point => can pad with any character
            else
                str.PadRight(w, ' ') // pad NaNs with ' '

        let leftJustifyWithNonGFormat (str: string) isNumber isPositive w (prefixForPositives: string) padChar  =
            if isNumber then
                let str = if isPositive then prefixForPositives + str else str
                str.PadRight(w, padChar)
            else
                str.PadRight(w, ' ') // pad NaNs with ' ' 
        
        /// processes given string based depending on values isNumber\isPositive
        let noJustificationCore (str: string) isNumber isPositive prefixForPositives = 
            if isNumber && isPositive then prefixForPositives + str
            else str
        
        /// noJustification handler for f: 'T -> string - basic integer types
        let noJustification (f: obj -> string) (prefix: string) isUnsigned =
            if isUnsigned then
                fun (v: obj) -> noJustificationCore (f v) true true prefix
            else 
                fun (v: obj) -> noJustificationCore (f v) true (isPositive v) prefix

    /// contains functions to handle left\right and no justification case for numbers
    module Integer =
    
        let eliminateNative (v: obj) = 
            match v with
            | :? nativeint as n ->
                if IntPtr.Size = 4 then box (n.ToInt32())
                else box (n.ToInt64())
            | :? unativeint as n ->
                if IntPtr.Size = 4 then box (uint32 (n.ToUInt32()))
                else box (uint64 (n.ToUInt64()))
            | _ -> v

        let rec toString (v: obj) =
            match v with
            | :? int32 as n -> n.ToString(CultureInfo.InvariantCulture)
            | :? int64 as n -> n.ToString(CultureInfo.InvariantCulture)
            | :? sbyte as n -> n.ToString(CultureInfo.InvariantCulture)
            | :? byte as n -> n.ToString(CultureInfo.InvariantCulture)
            | :? int16 as n -> n.ToString(CultureInfo.InvariantCulture)
            | :? uint16 as n -> n.ToString(CultureInfo.InvariantCulture)
            | :? uint32 as n -> n.ToString(CultureInfo.InvariantCulture)
            | :? uint64 as n -> n.ToString(CultureInfo.InvariantCulture)
            | :? nativeint | :? unativeint -> toString (eliminateNative v)
            | _ -> failwith "toString: unreachable"

        let rec toFormattedString fmt (v: obj) = 
            match v with
            | :? int32 as n -> n.ToString(fmt, CultureInfo.InvariantCulture)
            | :? int64 as n -> n.ToString(fmt, CultureInfo.InvariantCulture)
            | :? sbyte as n -> n.ToString(fmt, CultureInfo.InvariantCulture)
            | :? byte as n -> n.ToString(fmt, CultureInfo.InvariantCulture)
            | :? int16 as n -> n.ToString(fmt, CultureInfo.InvariantCulture)
            | :? uint16 as n -> n.ToString(fmt, CultureInfo.InvariantCulture)
            | :? uint32 as n -> n.ToString(fmt, CultureInfo.InvariantCulture)
            | :? uint64 as n -> n.ToString(fmt, CultureInfo.InvariantCulture)
            | :? nativeint | :? unativeint -> toFormattedString fmt (eliminateNative v)
            | _ -> failwith "toString: unreachable"

        let rec toUnsigned (v: obj) = 
            match v with
            | :? int32 as n -> box (uint32 n)
            | :? int64 as n -> box (uint64 n)
            | :? sbyte as n -> box (byte n)
            | :? int16 as n -> box (uint16 n)
            | :? nativeint | :? unativeint -> toUnsigned (eliminateNative v)
            | _ -> v

        /// Left justification handler for f: 'T -> string - basic integer types
        let leftJustify isGFormat (f: obj -> string) (prefix: string) padChar isUnsigned = 
            if isUnsigned then
                if isGFormat then
                    fun (w: int) (v: obj) ->
                        GenericNumber.leftJustifyWithGFormat (f v) true true true w prefix padChar
                else
                    fun (w: int) (v: obj) ->
                        GenericNumber.leftJustifyWithNonGFormat (f v) true true w prefix padChar
            else
                if isGFormat then
                    fun (w: int) (v: obj) ->
                        GenericNumber.leftJustifyWithGFormat (f v) true true (GenericNumber.isPositive v) w prefix padChar
                else
                    fun (w: int) (v: obj) ->
                        GenericNumber.leftJustifyWithNonGFormat (f v) true (GenericNumber.isPositive v) w prefix padChar
        
        /// Right justification handler for f: 'T -> string - basic integer types
        let rightJustify f (prefixForPositives: string) padChar isUnsigned =
            if isUnsigned then
                if padChar = '0' then
                    fun (w: int) (v: obj) ->
                        GenericNumber.rightJustifyWithZeroAsPadChar (f v) true true w prefixForPositives
                else
                    System.Diagnostics.Debug.Assert((padChar = ' '))
                    fun (w: int) (v: obj) ->
                        GenericNumber.rightJustifyWithSpaceAsPadChar (f v) true true w prefixForPositives
            else
                if padChar = '0' then
                    fun (w: int) (v: obj) ->
                        GenericNumber.rightJustifyWithZeroAsPadChar (f v) true (GenericNumber.isPositive v) w prefixForPositives

                else
                    System.Diagnostics.Debug.Assert((padChar = ' '))
                    fun (w: int) v ->
                        GenericNumber.rightJustifyWithSpaceAsPadChar (f v) true (GenericNumber.isPositive v) w prefixForPositives

        let withPadding (spec: FormatSpecifier) isUnsigned (f: obj -> string)  =
            let allowZeroPadding = not (isLeftJustify spec.Flags) || spec.IsDecimalFormat
            let padChar, prefix = spec.GetPadAndPrefix allowZeroPadding
            Padding.withPadding spec
                (GenericNumber.noJustification f prefix isUnsigned)
                (leftJustify spec.IsGFormat f prefix padChar isUnsigned)
                (rightJustify f prefix padChar isUnsigned)

        let getValueConverter (spec: FormatSpecifier) : ValueConverter =
            let c = spec.TypeChar
            if c = 'd' || c = 'i' then
                withPadding spec false toString
            elif c = 'u' then
                withPadding spec true  (toUnsigned >> toString) 
            elif c = 'x' then
                withPadding spec true (toFormattedString "x")
            elif c = 'X' then
                withPadding spec true (toFormattedString "X")
            elif c = 'o' then
                withPadding spec true (fun (v: obj) ->
                    match toUnsigned v with 
                    | :? uint64 as u -> Convert.ToString(int64 u, 8)
                    | u -> Convert.ToString(Convert.ToInt64 u, 8))
            else raise (ArgumentException())    
    
    module FloatAndDecimal = 

        let rec toFormattedString fmt (v: obj) = 
            match v with
            | :? single as n -> n.ToString(fmt, CultureInfo.InvariantCulture)
            | :? double as n -> n.ToString(fmt, CultureInfo.InvariantCulture)
            | :? decimal as n -> n.ToString(fmt, CultureInfo.InvariantCulture)
            | _ -> failwith "toFormattedString: unreachable"

        let isNumber (x: obj) =
            match x with
            | :? single as x -> 
                not (Single.IsPositiveInfinity(x)) &&
                not (Single.IsNegativeInfinity(x)) &&
                not (Single.IsNaN(x))
            | :? double as x -> 
                not (Double.IsPositiveInfinity(x)) &&
                not (Double.IsNegativeInfinity(x)) &&
                not (Double.IsNaN(x))
            | :? decimal -> true
            | _ -> failwith "isNumber: unreachable"

        let isInteger (n: obj) = 
            match n with 
            | :? single as n -> n % 1.0f = 0.0f
            | :? double as n -> n % 1. = 0.
            | :? decimal as n -> n % 1.0M = 0.0M
            | _ -> failwith "isInteger: unreachable"

        let noJustification (prefixForPositives: string) = 
            fun (fmt: string) (v: obj) -> 
                GenericNumber.noJustificationCore (toFormattedString fmt v) (isNumber v) (GenericNumber.isPositive v) prefixForPositives
    
        let leftJustify isGFormat (prefix: string) padChar = 
            if isGFormat then
                fun (fmt: string) (w: int) (v: obj) ->
                    GenericNumber.leftJustifyWithGFormat (toFormattedString fmt v) (isNumber v) (isInteger v) (GenericNumber.isPositive v) w prefix padChar
            else
                fun (fmt: string) (w: int) (v: obj) ->
                    GenericNumber.leftJustifyWithNonGFormat (toFormattedString fmt v) (isNumber v) (GenericNumber.isPositive v) w prefix padChar  

        let rightJustify (prefixForPositives: string) padChar =
            if padChar = '0' then
                fun (fmt: string) (w: int) (v: obj) ->
                    GenericNumber.rightJustifyWithZeroAsPadChar (toFormattedString fmt v) (isNumber v) (GenericNumber.isPositive v) w prefixForPositives
            else
                System.Diagnostics.Debug.Assert((padChar = ' '))
                fun (fmt: string) (w: int) (v: obj) ->
                    GenericNumber.rightJustifyWithSpaceAsPadChar (toFormattedString fmt v) (isNumber v) (GenericNumber.isPositive v) w prefixForPositives

        let withPadding (spec: FormatSpecifier) getFormat defaultFormat =
            let padChar, prefix = spec.GetPadAndPrefix true 
            Padding.withPaddingFormatted spec getFormat defaultFormat
                (noJustification prefix)
                (leftJustify spec.IsGFormat prefix padChar)
                (rightJustify prefix padChar)

    type ObjectPrinter = 

        static member ObjectToString(spec: FormatSpecifier) : ValueConverter = 
            Basic.withPadding spec (fun (v: obj) ->
                match v with
                | null -> "<null>"
                | x -> x.ToString())
        
        /// Convert an interpoland to a string
        static member InterpolandToString(spec: FormatSpecifier) : ValueConverter = 
            let fmt = 
                match spec.InteropHoleDotNetFormat with 
                | None -> null
                | Some fmt -> "{0:" + fmt + "}"
            Basic.withPadding spec (fun (vobj: obj) ->
                match vobj with
                | null -> ""
                | x -> 
                    match fmt with 
                    | null -> x.ToString()
                    | fmt -> String.Format(fmt, x))
        
        static member GenericToStringCore(v: 'T, opts: Microsoft.FSharp.Text.StructuredPrintfImpl.FormatOptions, bindingFlags) = 
            let vty = 
                match box v with
                | null -> typeof<'T>
                | _ -> v.GetType()
            Microsoft.FSharp.Text.StructuredPrintfImpl.Display.anyToStringForPrintf opts bindingFlags (v, vty)

        static member GenericToString<'T>(spec: FormatSpecifier) : ValueConverter = 
            let bindingFlags = 
                if isPlusForPositives spec.Flags then BindingFlags.Public ||| BindingFlags.NonPublic
                else BindingFlags.Public 

            let useZeroWidth = isPadWithZeros spec.Flags
            let opts = 
                let o = Microsoft.FSharp.Text.StructuredPrintfImpl.FormatOptions.Default
                let o =
                    if useZeroWidth then { o with PrintWidth = 0} 
                    elif spec.IsWidthSpecified then { o with PrintWidth = spec.Width}
                    else o
                if spec.IsPrecisionSpecified then { o with PrintSize = spec.Precision}
                else o

            match spec.IsStarWidth, spec.IsStarPrecision with
            | true, true ->
                ValueConverter.Make (fun (vobj: obj) (width: int) (prec: int) ->
                    let v = unbox<'T> vobj
                    let opts = { opts with PrintSize = prec }
                    let opts  = if not useZeroWidth then { opts with PrintWidth = width} else opts
                    ObjectPrinter.GenericToStringCore(v, opts, bindingFlags)
                    )

            | true, false ->
                ValueConverter.Make (fun (vobj: obj) (width: int) ->
                    let v = unbox<'T> vobj
                    let opts  = if not useZeroWidth then { opts with PrintWidth = width} else opts
                    ObjectPrinter.GenericToStringCore(v, opts, bindingFlags))

            | false, true ->
                ValueConverter.Make (fun (vobj: obj) (prec: int) ->
                    let v = unbox<'T> vobj
                    let opts = { opts with PrintSize = prec }
                    ObjectPrinter.GenericToStringCore(v, opts, bindingFlags) )

            | false, false ->
                ValueConverter.Make (fun (vobj: obj) ->
                    let v = unbox<'T> vobj
                    ObjectPrinter.GenericToStringCore(v, opts, bindingFlags))
        
    let basicFloatToString spec = 
        let defaultFormat = getFormatForFloat spec.TypeChar DefaultPrecision
        FloatAndDecimal.withPadding spec (getFormatForFloat spec.TypeChar) defaultFormat

    let private NonPublicStatics = BindingFlags.NonPublic ||| BindingFlags.Static

    let mi_GenericToString = typeof<ObjectPrinter>.GetMethod("GenericToString", NonPublicStatics)

    let private getValueConverter (ty: Type) (spec: FormatSpecifier) : ValueConverter = 
        match spec.TypeChar with
        | 'b' ->  
            Basic.withPadding spec (unbox >> boolToString)
        | 's' ->
            Basic.withPadding spec (unbox >> stringToSafeString)
        | 'c' ->
            Basic.withPadding spec (fun (c: obj) -> (unbox<char> c).ToString())
        | 'M'  ->
            FloatAndDecimal.withPadding spec (fun _ -> "G") "G" // %M ignores precision
        | 'd' | 'i' | 'x' | 'X' | 'u' | 'o'-> 
            Integer.getValueConverter spec
        | 'e' | 'E' 
        | 'f' | 'F' 
        | 'g' | 'G' -> 
            basicFloatToString spec
        | 'A' ->
            let mi = mi_GenericToString.MakeGenericMethod ty
            mi.Invoke(null, [| box spec |]) |> unbox
        | 'O' -> 
            ObjectPrinter.ObjectToString(spec) 
        | 'P' -> 
            ObjectPrinter.InterpolandToString(spec) 
        | _ -> 
            raise (ArgumentException(SR.GetString(SR.printfBadFormatSpecifier)))
    
    let extractCurriedArguments (ty: Type) n = 
        System.Diagnostics.Debug.Assert(n = 1 || n = 2 || n = 3, "n = 1 || n = 2 || n = 3")
        let buf = Array.zeroCreate n
        let rec go (ty: Type) i = 
            if i < n then
                match ty.GetGenericArguments() with
                | [| argTy; retTy|] ->
                    buf.[i] <- argTy
                    go retTy (i + 1)
                | _ -> failwith (String.Format("Expected function with {0} arguments", n))
            else 
                System.Diagnostics.Debug.Assert((i = n), "i = n")
                buf, ty
        go ty 0    

    let MAX_CAPTURE = 3

    /// Parses format string and creates resulting step list and printer factory function.
    type FormatParser<'Printer, 'State, 'Residue, 'Result>(fmt: string) =
    
        let buildCaptureFunc (spec: FormatSpecifier, steps, argTys: Type[], retTy, nextInfo) = 
            let (next:obj, nextCanCombine: bool, nextArgTys: Type[], nextRetTy, nextNextOpt) = nextInfo
            assert (argTys.Length > 0)

            // See if we can compress a capture to a multi-capture
            //     CaptureN + Final --> CaptureFinalN
            //     Capture1 + Capture1 --> Capture2
            //     Capture1 + Capture2 --> Capture3
            //     Capture2 + Capture1 --> Capture3
            match argTys.Length, nextArgTys.Length with 
            |  _ when spec.TypeChar = 'a' ->
                // %a has an existential type which must be converted to obj
                assert (argTys.Length = 2)
                let captureMethName = "CaptureLittleA" 
                let mi = typeof<Specializations<'State, 'Residue, 'Result>>.GetMethod(captureMethName, NonPublicStatics)
                let mi = mi.MakeGenericMethod([| argTys.[1]; retTy |])
                let funcObj = mi.Invoke(null, [| next  |])
                funcObj, false, argTys, retTy, Some next

            | n1, n2 when nextCanCombine && n1 + n2 <= MAX_CAPTURE ->
                // 'next' is thrown away on this path and replaced by a combined Capture
                let captureCount = n1 + n2
                let combinedArgTys = Array.append argTys nextArgTys
                match nextNextOpt with 
                | None ->
                    let captureMethName = "CaptureFinal" + string captureCount
                    let mi = typeof<Specializations<'State, 'Residue, 'Result>>.GetMethod(captureMethName, NonPublicStatics)
                    let mi = mi.MakeGenericMethod(combinedArgTys)
                    let funcObj = mi.Invoke(null, [| steps |])
                    funcObj, true, combinedArgTys, nextRetTy, None
                | Some nextNext ->
                    let captureMethName = "Capture" + string captureCount
                    let mi = typeof<Specializations<'State, 'Residue, 'Result>>.GetMethod(captureMethName, NonPublicStatics)
                    let mi = mi.MakeGenericMethod(Array.append combinedArgTys [| nextRetTy |])
                    let funcObj = mi.Invoke(null, [| nextNext |])
                    funcObj, true, combinedArgTys, nextRetTy, nextNextOpt

            | captureCount, _ ->
                let captureMethName = "Capture" + string captureCount
                let mi = typeof<Specializations<'State, 'Residue, 'Result>>.GetMethod(captureMethName, NonPublicStatics)
                let mi = mi.MakeGenericMethod(Array.append argTys [| retTy |])
                let funcObj = mi.Invoke(null, [| next  |])
                funcObj, true, argTys, retTy, Some next

        let buildStep (spec: FormatSpecifier) (argTys: Type[]) prefix = 
            if spec.TypeChar = 'a' then
                StepLittleA prefix
            elif spec.TypeChar = 't' then
                StepLittleT prefix
            elif spec.IsStarPrecision || spec.IsStarWidth then
                let isTwoStar = (spec.IsStarWidth = spec.IsStarPrecision)
                match isTwoStar, spec.TypeChar with 
                | false, '%' -> StepPercentStar1 prefix
                | true, '%' -> StepPercentStar2 prefix
                | _ ->
                    // For curried interpolated string format processing, the static types of the '%A' arguments 
                    // are provided via the argument typed extracted from the curried function. They are known on first phase.
                    let argTy = match argTys with null -> typeof<obj> | _ -> argTys.[argTys.Length - 1]
                    let conv = getValueConverter argTy spec 
                    if isTwoStar then 
                        let convFunc = conv.FuncObj :?> (obj -> int -> int -> string)
                        StepStar2 (prefix, convFunc)
                    else
                        let convFunc = conv.FuncObj :?> (obj -> int -> string)
                        StepStar1 (prefix, convFunc)
            else
                // For interpolated string format processing, the static types of the '%A' arguments 
                // are provided via CaptureTypes and are only known on second phase.
                match argTys with
                | null when spec.TypeChar = 'A' ->
                    let convFunc arg argTy = 
                        let mi = mi_GenericToString.MakeGenericMethod [| argTy |]
                        let f = mi.Invoke(null, [| box spec |]) :?> ValueConverter
                        let f2 = f.FuncObj :?> (obj -> string)
                        f2 arg

                    StepWithTypedArg (prefix, convFunc)

                | _ -> 
                    // For curried interpolated string format processing, the static types of the '%A' arguments 
                    // are provided via the argument typed extracted from the curried function. They are known on first phase.
                    let argTy = match argTys with null -> typeof<obj> | _ -> argTys.[0]
                    let conv = getValueConverter argTy spec
                    let convFunc = conv.FuncObj :?> (obj -> string)
                    StepWithArg (prefix, convFunc)
            
        let parseSpec i = 
            let flags, i = FormatString.parseFlags fmt (i + 1)
            let width, i = FormatString.parseWidth fmt i
            let precision, i = FormatString.parsePrecision fmt i
            let typeChar, i = FormatString.parseTypeChar fmt i
            let interpHoleDotnetFormat, i = FormatString.parseInterpolatedHoleDotNetFormat typeChar fmt i

            // Skip %P insertion points added after %d{...} etc. in interpolated strings
            let i = FormatString.skipInterpolationHole fmt i

            let spec = 
                { TypeChar = typeChar
                  Precision = precision
                  Flags = flags
                  Width = width
                  InteropHoleDotNetFormat = interpHoleDotnetFormat }
            i, spec
            
        // A simplified parser. For the case where the string is being used with interpolands captured in the Format object. 
        let rec parseStepsAux steps (prefix: string) i = 
            if i >= fmt.Length then 
                let step = StepString(prefix)
                (step :: steps)
            else
                let i, spec = parseSpec i
                let next, suffix = FormatString.findNextFormatSpecifier fmt i
                let step = buildStep spec null prefix
                parseStepsAux (step::steps) suffix next

        let parseSteps () =
            let i, prefix = FormatString.findNextFormatSpecifier fmt 0
            let steps = parseStepsAux [] prefix i
            let count = Step.BlockCount steps
            count, steps

        /// The more advanced parser which both builds the steps (with accurate %A types),
        /// and produces a curried function value of the right type guided by funcTy
        let rec parseAndCreateFuncFactoryAux steps (prefix: string) (funcTy: Type) i = 
            
            if i >= fmt.Length then 
                let step = StepString(prefix)
                let last = Specializations<'State, 'Residue, 'Result>.Final0(steps)
                (step :: steps), (box last, true, [| |], funcTy, None)
            else
                System.Diagnostics.Debug.Assert(fmt.[i] = '%', "s.[i] = '%'")
                let i, spec = parseSpec i
                let next, suffix = FormatString.findNextFormatSpecifier fmt i
                let argTys, retTy =  extractCurriedArguments funcTy spec.ArgCount
                let step = buildStep spec argTys prefix
                let allSteps, next = parseAndCreateFuncFactoryAux (step::steps) suffix retTy next
                let nextNew = buildCaptureFunc (spec, allSteps, argTys, retTy, next)
                allSteps, nextNew

        let parseAndCreateFuncFactory () =
            let funcTy = typeof<'Printer>

            // Find the first format specifier
            let prefixPos, prefix = FormatString.findNextFormatSpecifier fmt 0
            
            // If there are not format specifiers then take a simple path
            if prefixPos = fmt.Length then 
                0, box (fun (initial: PrintfFuncContext<'State, 'Residue, 'Result>) -> 
                    let (_args, env) = initial()
                    env.WriteSkipEmpty prefix
                    env.Finish())
            else
                let steps, (factoryObj, _, _, _, _) = parseAndCreateFuncFactoryAux [] prefix funcTy prefixPos
                let count = Step.BlockCount steps
                count, factoryObj 

        // The simple steps, populated on-demand, for the case where the string is being used with interpolands captured in the Format object. 
        let mutable allSteps = Unchecked.defaultof<_>

        // The function factory, populated on-demand 
        let mutable functionFactory = Unchecked.defaultof<_>

        // The function factory, populated on-demand 
        let mutable stringCount = 0

        /// The format string, used to help identify the cache entry (the cache index types are taken
        /// into account as well).
        member _.FormatString = fmt

        /// The steps involved in executing the format string when interpolands are captured
        member _.Steps =
            match allSteps with
            | null -> 
                // We may initialize this twice, but the assignment is atomic and the computation will give functionally
                // identical results each time it is ok
                let count, steps = parseSteps () 
                stringCount <- count
                allSteps <- steps |> List.rev |> List.toArray
            | _ -> ()
            allSteps

        /// The number of strings produced for a sprintf
        member _.BlockCount = stringCount
            
        /// The factory function used to generate the result or the resulting function.  
        member _.FunctionFactory =
            match box functionFactory with
            | null -> 
                let count, funcObj = parseAndCreateFuncFactory () 
                // We may initialize this twice, but the assignment is atomic and the computation will give functionally
                // identical results each time it is ok
                functionFactory <- (funcObj :?> PrintfFuncFactory<'Printer, 'State, 'Residue, 'Result>)
                stringCount <- (2 * count + 1)  
            | _ -> ()
            functionFactory

    /// 2-level cache, keyed by format string and index types
    type Cache<'Printer, 'State, 'Residue, 'Result>() =

        /// 1st level cache (type-indexed). Stores last value that was consumed by the current thread in
        /// thread-static field thus providing shortcuts for scenarios when printf is called in tight loop.
        [<DefaultValue; ThreadStatic>]
        static val mutable private mostRecent: FormatParser<'Printer, 'State, 'Residue, 'Result>
    
        // 2nd level cache (type-indexed). Dictionary that maps format string to the corresponding cache entry
        static let mutable dict : ConcurrentDictionary<string, FormatParser<'Printer, 'State, 'Residue, 'Result>> = null

        static member GetParser(format: Format<'Printer, 'State, 'Residue, 'Result>) =
            let cacheEntry = Cache<'Printer, 'State, 'Residue, 'Result>.mostRecent
            let fmt = format.Value
            if cacheEntry === null then 
                let parser = FormatParser(fmt)
                Cache.mostRecent <- parser
                parser
            elif fmt.Equals cacheEntry.FormatString then 
                cacheEntry
            else
                // Initialize the 2nd level cache if necessary.  Note there's a race condition but it doesn't
                // matter if we initialize these values twice (and lose one entry)
                if isNull dict then 
                    dict <- ConcurrentDictionary<_,_>()

                let parser = 
                    match dict.TryGetValue(fmt) with 
                    | true, res -> res
                    | _ -> 
                        let parser = FormatParser(fmt)
                        // There's a race condition - but the computation is functional and it doesn't matter if we do it twice
                        dict.TryAdd(fmt, parser) |> ignore
                        parser
                Cache.mostRecent <- parser
                parser

    type LargeStringPrintfEnv<'Result>(continuation, blockSize) = 
        inherit PrintfEnv<unit, string, 'Result>(())
        let buf: string[] = Array.zeroCreate blockSize
        let mutable ptr = 0

        override __.Finish() : 'Result = continuation (String.Concat buf)

        override __.Write(s: string) = 
            buf.[ptr] <- s
            ptr <- ptr + 1

        override __.WriteT s =
            buf.[ptr] <- s
            ptr <- ptr + 1

    type SmallStringPrintfEnv() = 
        inherit PrintfEnv<unit, string, string>(())
        let mutable c = null

        override __.Finish() : string = if isNull c then "" else c
        override __.Write(s: string) = if isNull c then c <- s else c <- c + s
        override __.WriteT s = if isNull c then c <- s else c <- c + s

    let StringPrintfEnv blockSize = 
        if blockSize <= 2 then
            SmallStringPrintfEnv() :> PrintfEnv<_,_,_>
        else
            LargeStringPrintfEnv(id, blockSize) :> PrintfEnv<_,_,_>

    let StringBuilderPrintfEnv<'Result>(k, buf) = 
        { new PrintfEnv<Text.StringBuilder, unit, 'Result>(buf) with
            override __.Finish() : 'Result = k ()
            override __.Write(s: string) = ignore(buf.Append s)
            override __.WriteT(()) = () }

    let TextWriterPrintfEnv<'Result>(k, tw: IO.TextWriter) =
        { new PrintfEnv<IO.TextWriter, unit, 'Result>(tw) with 
            override __.Finish() : 'Result = k()
            override __.Write(s: string) = tw.Write s
            override __.WriteT(()) = () }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Printf =

    open System
    open System.IO
    open System.Text
    open PrintfImpl

    type BuilderFormat<'T, 'Result> = Format<'T, StringBuilder, unit, 'Result>
    type StringFormat<'T, 'Result> = Format<'T, unit, string, 'Result>
    type TextWriterFormat<'T, 'Result> = Format<'T, TextWriter, unit, 'Result>
    type BuilderFormat<'T> = BuilderFormat<'T,unit>
    type StringFormat<'T> = StringFormat<'T,string>
    type TextWriterFormat<'T>  = TextWriterFormat<'T,unit>

    let gprintf envf (format: Format<'Printer, 'State, 'Residue, 'Result>) = 
        let cacheItem = Cache.GetParser format
        match format.Captures with 
        | null -> 
            // The ksprintf "...%d ...." arg path, producing a function
            let factory = cacheItem.FunctionFactory
            let initial() = ([], envf cacheItem.BlockCount :> PrintfEnv<_,_,_>)
            factory initial
        | captures -> 
            // The ksprintf $"...%d{3}...." path, running the steps straight away to produce a string
            let steps = cacheItem.Steps
            let env = envf cacheItem.BlockCount :> PrintfEnv<_,_,_>
            let res = env.RunSteps(captures, format.CaptureTypes, steps)
            unbox res // prove 'T = 'Result
            //continuation res
    
    [<CompiledName("PrintFormatToStringThen")>]
    let ksprintf continuation (format: StringFormat<'T, 'Result>) : 'T = 
        gprintf (fun stringCount -> LargeStringPrintfEnv(continuation, stringCount)) format

    [<CompiledName("PrintFormatToStringThen")>]
    let sprintf (format: StringFormat<'T>) =
        // We inline gprintf by hand here to be sure to remove a few allocations
        let cacheItem = Cache.GetParser format
        match format.Captures with 
        | null ->
            // The sprintf "...%d ...." arg path, producing a function
            let factory = cacheItem.FunctionFactory
            let initial() = ([], StringPrintfEnv cacheItem.BlockCount)
            factory initial
        | captures -> 
            // The sprintf $"...%d{3}...." path, running the steps straight away to produce a string
            let steps = cacheItem.Steps
            let env = StringPrintfEnv cacheItem.BlockCount
            let res = env.RunSteps(captures, format.CaptureTypes, steps)
            unbox res // proves 'T = string

    [<CompiledName("PrintFormatThen")>]
    let kprintf continuation format = ksprintf continuation format

    [<CompiledName("PrintFormatToStringBuilderThen")>]
    let kbprintf continuation (builder: StringBuilder) (format: BuilderFormat<'T, 'Result>) : 'T = 
        gprintf (fun _stringCount -> StringBuilderPrintfEnv(continuation, builder)) format
    
    [<CompiledName("PrintFormatToTextWriterThen")>]
    let kfprintf continuation textWriter (format: TextWriterFormat<'T, 'Result>) =
        gprintf (fun _stringCount -> TextWriterPrintfEnv(continuation, textWriter)) format

    [<CompiledName("PrintFormatToStringBuilder")>]
    let bprintf builder format =
        kbprintf ignore builder format 

    [<CompiledName("PrintFormatToTextWriter")>]
    let fprintf (textWriter: TextWriter) format =
        kfprintf ignore textWriter format 

    [<CompiledName("PrintFormatLineToTextWriter")>]
    let fprintfn (textWriter: TextWriter) format =
        kfprintf (fun _ -> textWriter.WriteLine()) textWriter format

    [<CompiledName("PrintFormatToStringThenFail")>]
    let failwithf format =
        ksprintf failwith format

    [<CompiledName("PrintFormat")>]
    let printf format =
        fprintf Console.Out format

    [<CompiledName("PrintFormatToError")>]
    let eprintf format =
        fprintf Console.Error format

    [<CompiledName("PrintFormatLine")>]
    let printfn format =
        fprintfn Console.Out format

    [<CompiledName("PrintFormatLineToError")>]
    let eprintfn format =
        fprintfn Console.Error format
