// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace FSharp.Compiler 
    open Microsoft.FSharp.Core
    open Microsoft.FSharp.Collections
    open Microsoft.FSharp.Reflection

    module internal SR =
        let private resources = lazy (new System.Resources.ResourceManager("fsstrings", System.Reflection.Assembly.GetExecutingAssembly()))

        let GetString(name:string) =
            let s = resources.Force().GetString(name, System.Globalization.CultureInfo.CurrentUICulture)
#if DEBUG
            if null = s then
                System.Diagnostics.Debug.Assert(false, sprintf "**RESOURCE ERROR**: Resource token %s does not exist!" name)
#endif
            s

    module internal DiagnosticMessage =

        open Microsoft.FSharp.Core.LanguagePrimitives.IntrinsicOperators

        let mkFunctionValue (tys: System.Type[]) (impl:obj->obj) = 
            FSharpValue.MakeFunction(FSharpType.MakeFunctionType(tys.[0],tys.[1]), impl)

        let funTyC = typeof<obj -> obj>.GetGenericTypeDefinition()  
        let mkFunTy a b = funTyC.MakeGenericType([| a;b |])

        let isNamedType(ty:System.Type) = not (ty.IsArray ||  ty.IsByRef ||  ty.IsPointer)
        let isFunctionType (ty1:System.Type)  = 
            isNamedType(ty1) && ty1.IsGenericType && (ty1.GetGenericTypeDefinition()).Equals(funTyC)

        let rec destFunTy (ty:System.Type) =
            if isFunctionType ty then 
                ty, ty.GetGenericArguments() 
            else
                match ty.BaseType with 
                | null -> failwith "destFunTy: not a function type" 
                | b -> destFunTy b 

        let buildFunctionForOneArgPat (ty: System.Type) impl = 
            let _,tys = destFunTy ty 
            let rty = tys.[1]
            // PERF: this technique is a bit slow (e.g. in simple cases, like 'sprintf "%x"') 
            mkFunctionValue tys (fun inp -> impl rty inp)
                    
        let capture1 (fmt:string) i args ty (go : obj list -> System.Type -> int -> obj) : obj = 
            match fmt.[i] with
            | '%' -> go args ty (i+1) 
            | 'd'
            | 'f'
            | 's' -> buildFunctionForOneArgPat ty (fun rty n -> go (n :: args) rty (i+1))
            | _ -> failwith "bad format specifier"

        // newlines and tabs get converted to strings when read from a resource file
        // this will preserve their original intention    
        let postProcessString (s : string) =
            s.Replace("\\n","\n").Replace("\\t","\t")

        let createMessageString (messageString : string) (fmt : Printf.StringFormat<'T>) : 'T = 
            let fmt = fmt.Value // here, we use the actual error string, as opposed to the one stored as fmt
            let len = fmt.Length 

            /// Function to capture the arguments and then run.
            let rec capture args ty i = 
                if i >= len ||  (fmt.[i] = '%' && i+1 >= len) then 
                    let b = new System.Text.StringBuilder()    
                    b.AppendFormat(messageString, (Array.ofList (List.rev args))) |> ignore
                    box(b.ToString())
                // REVIEW: For these purposes, this should be a nop, but I'm leaving it
                // in case we ever decide to support labels for the error format string
                // E.g., "<name>%s<foo>%d"
                elif System.Char.IsSurrogatePair(fmt,i) then 
                   capture args ty (i+2)
                else
                    match fmt.[i] with
                    | '%' ->
                        let i = i+1 
                        capture1 fmt i args ty capture
                    | _ ->
                        capture args ty (i+1) 

            (unbox (capture [] typeof<'T> 0) : 'T)

        type ResourceString<'T>(fmtString : string, fmt : Printf.StringFormat<'T>) =
            member a.Format =
                createMessageString fmtString fmt

        let DeclareResourceString (messageID : string,fmt : Printf.StringFormat<'T>) =
            let mutable messageString = SR.GetString(messageID)
#if DEBUG
            // validate that the message string exists
            let fmtString = fmt.Value
            
            if null = messageString then
                System.Diagnostics.Debug.Assert(false, sprintf "**DECLARED MESSAGE ERROR** String resource %s does not exist" messageID)
                messageString <- ""
            
            // validate the formatting specifiers
            let countFormatHoles (s : string) =
                // remove escaped format holes
                let s = s.Replace("{{","").Replace("}}","")
                let len = s.Length - 2
                let mutable pos = 0
                let mutable nHoles = 0
                let mutable order = Set.empty<int>
    
                while pos < len do
                    if s.[pos] = '{' then
                        let mutable pos' = pos+1
                        while System.Char.IsNumber(s.[pos']) do
                            pos' <- pos' + 1
                        if pos' > pos+1 && s.[pos'] = '}' then
                            nHoles <- nHoles + 1
                            let ordern = int s.[(pos+1) .. (pos'-1)]
                            order <- order.Add(ordern)
                            pos <- pos'
                    pos <- pos + 1
                // the sort should be unnecessary, but better safe than sorry
                nHoles,Set.toList order |> List.sortDescending

            let countFormatPlaceholders (s : string) =
                // strip any escaped % characters - yes, this will fail if given %%%...
                let s = s.Replace("%%","")
                
                if s = "" then 
                    0
                else
                    let len = s.Length - 1
                    let mutable pos = 0
                    let mutable nFmt = 0
                
                    while pos < len do
                        if s.[pos] = '%' && 
                          (s.[pos+1] = 'd' || s.[pos+1] = 's' || s.[pos+1] = 'f') then
                            nFmt <- nFmt + 1
                            pos <- pos + 2 ;
                        else
                            pos <- pos + 1 ;
                    nFmt
                    
            let nHoles,holes = countFormatHoles messageString
            let nPlaceholders = countFormatPlaceholders fmtString
            
            // first, verify that the number of holes in the message string does not exceed the 
            // largest hole reference
            if holes <> [] && holes.[0] > nHoles - 1 then
                System.Diagnostics.Debug.Assert(false, sprintf "**DECLARED MESSAGE ERROR** Message string %s contains %d holes, but references hole %d" messageID nHoles holes.[0])
                
            // next, verify that the number of format placeholders is the same as the number of holes
            if nHoles <> nPlaceholders then
                System.Diagnostics.Debug.Assert(false, sprintf "**DECLARED MESSAGE ERROR** Message string %s contains %d holes, but its format specifier contains %d placeholders" messageID nHoles nPlaceholders)
                
 #endif            
            messageString <- postProcessString messageString                            
            new ResourceString<'T>(messageString, fmt)
