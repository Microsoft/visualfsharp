// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace FSharp.Compiler.EditorServices

open System.Collections.Generic
open System.Runtime.CompilerServices
open Internal.Utilities.Library
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols.FSharpSymbolPatterns
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax.PrettyNaming
open FSharp.Compiler.Text
open FSharp.Compiler.Text.Range

module UnusedOpens =

    let symbolHash = HashIdentity.FromFunctions (fun (x: FSharpSymbol) -> x.GetEffectivelySameAsHash()) (fun x y -> x.IsEffectivelySameAs(y))

    /// Represents one namespace or module opened by an 'open' statement
    type OpenedModule(entity: FSharpEntity, isNestedAutoOpen: bool) = 

        /// Compute an indexed table of the set of symbols revealed by 'open', on-demand
        let revealedSymbols : Lazy<HashSet<FSharpSymbol>> =
           lazy
            let symbols = 
               [| for ent in entity.NestedEntities do
                      yield ent :> FSharpSymbol
                      
                      if ent.IsFSharpRecord then
                          for rf in ent.FSharpFields do
                              yield rf  :> FSharpSymbol
                      
                      if ent.IsFSharpUnion && not (ent.HasAttribute<RequireQualifiedAccessAttribute>()) then
                          for unionCase in ent.UnionCases do
                              yield unionCase :> FSharpSymbol

                      if ent.HasAttribute<ExtensionAttribute>() then
                          for fv in ent.MembersFunctionsAndValues do
                              // fv.IsExtensionMember is always false for C# extension methods returning by `MembersFunctionsAndValues`,
                              // so we have to check Extension attribute instead. 
                              // (note: fv.IsExtensionMember has proper value for symbols returning by GetAllUsesOfAllSymbolsInFile though)
                              if fv.HasAttribute<ExtensionAttribute>() then
                                  yield fv :> FSharpSymbol
                      
                  for apCase in entity.ActivePatternCases do
                      yield apCase :> FSharpSymbol

                  // The IsNamespace and IsFSharpModule cases are handled by looking at DeclaringEntity below
                  if not entity.IsNamespace && not entity.IsFSharpModule then
                      for fv in entity.MembersFunctionsAndValues do 
                          yield fv :> FSharpSymbol |]

            HashSet<_>(symbols, symbolHash)

        member _.Entity = entity
        member _.IsNestedAutoOpen = isNestedAutoOpen
        member _.RevealedSymbolsContains(symbol) = revealedSymbols.Force().Contains symbol

    type OpenedModuleGroup = 
        { OpenedModules: OpenedModule [] }
        
        static member Create (modul: FSharpEntity) =
            let rec getModuleAndItsAutoOpens (isNestedAutoOpen: bool) (modul: FSharpEntity) =
                [|
                    yield OpenedModule (modul, isNestedAutoOpen)
                    for ent in modul.NestedEntities do
                        if ent.IsFSharpModule && ent.HasAttribute<AutoOpenAttribute>() then
                            yield! getModuleAndItsAutoOpens true ent |]
            { OpenedModules = getModuleAndItsAutoOpens false modul }

    /// Represents a single open statement
    type OpenStatement =
        { /// All namespaces, modules and types which this open declaration effectively opens, including the AutoOpen ones
          OpenedGroups: OpenedModuleGroup list

          /// The range of open statement itself
          Range: range

          /// The scope on which this open declaration is applied
          AppliedScope: range }

    /// Gets the open statements, their scopes and their resolutions
    let getOpenStatements (openDeclarations: FSharpOpenDeclaration[]) : OpenStatement[] = 
        openDeclarations
        |> Array.choose (fun openDecl ->
            if openDecl.IsOwnNamespace then
                None
            else
                match openDecl.LongId, openDecl.Range with
                | firstId :: _, Some range ->
                    if firstId.idText = MangledGlobalName then 
                        None
                    else
                        let openedModulesAndTypes = List.concat [openDecl.Modules; openDecl.Types |> List.map(fun ty -> ty.TypeDefinition)]
                        Some { OpenedGroups = openedModulesAndTypes |> List.map OpenedModuleGroup.Create
                               Range = range
                               AppliedScope = openDecl.AppliedScope }
                | _ -> None)

    /// Only consider symbol uses which are the first part of a long ident, i.e. with no qualifying identifiers
    let filterSymbolUses (getSourceLineStr: int -> string) (symbolUses: seq<FSharpSymbolUse>) =
        symbolUses
        |> Seq.filter(fun (su: FSharpSymbolUse) ->
            match su.Symbol with
            | :? FSharpMemberOrFunctionOrValue as fv when fv.IsExtensionMember -> 
                // Extension members should be taken into account even though they have a prefix (as they do most of the time)
                true

            | :? FSharpMemberOrFunctionOrValue as fv when not fv.IsModuleValueOrMember -> 
                // Local values can be ignored
                false

            | :? FSharpMemberOrFunctionOrValue when su.IsFromDefinition -> 
                // Value definitions should be ignored
                false

            | :? FSharpGenericParameter -> 
                // Generic parameters can be ignored, they never come into scope via 'open'
                false

            | :? FSharpUnionCase when su.IsFromDefinition -> 
                false

            | :? FSharpField as field when
                    field.DeclaringEntity.IsSome && field.DeclaringEntity.Value.IsFSharpRecord ->
                // Record fields are used in name resolution
                true

            | :? FSharpField as field when field.IsUnionCaseField ->
                false

            | _ ->
                // For the rest of symbols we pick only those which are the first part of a long ident, because it's they which are
                // contained in opened namespaces / modules. For example, we pick `IO` from long ident `IO.File.OpenWrite` because
                // it's `open System` which really brings it into scope.
                let partialName = QuickParse.GetPartialLongNameEx (getSourceLineStr su.Range.StartLine, su.Range.EndColumn - 1)
                List.isEmpty partialName.QualifyingIdents)
        |> Array.ofSeq

    /// Split symbol uses into cases that are easy to handle (via DeclaringEntity) and those that don't have a good DeclaringEntity
    let splitSymbolUses (symbolUses: FSharpSymbolUse[])  =
        symbolUses |> Array.partition (fun symbolUse ->
            let symbol = symbolUse.Symbol
            match symbol with
            | :? FSharpMemberOrFunctionOrValue as f ->
                match f.DeclaringEntity with
                | Some ent when ent.IsNamespace || ent.IsFSharpModule -> true
                | _ -> false
            | _ -> false)

    /// Given an 'open' statement, find fresh modules/namespaces referred to by that statement where there is some use of a revealed symbol
    /// in the scope of the 'open' is from that module.
    ///
    /// Performance will be roughly NumberOfOpenStatements x NumberOfSymbolUses
    let isOpenStatementUsed (symbolUses2: FSharpSymbolUse[]) (symbolUsesRangesByDeclaringEntity: Dictionary<FSharpEntity, range list>) 
                            (usedModules: Dictionary<FSharpEntity, range list>) (openStatement: OpenStatement) =

        // Don't re-check modules whose symbols are already known to have been used
        let openedGroupsToExamine =
            openStatement.OpenedGroups |> List.choose (fun openedGroup ->
                let openedEntitiesToExamine =
                    openedGroup.OpenedModules 
                    |> Array.filter (fun openedEntity ->
                        not (usedModules.BagExistsValueForKey(openedEntity.Entity, fun scope -> rangeContainsRange scope openStatement.AppliedScope)))
                             
                match openedEntitiesToExamine with
                | [||] -> None
                | _ when openedEntitiesToExamine |> Array.exists (fun x -> not x.IsNestedAutoOpen) -> Some { OpenedModules = openedEntitiesToExamine }
                | _ -> None)

        // Find the opened groups that are used by some symbol use
        let newlyUsedOpenedGroups = 
            openedGroupsToExamine |> List.filter (fun openedGroup ->
                openedGroup.OpenedModules |> Array.exists (fun openedEntity ->
                    symbolUsesRangesByDeclaringEntity.BagExistsValueForKey(openedEntity.Entity, fun symbolUseRange ->
                        rangeContainsRange openStatement.AppliedScope symbolUseRange &&
                        Position.posGt symbolUseRange.Start openStatement.Range.End) || 
                    
                    symbolUses2 |> Array.exists (fun symbolUse ->
                        rangeContainsRange openStatement.AppliedScope symbolUse.Range &&
                        Position.posGt symbolUse.Range.Start openStatement.Range.End &&
                        openedEntity.RevealedSymbolsContains symbolUse.Symbol)))

        // Return them as interim used entities
        let newlyOpenedModules = newlyUsedOpenedGroups |> List.collect (fun openedGroup -> openedGroup.OpenedModules |> List.ofArray)
        for openedModule in newlyOpenedModules do
            let scopes =
                match usedModules.TryGetValue openedModule.Entity with
                | true, scopes -> openStatement.AppliedScope :: scopes
                | _ -> [openStatement.AppliedScope]
            usedModules.[openedModule.Entity] <- scopes
        not newlyOpenedModules.IsEmpty
                                          
    /// Incrementally filter out the open statements one by one. Filter those whose contents are referred to somewhere in the symbol uses.
    /// Async to allow cancellation.
    let rec filterOpenStatementsIncremental symbolUses2 (symbolUsesRangesByDeclaringEntity: Dictionary<FSharpEntity, range list>) (openStatements: OpenStatement list)
                                            (usedModules: Dictionary<FSharpEntity, range list>) acc = 
        async { 
            match openStatements with
            | openStatement :: rest ->
                if isOpenStatementUsed symbolUses2 symbolUsesRangesByDeclaringEntity usedModules openStatement then
                    return! filterOpenStatementsIncremental symbolUses2 symbolUsesRangesByDeclaringEntity rest usedModules acc
                else
                    // The open statement has not been used, include it in the results
                    return! filterOpenStatementsIncremental symbolUses2 symbolUsesRangesByDeclaringEntity rest usedModules (openStatement :: acc)
            | [] -> return List.rev acc
        }

    let entityHash = HashIdentity.FromFunctions (fun (x: FSharpEntity) -> x.GetEffectivelySameAsHash()) (fun x y -> x.IsEffectivelySameAs(y))

    /// Filter out the open statements whose contents are referred to somewhere in the symbol uses.
    /// Async to allow cancellation.
    let filterOpenStatements (symbolUses1: FSharpSymbolUse[], symbolUses2: FSharpSymbolUse[]) openStatements =
        async {
            // the key is a namespace or module, the value is a list of FSharpSymbolUse range of symbols defined in the 
            // namespace or module. So, it's just symbol uses ranges grouped by namespace or module where they are _defined_. 
            let symbolUsesRangesByDeclaringEntity = Dictionary<FSharpEntity, range list>(entityHash)
            for symbolUse in symbolUses1 do
                match symbolUse.Symbol with
                | :? FSharpMemberOrFunctionOrValue as f ->
                    match f.DeclaringEntity with
                    | Some entity when entity.IsNamespace || entity.IsFSharpModule -> 
                        symbolUsesRangesByDeclaringEntity.BagAdd(entity, symbolUse.Range)
                    | _ -> ()
                | _ -> ()

            let! results = filterOpenStatementsIncremental symbolUses2 symbolUsesRangesByDeclaringEntity (List.ofArray openStatements) (Dictionary(entityHash)) []
            return results |> List.map (fun os -> os.Range)
        }

    /// Get the open statements whose contents are not referred to anywhere in the symbol uses.
    /// Async to allow cancellation.
    let getUnusedOpens (checkFileResults: FSharpCheckFileResults, getSourceLineStr: int -> string) : Async<range list> =
        async {
            let! ct = Async.CancellationToken
            let symbolUses = checkFileResults.GetAllUsesOfAllSymbolsInFile(ct)
            let symbolUses = filterSymbolUses getSourceLineStr symbolUses
            let symbolUses = splitSymbolUses symbolUses
            let openStatements = getOpenStatements checkFileResults.OpenDeclarations
            return! filterOpenStatements symbolUses openStatements
        }
 
module SimplifyNames = 
    type SimplifiableRange =
        {
          Range: range
          RelativeName: string
        }

    let getPlidLength (plid: string list) = (plid |> List.sumBy String.length) + plid.Length    

    let getSimplifiableNames (checkFileResults: FSharpCheckFileResults, getSourceLineStr: int -> string) =
        async {
            let result = ResizeArray()
            let! ct = Async.CancellationToken
            let symbolUses =
                checkFileResults.GetAllUsesOfAllSymbolsInFile(ct)
                |> Seq.choose (fun symbolUse ->
                    if symbolUse.IsFromOpenStatement || symbolUse.IsFromDefinition then
                        None
                    else
                        let lineStr = getSourceLineStr symbolUse.Range.StartLine
                        // for `System.DateTime.Now` it returns ([|"System"; "DateTime"|], "Now")
                        let partialName = QuickParse.GetPartialLongNameEx(lineStr, symbolUse.Range.EndColumn - 1)
                        // `symbolUse.Range.Start` does not point to the start of plid, it points to start of `name`,
                        // so we have to calculate plid's start ourselves.
                        let plidStartCol = symbolUse.Range.EndColumn - partialName.PartialIdent.Length - (getPlidLength partialName.QualifyingIdents)
                        if partialName.PartialIdent = "" || List.isEmpty partialName.QualifyingIdents then
                            None
                        else
                            Some (symbolUse, partialName.QualifyingIdents, plidStartCol, partialName.PartialIdent))
                |> Seq.groupBy (fun (symbolUse, _, plidStartCol, _) -> symbolUse.Range.StartLine, plidStartCol)
                |> Seq.map (fun (_, xs) -> xs |> Seq.maxBy (fun (symbolUse, _, _, _) -> symbolUse.Range.EndColumn))

            for symbolUse, plid, plidStartCol, name in symbolUses do
                let posAtStartOfName =
                    let r = symbolUse.Range
                    if r.StartLine = r.EndLine then Position.mkPos r.StartLine (r.EndColumn - name.Length)
                    else r.Start   

                let getNecessaryPlid (plid: string list) : string list =
                    let rec loop (rest: string list) (current: string list) =
                        match rest with
                        | [] -> current
                        | headIdent :: restPlid ->
                            let res = checkFileResults.IsRelativeNameResolvableFromSymbol(posAtStartOfName, current, symbolUse.Symbol)
                            if res then current
                            else loop restPlid (headIdent :: current)
                    loop (List.rev plid) []
                    
                let necessaryPlid = getNecessaryPlid plid 
                    
                match necessaryPlid with
                | necessaryPlid when necessaryPlid = plid -> ()
                | necessaryPlid ->
                    let r = symbolUse.Range
                    let necessaryPlidStartCol = r.EndColumn - name.Length - (getPlidLength necessaryPlid)
                    
                    let unnecessaryRange = 
                        mkRange r.FileName (Position.mkPos r.StartLine plidStartCol) (Position.mkPos r.EndLine necessaryPlidStartCol)
                    
                    let relativeName = (String.concat "." plid) + "." + name
                    result.Add({Range = unnecessaryRange; RelativeName = relativeName})

            return (result :> seq<_>)
        }

module UnusedDeclarations = 
    let isPotentiallyUnusedDeclaration (symbol: FSharpSymbol) : bool =
        match symbol with

        // Determining that a record, DU or module is used anywhere requires inspecting all their enclosed entities (fields, cases and func / vals)
        // for usages, which is too expensive to do. Hence we never gray them out.
        | :? FSharpEntity as e when e.IsFSharpRecord || e.IsFSharpUnion || e.IsInterface || e.IsFSharpModule || e.IsClass || e.IsNamespace -> false

        // FCS returns inconsistent results for override members; we're skipping these symbols.
        | :? FSharpMemberOrFunctionOrValue as f when 
                f.IsOverrideOrExplicitInterfaceImplementation ||
                f.IsBaseValue ||
                f.IsConstructor -> false

        // Usage of DU case parameters does not give any meaningful feedback; we never gray them out.
        | :? FSharpParameter -> false
        | _ -> true

    let getUnusedDeclarationRanges (symbolsUses: seq<FSharpSymbolUse>) (isScript: bool) =
        let usages =
            let usages = 
                symbolsUses
                |> Seq.choose (fun su -> if not su.IsFromDefinition then su.Symbol.DeclarationLocation else None)
            HashSet(usages)

        symbolsUses
        |> Seq.distinctBy (fun su -> su.Range) // Account for "hidden" uses, like a val in a member val definition. These aren't relevant
        |> Seq.choose(fun (su: FSharpSymbolUse) ->
            if su.IsFromDefinition && 
                su.Symbol.DeclarationLocation.IsSome && 
                (isScript || su.IsPrivateToFile) && 
                not (su.Symbol.DisplayName.StartsWith "_") &&
                isPotentiallyUnusedDeclaration su.Symbol
            then
                Some (su, usages.Contains su.Symbol.DeclarationLocation.Value)
            else
                None)
        |> Seq.groupBy (fun (defSu, _) -> defSu.Range)
        |> Seq.filter (fun (_, defSus) -> defSus |> Seq.forall (fun (_, isUsed) -> not isUsed))
        |> Seq.map (fun (m, _) -> m)
    
    let getUnusedDeclarations(checkFileResults: FSharpCheckFileResults, isScriptFile: bool) = 
        async {
            let! ct = Async.CancellationToken
            let allSymbolUsesInFile = checkFileResults.GetAllUsesOfAllSymbolsInFile(ct)
            let unusedRanges = getUnusedDeclarationRanges allSymbolUsesInFile isScriptFile
            return unusedRanges
        }