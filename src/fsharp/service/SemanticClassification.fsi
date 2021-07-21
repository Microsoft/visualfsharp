// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace FSharp.Compiler.EditorServices

open FSharp.Compiler.AccessibilityLogic
open FSharp.Compiler.Import
open FSharp.Compiler.NameResolution
open FSharp.Compiler.TcGlobals
open FSharp.Compiler.Text
open FSharp.Compiler.TypedTreeOps

/// A kind that determines what range in a source's text is semantically classified as after type-checking.
[<RequireQualifiedAccess>]
type SemanticClassificationType =
    | ReferenceType = 0
    | ValueType = 1
    | UnionCase = 2
    | UnionCaseField = 3
    | Function = 4
    | Property = 5
    | MutableVar = 6
    | Module = 7
    | Namespace = 8
    | Printf = 9
    | ComputationExpression = 10
    | IntrinsicFunction = 11
    | Enumeration = 12
    | Interface = 13
    | TypeArgument = 14
    | Operator = 15
    | DisposableType = 16
    | DisposableTopLevelValue = 17
    | DisposableLocalValue = 18
    | Method = 19
    | ExtensionMethod = 20
    | ConstructorForReferenceType = 21
    | ConstructorForValueType = 22
    | Literal = 23
    | RecordField = 24
    | MutableRecordField = 25
    | RecordFieldAsFunction = 26
    | Exception = 27
    | Field = 28
    | Event = 29
    | Delegate = 30
    | NamedArgument = 31
    | Value = 32
    | LocalValue = 33
    | Type = 34
    | TypeDef = 35
    | Plaintext = 36

[<RequireQualifiedAccess>]
[<Struct>]
type SemanticClassificationItem =
    val Range: range
    val Type: SemanticClassificationType
    new: (range * SemanticClassificationType) -> SemanticClassificationItem

/// Extension methods for the TcResolutions type.
[<AutoOpen>]
module internal TcResolutionsExtensions =
    val (|CNR|) : cnr: CapturedNameResolution -> Item * ItemOccurence * DisplayEnv * NameResolutionEnv * AccessorDomain * range

    type TcResolutions with
        member GetSemanticClassification: g: TcGlobals * amap: ImportMap * formatSpecifierLocations: (range * int) [] * range: range option -> SemanticClassificationItem []