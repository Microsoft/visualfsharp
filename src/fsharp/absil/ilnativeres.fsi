
module internal FSharp.Compiler.AbstractIL.NativeRes

open System
open System.Collections.Generic
open System.IO
open System.Reflection.Metadata

type BYTE = Byte
type DWORD = UInt32
type WCHAR = Char
type WORD = UInt16

[<Class>]
type RESOURCE_STRING =
    member Ordinal: WORD with get, set
    member theString : string with get, set

[<Class>]
type RESOURCE =
    member pstringType : RESOURCE_STRING with get, set
    member pstringName : RESOURCE_STRING with get, set
    member DataSize : DWORD with get, set
    member HeaderSize : DWORD with get, set
    member DataVersion : DWORD with get, set
    member MemoryFlags : WORD with get, set
    member LanguageId : WORD with get, set
    member Version : DWORD with get, set
    member Characteristics : DWORD with get, set
    member data : byte[] with get, set

type Win32Resource =
    new : data:byte [] * codePage: DWORD * languageId: DWORD * id: int *
          name: string * typeId:int * typeName : string -> Win32Resource
    member CodePage: DWORD
    member Data: byte []
    member Id: int
    member LanguageId : DWORD
    member Name: string
    member TypeId: int
    member TypeName: string

[<Class>]
type CvtResFile =
    static member ReadResFile : stream:Stream -> List<RESOURCE>

[<Class>]
type Win32ResourceConversions =
    static member AppendIconToResourceStream : resStream:Stream * iconStream:Stream -> unit
    static member AppendVersionToResourceStream : resStream:Stream * isDll:Boolean * fileVersion:string * originalFileName:string * internalName:string * productVersion:string * assemblyVersion:Version * ?fileDescription:string * ?legalCopyright:string * ?legalTrademarks:string * ?productName:string * ?comments:string * ?companyName:string -> unit
    static member AppendManifestToResourceStream : resStream:Stream * manifestStream:Stream * isDll:Boolean -> unit

// Write native resources
[<Class>]
type NativeResourceWriter =
    static member SortResources: resources: IEnumerable<Win32Resource> -> IEnumerable<Win32Resource>
    static member SerializeWin32Resources: builder:BlobBuilder * theResources: IEnumerable<Win32Resource> * resourcesRva: int -> unit
    (*
    static member SerializeWin32Resources (builder : BlobBuilder, resourceSections : ResourceSection, resourcesRva : int) -> unit
        ()*)