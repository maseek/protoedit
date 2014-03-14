module ProtoDescriptor

open System

type PrimitiveFieldType = 
    | TypeDouble = 1
    | TypeFloat = 2
    | TypeInt32 = 3
    | TypeInt64 = 5
    | TypeUInt32 = 6
    | TypeUInt64 = 7
    | TypeSInt32 = 8
    | TypeSInt64 = 9
    | TypeFixed32 = 10
    | TypeFixed64 = 11
    | TypeSFixed32 = 12
    | TypeSFixed64 = 13
    | TypeBool = 14
    | TypeString = 15
    | TypeBytes = 16
    
type EnumFieldType =
    | EnumTypeNode of (string * EnumFieldType)
    | EnumTypeEmpty

type MessageFieldType =
    | MessageTypeNode of (string * MessageFieldType)
    | MessageTypeEmpty

type FieldType =
    | Primitive of PrimitiveFieldType
    | Enum of EnumFieldType
    | Message of MessageFieldType

let primitiveFieldTypes =
    Map.empty.
        Add("double", PrimitiveFieldType.TypeDouble).
        Add("float", PrimitiveFieldType.TypeFloat).
        Add("int32", PrimitiveFieldType.TypeInt32).
        Add("int64", PrimitiveFieldType.TypeInt64).
        Add("uint32", PrimitiveFieldType.TypeUInt32).
        Add("uint64", PrimitiveFieldType.TypeUInt64).
        Add("sint32", PrimitiveFieldType.TypeSInt32).
        Add("sint64", PrimitiveFieldType.TypeSInt64).
        Add("fixed32", PrimitiveFieldType.TypeFixed32).
        Add("fixed64", PrimitiveFieldType.TypeFixed64).
        Add("sfixed32", PrimitiveFieldType.TypeSFixed32).
        Add("sfixed64", PrimitiveFieldType.TypeSFixed64).
        Add("bool", PrimitiveFieldType.TypeBool).
        Add("string", PrimitiveFieldType.TypeString).
        Add("bytes", PrimitiveFieldType.TypeBytes)
        
let show (fieldType : FieldType) : String =
    match fieldType with
    | Primitive p -> Map.fold (fun state key value -> if value.Equals(p) then key else state) "" primitiveFieldTypes
    | Enum e -> 
        match e with
        | EnumTypeNode (name, next) -> 
            let rec addNext next =
                match next with
                | EnumTypeNode (name, next) -> "." + name + addNext next
                | _ -> ""
            name + addNext next
        | _ -> ""
    | Message m -> 
        match m with
        | MessageTypeNode (name, next) -> 
            let rec addNext next =
                match next with
                | MessageTypeNode (name, next) -> "." + name + addNext next
                | _ -> ""
            name + addNext next
        | _ -> ""
    | _ -> ""

type FieldLabel =
    | LabelOptional = 1
    | LabelRequired = 2
    | LabelRepeated = 3

type FieldDescriptor =
    {Name : string;
    Number : int;
    Type : FieldType;
    Label : FieldLabel;
    Default : obj option}

type EnumValueDescriptor =
    {Name : string;
    Number : int}

type EnumDescriptor =
    {Name : string;
    Values : EnumValueDescriptor list}
    
type MessageDescriptor = 
    {Name : string;
    Messages : MessageDescriptor list;
    Fields : FieldDescriptor list;
    Enums : EnumDescriptor list}

type ProtoCrumb = 
    | MessageCrumb
    | EnumCrumb

type ProtoCrumbs = ProtoCrumb list

type ProtoZipper = MessageDescriptor * ProtoCrumbs
type MessagesZipper = MessageDescriptor list * ProtoCrumbs