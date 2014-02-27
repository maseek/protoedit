module ProtoDescriptor

type FieldType = 
    | TypeDouble = 1
    | TypeFloat = 2
    | TypeInt64 = 3
    | TypeUint64 = 4
    | TypeInt32 = 5
    | TypeFixed64 = 6
    | TypeFixed32 = 7
    | TypeBool = 8
    | TypeString = 9
    | TypeGroup = 10
    | TypeMessage = 11
    | TypeBytes = 12
    | TypeUint32 = 13
    | TypeEnum = 14
    | TypeSFixed32 = 15
    | TypeSFixed64 = 16
    | TypeSInt32 = 17
    | TypeSInt64 = 18

type FieldLabel =
    | LabelOptional = 1
    | LabelRequired = 2
    | LabelRepeated = 3

type FieldDescriptor =
    {Name : string;
    Number : int;
    Type : FieldType;
    Label : FieldLabel;}

type EnumValueDescriptor =
    {Name : string;
    Number : int}

type EnumDescriptor =
    {Name : string;
    Values : EnumValueDescriptor list}
    
type MessageDescriptor = 
    {Name : string;
    Children : MessageDescriptor list;
    Fields : FieldDescriptor list;
    Enums : EnumDescriptor list}

type ProtoDescriptor = 
    {FilePath : string;
    Messages : MessageDescriptor list;
    Enums : EnumDescriptor list}

type ProtoCrumb = 
    | MessageCrumb
    | EnumCrumb

type ProtoCrumbs = ProtoCrumb list

type ProtoZipper = ProtoDescriptor * ProtoCrumbs
type MessageZipper = MessageDescriptor list * ProtoCrumbs