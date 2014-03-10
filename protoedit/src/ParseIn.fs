module ParseIn

open System
open ProtoDescriptor

let filterEmpty (xs : String list) = List.filter (fun x -> not (x.Equals(""))) xs

let parseProtoLine ((proto, breadcrumbs) : (ProtoZipper)) (line : string) : ProtoZipper =
    let words = line.Split ' ' |> Array.toList |>  filterEmpty
    
    let rec addMessage name messages crumbs : MessagesZipper =
        match crumbs with
        | [] -> ({Name = name; Messages = []; Fields = []; Enums = []} :: messages, List.append crumbs [MessageCrumb])
        | MessageCrumb :: bs -> 
            let firstMessage = List.head messages
            ({firstMessage with Messages = (addMessage name firstMessage.Messages bs) |> fst} :: List.tail messages, List.append crumbs [MessageCrumb])
        | _ -> (messages, crumbs)

    let rec addEnum name (messages : MessageDescriptor list) crumbs : MessagesZipper =
        match crumbs with
        | [MessageCrumb] ->
            let firstMessage = List.head messages
            ({firstMessage with Enums = {Name = name; Values = []} :: firstMessage.Enums} :: List.tail messages, List.append crumbs [EnumCrumb])
        | MessageCrumb :: bs -> 
            let firstMessage = List.head messages
            ({firstMessage with Messages = (addEnum name firstMessage.Messages bs) |> fst} :: List.tail messages, List.append crumbs [EnumCrumb])
        | _ -> (messages, crumbs)

    let rec addEnumField name value messages crumbs : MessagesZipper =
        match crumbs with
        | [EnumCrumb] ->
            let firstMessage = List.head messages
            let firstEnum = List.head firstMessage.Enums
            let number = let (ok, v) = System.Int32.TryParse(value) in if ok then Some(v) else None
            match number with
            | Some(number) -> 
                let newEnum = {firstEnum with Values = ({Name = name; Number = number} :: firstEnum.Values)}
                ({firstMessage with Enums = newEnum :: List.tail firstMessage.Enums} :: List.tail messages, crumbs)
            | _ -> (messages, crumbs)
        | MessageCrumb :: bs ->
            let firstMessage = List.head messages
            ({firstMessage with Messages = (addEnumField name value firstMessage.Messages bs) |> fst} :: List.tail messages, crumbs)
        | _ -> (messages, crumbs)

    let rec addMessageField words messages crumbs : MessagesZipper =
        let parseFieldDescriptor words : FieldDescriptor option =
            let buildFieldDescriptor fLabel fType fName (fNumber : string) rest =
                let rec getCurrentMessage proto crumbs =
                    match crumbs with
                    | [MessageCrumb] -> List.head proto.Messages
                    | MessageCrumb :: bs -> getCurrentMessage (List.head proto.Messages) bs
                    | _ -> proto

                let rec getEnumDescriptor enumFieldType message =
                    match enumFieldType with
                    | EnumTypeNode (name, EnumTypeEmpty) -> 
                        let enums = List.filter (fun (e : EnumDescriptor) -> e.Name.Equals(name)) message.Enums
                        if enums.Length > 0 then Some(enums.Head) else None
                    | EnumTypeNode (name, child) -> 
                        let message = (List.fold (fun s (m : MessageDescriptor) -> if m.Name.Equals(name) then Some(m) else None) None message.Messages)
                        match message with
                        | Some(m) -> getEnumDescriptor child m
                        | None -> None
                    | _ -> None

                let getRelativeOrAbsoluteEnumDescriptor enumFieldType =
                    let relativePathEnum = getEnumDescriptor enumFieldType (getCurrentMessage proto crumbs)
                    let absolutePathEnum = getEnumDescriptor enumFieldType proto
                    match (relativePathEnum, absolutePathEnum) with
                    | Some _, _ -> relativePathEnum
                    | _, Some _ -> absolutePathEnum
                    | _,_ -> None

                let parseFieldDescriptorType word : FieldType option =
                    match primitiveFieldTypes.TryFind word with
                    | Some(fType) -> Some(Primitive fType)
                    | None ->
                        let rec parseEnumFieldType enumNames =
                            match enumNames with
                            | [e] -> EnumTypeNode (e, EnumTypeEmpty)
                            | e :: es -> EnumTypeNode (e, parseEnumFieldType es)
                            | _ -> EnumTypeEmpty

                        let enumFieldType = parseEnumFieldType (word.Split '.' |> Array.toList)
                        
                            
                        let rec doesEnumExist enumFieldType message =
                            match enumFieldType with
                            | EnumTypeNode (name, EnumTypeEmpty) -> List.fold (fun s (e : EnumDescriptor) -> s || e.Name.Equals(name)) false message.Enums
                            | EnumTypeNode (name, child) -> 
                                let message = (List.fold (fun s (m : MessageDescriptor) -> if m.Name.Equals(name) then Some(m) else None) None message.Messages)
                                match message with
                                | Some(m) -> doesEnumExist child m
                                | None -> false
                            | _ -> false

                        if (doesEnumExist enumFieldType (getCurrentMessage proto crumbs)) || (doesEnumExist enumFieldType proto) then
                            match enumFieldType with
                            | EnumTypeNode (_,_) -> Some(Enum enumFieldType)
                            | EnumTypeEmpty -> None
                        else
                            let rec parseMessageFieldType messageNames =
                                match messageNames with
                                | [m] -> MessageTypeNode (m, MessageTypeEmpty)
                                | m :: ms -> MessageTypeNode (m, parseMessageFieldType ms)
                                | _ -> MessageTypeEmpty

                            let messageFieldType = parseMessageFieldType (word.Split '.' |> Array.toList)

                            let rec doesMessageExist messageFieldType messages =
                                match messageFieldType with
                                | MessageTypeNode (name, MessageTypeEmpty) -> List.fold (fun s (m : MessageDescriptor) -> s || m.Name.Equals(name)) false messages
                                | MessageTypeNode (name, child) -> List.fold (fun s (m : MessageDescriptor) -> s || m.Name.Equals(name) && doesMessageExist child m.Messages) false messages
                                | _ -> false

                            if (doesMessageExist messageFieldType (getCurrentMessage proto crumbs).Messages) || (doesMessageExist messageFieldType proto.Messages) then
                                match messageFieldType with
                                | MessageTypeNode (_,_) -> Some(Message messageFieldType)
                                | MessageTypeEmpty -> None
                            else None

                let parseFieldDescriptorNumber word : int option =
                    try
                        Some(Int32.Parse(word))
                    with
                        | :? FormatException -> None

                let parseFieldDescriptorDefault fieldType (rest : String list) : obj option =
                    let stripped = (List.fold (fun x xs -> x + xs) "" rest).TrimStart('[').TrimEnd([|']'; ';'|]).Split('=') |> Array.toList |> filterEmpty
                    match (fieldType, stripped) with
                    | (Some(fType), "default" :: value :: _) ->
                        match fType with
                        | Primitive primitiveFieldType ->
                            match primitiveFieldType with
                            | PrimitiveFieldType.TypeDouble -> let (ok, v) = System.Double.TryParse(value) in if ok then Some(v |> box) else None
                            | PrimitiveFieldType.TypeFloat -> let (ok, v) = System.Single.TryParse(value) in if ok then Some(v |> box) else None
                            | PrimitiveFieldType.TypeInt32  -> let (ok, v) = System.Int32.TryParse(value) in if ok then Some(v |> box) else None
                            | PrimitiveFieldType.TypeInt64 -> let (ok, v) = System.Int64.TryParse(value) in if ok then Some(v |> box) else None
                            | PrimitiveFieldType.TypeUInt32 -> let (ok, v) = System.UInt32.TryParse(value) in if ok then Some(v |> box) else None
                            | PrimitiveFieldType.TypeUInt64 -> let (ok, v) = System.UInt64.TryParse(value) in if ok then Some(v |> box) else None
                            | PrimitiveFieldType.TypeSInt32  -> let (ok, v) = System.Int32.TryParse(value) in if ok then Some(v |> box) else None
                            | PrimitiveFieldType.TypeSInt64 -> let (ok, v) = System.Int64.TryParse(value) in if ok then Some(v |> box) else None
                            | PrimitiveFieldType.TypeFixed32 -> let (ok, v) = System.UInt32.TryParse(value) in if ok then Some(v |> box) else None
                            | PrimitiveFieldType.TypeFixed64 -> let (ok, v) = System.UInt64.TryParse(value) in if ok then Some(v |> box) else None
                            | PrimitiveFieldType.TypeSFixed32 -> let (ok, v) = System.Int32.TryParse(value) in if ok then Some(v |> box) else None
                            | PrimitiveFieldType.TypeSFixed64 -> let (ok, v) = System.Int64.TryParse(value) in if ok then Some(v |> box) else None
                            | PrimitiveFieldType.TypeBool -> let (ok, v) = System.Boolean.TryParse(value) in if ok then Some(v |> box) else None
                            | PrimitiveFieldType.TypeString -> Some(value.Trim('"') |> box)
                            | PrimitiveFieldType.TypeBytes -> Some(System.Text.Encoding.UTF8.GetBytes(value) |> box)
                            | _ -> None
                        //TODO: check after enum values parsing is done
                        | Enum enumFieldType -> 
                            let enum = getRelativeOrAbsoluteEnumDescriptor enumFieldType
                            match enum with
                            | Some e ->
                                let enumVals = List.filter (fun (enumVal : EnumValueDescriptor) -> enumVal.Name.Equals(value)) e.Values
                                if enumVals.Length > 0 then Some((List.head enumVals).Number |> box) else None
                            | None -> None
                        | _ -> None
                    | _ -> None

                let fieldType = parseFieldDescriptorType fType
                let fieldNumber = parseFieldDescriptorNumber (fNumber.TrimEnd ';')
                let fieldDefault = parseFieldDescriptorDefault fieldType rest
                match fieldType, fieldNumber with
                    | Some(fType), Some(fNumber) -> Some({Label = fLabel; Type = fType; Name = fName; Number = fNumber; Default = fieldDefault})
                    | _ -> None

            let fLabel =
                match List.head words with
                | "optional" -> Some(FieldLabel.LabelOptional)
                | "required" -> Some(FieldLabel.LabelRequired)
                | "repeated" -> Some(FieldLabel.LabelRepeated)
                | _ -> None

            match fLabel, List.tail words with
            | Some(fLabel), fType :: fName :: "=" :: fNumber :: rest -> 
                buildFieldDescriptor fLabel fType fName fNumber rest
            | _ -> None

        match crumbs with 
        | [MessageCrumb] -> 
            match (parseFieldDescriptor words) with
            | Some(x) ->
                let firstMessage = List.head messages
                ({firstMessage with Fields = x :: firstMessage.Fields} :: List.tail messages, crumbs)
            | None -> (messages, crumbs)
        | MessageCrumb :: bs -> 
            let firstMessage = List.head messages
            let children, fCrumbs = addMessageField words firstMessage.Messages bs
            ({firstMessage with Messages = children} :: List.tail messages, crumbs)
        | _ -> (messages, crumbs)

    match words with
    | "message" :: xs -> 
        let messages, crumbs = addMessage (List.head xs) proto.Messages breadcrumbs
        ({proto with Messages = messages}, crumbs)
    | "enum" :: xs ->
        match breadcrumbs with
        | [] -> ({proto with Enums = {Name = List.head xs; Values = []} :: proto.Enums}, EnumCrumb :: breadcrumbs)
        | _ ->
            let messages, crumbs = addEnum (List.head xs) proto.Messages breadcrumbs
            ({proto with Messages = messages}, crumbs)
    | "}" :: xs -> (proto, List.tail (List.rev breadcrumbs))
    | ("required" | "optional" | "repeated") :: xs ->
        match breadcrumbs with
        | [] -> (proto, breadcrumbs)
        | _ -> 
            let messages, crumbs = addMessageField words proto.Messages breadcrumbs
            ({proto with Messages = messages}, breadcrumbs)

    | enumFieldName :: "=" :: enumFieldVal :: _ -> 
        let protoList, crumbs = addEnumField enumFieldName (enumFieldVal.TrimEnd(';')) [proto] breadcrumbs
        (List.head protoList, breadcrumbs)
    | _ -> (proto, breadcrumbs)

let parseProtoFile (filePath : string) (fileLines : seq<string>) =
   let proto = {Name = filePath; Messages = []; Fields = []; Enums = []} : MessageDescriptor
   let breadcrumbs = [] : ProtoCrumbs
   Seq.fold parseProtoLine (proto, breadcrumbs) fileLines