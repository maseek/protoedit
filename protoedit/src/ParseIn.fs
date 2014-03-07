module ParseIn

open System
open ProtoDescriptor

let parseProtoLine ((proto, breadcrumbs) : (ProtoZipper)) (line : string) : ProtoZipper =
    let words = line.Split ' ' |> Array.toList |> List.filter (fun x -> not (x.Equals("")))
    
    let rec addMessage name messages crumbs : MessagesZipper =
        match crumbs with
        | [] -> ({Name = name; Messages = []; Fields = []; Enums = []} :: messages, MessageCrumb :: crumbs)
        | MessageCrumb :: bs -> 
            let firstMessage = List.head messages
            ({firstMessage with Messages = (addMessage name firstMessage.Messages bs) |> fst} :: List.tail messages, MessageCrumb :: crumbs)
        | _ -> (messages, crumbs)

    let rec addEnum name (messages : MessageDescriptor list) crumbs : MessagesZipper =
        match crumbs with
        | [MessageCrumb] ->
            let firstMessage = List.head messages
            ({firstMessage with Enums = {Name = name; Values = []} :: firstMessage.Enums} :: List.tail messages, EnumCrumb :: crumbs)
        | MessageCrumb :: bs -> 
            let firstMessage = List.head messages
            ({firstMessage with Messages = (addEnum name firstMessage.Messages bs) |> fst} :: List.tail messages, EnumCrumb :: crumbs)
        | _ -> (messages, crumbs)

    let rec addMessageField words messages crumbs : MessagesZipper =
        let parseFieldDescriptor words : FieldDescriptor option =
            let buildFieldDescriptor fLabel fType fName (fNumber : string) rest =
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

                        let rec getCurrentMessage proto crumbs =
                            match crumbs with
                            | [MessageCrumb] -> List.head proto.Messages
                            | MessageCrumb :: bs -> getCurrentMessage (List.head proto.Messages) bs
                            | _ -> proto

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

                //TODO: finish - fold, trim and then pattern match
                let parseFieldDescriptorDefault fieldType (rest : String list) : obj option =
                    match (fieldType, rest.Head.TrimStart('[') :: rest.Tail) with
                    | (Some(fType), "default" :: "=" :: value :: _) ->
                        let stringValue = value.TrimEnd([|']'; ';'|])
                        None
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
    | "}" :: xs -> (proto, List.tail breadcrumbs)
    | ("required" | "optional" | "repeated") :: xs ->
        match breadcrumbs with
        | [] -> (proto, breadcrumbs)
        | _ -> 
            let messages, crumbs = addMessageField words proto.Messages breadcrumbs
            ({proto with Messages = messages}, breadcrumbs)
    | _ -> (proto, breadcrumbs)

let parseProtoFile (filePath : string) (fileLines : seq<string>) =
   let proto = {Name = filePath; Messages = []; Fields = []; Enums = []} : MessageDescriptor
   let breadcrumbs = [] : ProtoCrumbs
   Seq.fold parseProtoLine (proto, breadcrumbs) fileLines