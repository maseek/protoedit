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
            let buildFieldDescriptor fLabel fType fName (fNumber : string) =
                let parseFieldDescriptorType word : FieldType option =
                    let primitiveFieldType = primitiveFieldTypes.TryFind word
                    match primitiveFieldType with
                    | Some(fType) -> Some(Primitive fType)
                    | None ->
                        let rec parseEnumFieldType enumNames =
                            match enumNames with
                            | [e] -> EnumTypeNode (e, EnumTypeEmpty)
                            | e :: es -> EnumTypeNode (e, parseEnumFieldType es)
                            | _ -> EnumTypeEmpty
                        let enumFieldType = parseEnumFieldType (word.Split '.' |> Array.toList)
                        let rec doesExist enum (message : MessageDescriptor) =
                            match enum with
                            | EnumTypeNode (name, EnumTypeEmpty) -> List.fold (fun s (e : EnumDescriptor) -> s || e.Name.Equals(name)) false message.Enums
                            | EnumTypeNode (name, child) -> 
                                let message = (List.fold (fun s (m : MessageDescriptor) -> if m.Name.Equals(name) then Some(m) else None) None message.Messages)
                                match message with
                                | Some(m) -> doesExist child m
                                | None -> false
                            | _ -> false
                        if doesExist enumFieldType proto then
                            match enumFieldType with
                            | EnumTypeNode (_,_) -> Some(Enum enumFieldType)
                            | EnumTypeEmpty -> None
                        //TODO: if enum doesn't exist try for message
                        else None

                let parseFieldDescriptorNumber word : int option =
                    try
                        Some(Int32.Parse(word))
                    with
                        | :? FormatException -> None

                let fieldType = parseFieldDescriptorType fType
                let fieldNumber = parseFieldDescriptorNumber (fNumber.TrimEnd ';')
                match fieldType, fieldNumber with
                    | Some(fType), Some(fNumber) -> Some({Label = fLabel; Type = fType; Name = fName; Number = fNumber})
                    | _ -> None

            match words with
            | "optional" :: fType :: fName :: "=" :: fNumber :: _ -> 
                buildFieldDescriptor FieldLabel.LabelOptional fType fName fNumber
            | "required" :: fType :: fName :: "=" :: fNumber :: _ -> 
                buildFieldDescriptor FieldLabel.LabelRequired fType fName fNumber
            | "repeated" :: fType :: fName :: "=" :: fNumber :: _ -> 
                buildFieldDescriptor FieldLabel.LabelRepeated fType fName fNumber
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