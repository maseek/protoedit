module ParseIn

open System
open ProtoDescriptor

let parseProtoLine ((proto, breadcrumbs) : (ProtoZipper)) (line : string) : ProtoZipper =
    let words = line.Split ' ' |> Array.toList |> List.filter (fun x -> not (x.Equals("")))
    
    let rec addMessage name messages crumbs : MessageZipper =
        match crumbs with
        | [] -> ({Name = name; Children = []; Fields = []; Enums = []} :: messages, MessageCrumb :: crumbs)
        | MessageCrumb :: bs -> 
            let firstMessage = List.head messages
            ({firstMessage with Children = (addMessage name firstMessage.Children bs) |> fst} :: List.tail messages, MessageCrumb :: crumbs)
        | _ -> (messages, crumbs)

    let rec addEnum name (messages : MessageDescriptor list) crumbs : MessageZipper =
        match crumbs with
        | [MessageCrumb] ->
            let firstMessage = List.head messages
            ({firstMessage with Enums = {Name = name; Values = []} :: firstMessage.Enums} :: List.tail messages, EnumCrumb :: crumbs)
        | MessageCrumb :: bs -> 
            let firstMessage = List.head messages
            ({firstMessage with Children = (addEnum name firstMessage.Children bs) |> fst} :: List.tail messages, EnumCrumb :: crumbs)
        | _ -> (messages, crumbs)


    let parseFieldDescriptor words : FieldDescriptor option =
        let parseFieldDescriptorType word : FieldType option =
            match word with
            | "int32" -> Some(FieldType.TypeInt32)
            | "string" -> Some(FieldType.TypeString)
            | _ -> None

        let parseFieldDescriptorNumber word : int option =
            try
                Some(Int32.Parse(word))
            with
                | :? FormatException -> None

        let buildFieldDescriptor fLabel fType fName (fNumber : string) =
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

    let rec addMessageField words messages crumbs : MessageZipper = 
        match crumbs with 
        | [MessageCrumb] -> 
            match (parseFieldDescriptor words) with
            | Some(x) ->
                let firstMessage = List.head messages
                ({firstMessage with Fields = x :: firstMessage.Fields} :: List.tail messages, crumbs)
            | None -> (messages, crumbs)
        | MessageCrumb :: bs -> 
            let firstMessage = List.head messages
            let children, fCrumbs = addMessageField words firstMessage.Children bs
            ({firstMessage with Children = children} :: List.tail messages, crumbs)
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
   let proto = {FilePath = filePath; Messages = []; Enums = []} : ProtoDescriptor
   let breadcrumbs = [] : ProtoCrumbs
   Seq.fold parseProtoLine (proto, breadcrumbs) fileLines