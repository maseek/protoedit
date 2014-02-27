module ParseIn

open ProtoDescriptor

let parseProtoLine ((proto, breadcrumbs) : (ProtoZipper)) (line : string) : ProtoZipper =
    let words = line.Split [|' '|] |> Array.toList |> List.filter (fun x -> not (x.Equals("")))
    
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

    //TODO:
    let rec addMessageField definition messages crumbs : MessageZipper = (messages, crumbs)

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
            let messages, crumbs = addMessageField xs proto.Messages breadcrumbs
            ({proto with Messages = messages}, crumbs)
    | _ -> (proto, breadcrumbs)

let parseProtoFile (filePath : string) (fileLines : seq<string>) =
   let proto = {FilePath = filePath; Messages = []; Enums = []} : ProtoDescriptor
   let breadcrumbs = [] : ProtoCrumbs
   Seq.fold parseProtoLine (proto, breadcrumbs) fileLines