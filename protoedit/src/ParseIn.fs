module ParseIn

open ProtoDescriptor

let parseProtoLine ((proto, breadcrumbs) : (ProtoZipper)) (line : string) : ProtoZipper =
    let words = line.Split [|' '|] |> Array.toList |> List.filter (fun x -> if x.Equals("") then false else true)
    
    let rec addProto name children crumbs : MessageZipper =
        match crumbs with
        | [] -> ({Name = name; Children = []; Fields = []; Enums = []} :: children, MessageCrumb :: crumbs)
        | MessageCrumb :: bs -> 
            let child = List.head children
            ({child with Children = (addProto name child.Children bs) |> fst} :: List.tail children, MessageCrumb :: crumbs)
        | EnumCrumb :: bs -> (children, crumbs)

    match words with
    | "message" :: xs -> 
        let children, crumbs = (addProto (List.head xs) proto.Children breadcrumbs)
        ({proto with Children = children}, crumbs)
    | "}" :: xs -> (proto, List.tail breadcrumbs)
    | _ -> (proto, breadcrumbs)

let parseProtoFile (filePath : string) (fileLines : seq<string>) =
   let proto = {FilePath = filePath; Children = []; Fields = []; Enums = []} : ProtoDescriptor
   let breadcrumbs = [] : ProtoCrumbs
   Seq.fold parseProtoLine (proto, breadcrumbs) fileLines