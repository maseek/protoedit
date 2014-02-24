module ParseIn

open ProtoDescriptor

let parseProtoLine ((proto, breadcrumbs) : (ProtoZipper)) (line : string) =
    let words = line.Split [|' '|] |> Array.toList

    let addProto name proto crumbs : ProtoZipper =
        match crumbs with
        | [] -> ({proto with Name = name}, (name |> ProtoCrumb) :: crumbs)
        | [b] -> ({proto with Children = {Name = name; Children = []; Fields = []; Enums = []} :: proto.Children}, (name |> ProtoCrumb) :: crumbs)
        | _ :: _ -> (proto, crumbs)
        //| b :: bx -> ({proto with Children = (addProto name proto.Children bx |> fst) :: proto.Children}, bx)

    match words with
    | "message" :: xs -> addProto (List.head xs) proto breadcrumbs
    | _ -> (proto, breadcrumbs)

let parseProtoFile (fileLines : seq<string>) =
   let proto = {Name = ""; Children = []; Fields = []; Enums = []} : ProtoDescriptor
   let breadcrumbs = [] : ProtoCrumbs
   Seq.fold parseProtoLine (proto, breadcrumbs) fileLines