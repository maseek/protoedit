module ParseIn

open ProtoDescriptor

let parseProtoLine ((proto, breadcrumbs) : (ProtoZipper)) (line : string) : ProtoZipper =
    let words = line.Split [|' '|] |> Array.toList |> List.filter (fun x -> if x.Equals("") then false else true)

    let rec addProto name proto crumbs : ProtoZipper =
        match crumbs with
        | [] -> ({proto with Name = name}, [ProtoCrumb])
        | [b] -> ({proto with Children = {Name = name; Children = []; Fields = []; Enums = []} :: proto.Children}, ProtoCrumb :: crumbs)
        | b :: bx -> ({proto with Children = (addProto name (List.head proto.Children) bx |> fst) :: List.tail proto.Children}, ProtoCrumb :: crumbs)

    match words with
    | "message" :: xs -> addProto (List.head xs) proto breadcrumbs
    | "}" :: xs -> (proto, List.tail breadcrumbs)
    | _ -> (proto, breadcrumbs)

let parseProtoFile (fileLines : seq<string>) =
   let proto = {Name = ""; Children = []; Fields = []; Enums = []} : ProtoDescriptor
   let breadcrumbs = [] : ProtoCrumbs
   Seq.fold parseProtoLine (proto, breadcrumbs) fileLines