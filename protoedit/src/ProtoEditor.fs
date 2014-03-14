module ProtoEditor

open System.Windows.Controls
open ProtoDescriptor

type ProtoEditor(protoDescriptor : MessageDescriptor) =
    let _protoDescriptor = protoDescriptor
    let _treeView = new TreeView()

    let addSubItems items (branch:TreeViewItem) = items |> Seq.iter (fun item -> item |> branch.Items.Add |> ignore)

    let addFields (fields : FieldDescriptor list) (items : ItemCollection) =
        let addFieldBranch (field : FieldDescriptor) =
            let branch = new TreeViewItem(Header=field.Name + " : " + field.Type.ToString())
            items.Add branch
        List.map addFieldBranch fields |> ignore

    let addMessages (proto : MessageDescriptor list) (items : ItemCollection) =
        let addMessageBranch message =
            let branch = new TreeViewItem(Header=message.Name)
            addFields message.Fields branch.Items
            items.Add branch
        List.map addMessageBranch proto |> ignore
        
    do
        _treeView.Name = "protoTreeView" |> ignore
        addMessages _protoDescriptor.Messages _treeView.Items

    member this.treeView = _treeView