module ProtoEditor

open System.Windows
open System.Windows.Controls
open ProtoDescriptor

type ProtoEditor(protoDescriptor : MessageDescriptor, view : Panel) =
    let _protoDescriptor = protoDescriptor
    let _view = view
    let _protoTreeView = new TreeView()
    let _fieldDataGrid = new DataGrid()

    let addSubItems items (branch:TreeViewItem) = items |> Seq.iter (fun item -> item |> branch.Items.Add |> ignore)

    //TODO
    let editField field =
        let h1 = new DataGridTextColumn()
        _fieldDataGrid.Columns.Add(h1)
        _fieldDataGrid.Visibility = Visibility.Visible |> ignore

    let addFields (fields : FieldDescriptor list) (items : ItemCollection) =
        let addFieldBranch (field : FieldDescriptor) =
            let branch = new TreeViewItem(Header=field.Name + " : " + show field.Type)
            branch.GotFocus.Add(fun (e : RoutedEventArgs) -> editField field)
            branch.ExpandSubtree() |> ignore
            items.Add branch
        List.map addFieldBranch (List.sort fields) |> ignore

    let addMessages (proto : MessageDescriptor list) (items : ItemCollection) =
        let addMessageBranch message =
            let branch = new TreeViewItem(Header=message.Name)
            addFields message.Fields branch.Items
            branch.ExpandSubtree() |> ignore
            items.Add branch
        List.map addMessageBranch proto |> ignore
        
    do
        _protoTreeView.Name = "protoTreeView" |> ignore
        addMessages _protoDescriptor.Messages _protoTreeView.Items
        _fieldDataGrid.Visibility = Visibility.Hidden |> ignore
        _view.Children.Clear()
        _view.Children.Add(_protoTreeView) |> ignore
        _view.Children.Add(_fieldDataGrid) |> ignore