module ProtoTreeView

open System.Windows.Controls
open ProtoDescriptor

type ProtoTreeView(protoDescriptor : MessageDescriptor) =
    let _protoDescriptor = protoDescriptor
    let _treeView = new TreeView()

    let addSubItems items (branch:TreeViewItem) = items |> Seq.iter (fun item -> 
     item |> branch.Items.Add |> ignore)

    //TODO: replace test data with _protoDescriptor data
    do
        _treeView.Items.Add
            (let animalBranch = new TreeViewItem(Header="Languages")
  
             animalBranch.Items.Add
                (let branch = new TreeViewItem(Header=".NET version")
                 let dogs = [".NET3.0";".NET3.5";".NET 4.0"]
                 addSubItems dogs branch
                 branch) |>ignore
  
             animalBranch.Items.Add
                (let branch = new TreeViewItem(Header=".NET Languages")
                 branch.Items.Add(new TreeViewItem(Header="ASP.NET")) |>ignore
                 branch.Items.Add(new Button(Content="VB.NET")) |>ignore
                 branch.Items.Add(".NET") |>ignore
                 branch) |> ignore
  
             animalBranch.Items.Add
                (let branch = new TreeViewItem(Header=".NET Code Behind")
                 let primates = ["C#";"Visual Basic";"F#"]
                 addSubItems primates branch
                 branch) |> ignore
             animalBranch) |> ignore
       
        _treeView.Items.Add
            (let branch = new TreeViewItem(Header="Books")
             let minerals = ["F# Books";"WPF Books";"FrameWork Books"]
             addSubItems minerals branch
             branch) |> ignore
  
        _treeView.Items.Add
            (let branch = new TreeViewItem(Header="WebSites")
             let vegetables = ["c-sharpcorner.com";"vbdotnetheaven";"Mindcracker.com"]
             addSubItems vegetables branch
             branch) |> ignore

    member this.treeView = _treeView