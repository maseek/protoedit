module MainApp

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open FSharpx

type MainWindow = XAML<"src/MainWindow.xaml">

let previewDrag (e : DragEventArgs) =
    e.Effects <- DragDropEffects.Link
    e.Handled <- true

let previewDrop (e : DragEventArgs) =
    let filePath : string [] = e.Data.GetData DataFormats.FileDrop |> unbox
    let source : TextBox = e.Source |> unbox
    source.Text <- String.Format("{0}", filePath.[0])

let loadWindow() =
    let window = MainWindow()
    let protoInput : TextBox = window.Root.FindName "protoInput" |> unbox
    protoInput.PreviewDragOver.Add(previewDrag)
    protoInput.PreviewDrop.Add(previewDrop)
    let dataInput : TextBox = window.Root.FindName "dataInput" |> unbox
    dataInput.PreviewDragOver.Add(previewDrag)
    dataInput.PreviewDrop.Add(previewDrop)
    window.Root


[<STAThread>]
(new Application()).Run(loadWindow()) |> ignore