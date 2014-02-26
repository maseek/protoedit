module MainApp

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Media
open FSharpx
open ParseIn

type MainWindow = XAML<"src/MainWindow.xaml">

let readBinaryFile (filePath:string) = System.IO.File.ReadAllBytes(filePath)
let readFileLines (filePath:string) = System.IO.File.ReadLines(filePath)

type App(window : MainWindow) = 
    let _window = window
    let _protoInput : TextBox = _window.Root.FindName "protoInput" |> unbox
    let _protoBrowse : Button = _window.Root.FindName "protoBrowse" |> unbox
    let _dataInput : TextBox = _window.Root.FindName "dataInput" |> unbox
    let _dataBrowse : Button = _window.Root.FindName "dataBrowse" |> unbox
    let _dataNew : Button = _window.Root.FindName "dataNew" |> unbox

    member this.window
        with get () = _window

    member this.previewDragHandler (e : DragEventArgs) =
        e.Effects <- DragDropEffects.Link
        e.Handled <- true

    member this.previewDropHandler (e : DragEventArgs) =
        let filePath : string [] = e.Data.GetData DataFormats.FileDrop |> unbox
        let source : TextBox = e.Source |> unbox
        source.Text <- String.Format("{0}", filePath.[0])

    member this.protoBrowseHandler (e : RoutedEventArgs) =
        let openFileDialog = new Forms.OpenFileDialog()
        openFileDialog.Filter <- "Proto files (*.proto)|*.proto|All Files (*.*)|*.*"
        let result = openFileDialog.ShowDialog ()
        if result.Equals(Forms.DialogResult.OK) 
            then
                let filePath = openFileDialog.FileName
                let fileLines = readFileLines filePath
                let protoDescriptor = parseProtoFile filePath fileLines
                _protoInput.Text <- filePath

    member this.dataBrowseHandler (e : RoutedEventArgs) =
        let openFileDialog = new Forms.OpenFileDialog()
        openFileDialog.Filter <- "All Files (*.*)|*.*"
        let result = openFileDialog.ShowDialog ()
        if result.Equals(Forms.DialogResult.OK) 
            then _dataInput.Text <- openFileDialog.FileName

    member this.dataNewHandler (e : RoutedEventArgs) =
        let saveFileDialog = new Forms.SaveFileDialog()
        saveFileDialog.Filter <- "Binary file (*.bin)|*.bin"
        saveFileDialog.FileName <- "data"
        saveFileDialog.DefaultExt <- ".bin"
        let result = saveFileDialog.ShowDialog ()
        if result.Equals(Forms.DialogResult.OK)
            then _dataInput.Text <- saveFileDialog.FileName

    member this.Init () =
        _protoInput.PreviewDragOver.Add(this.previewDragHandler)
        _protoInput.PreviewDrop.Add(this.previewDropHandler)
        _protoBrowse.Click.Add(this.protoBrowseHandler)
        _dataInput.PreviewDragOver.Add(this.previewDragHandler)
        _dataInput.PreviewDrop.Add(this.previewDropHandler) 
        _dataBrowse.Click.Add(this.dataBrowseHandler)
        _dataNew.Click.Add(this.dataNewHandler)

    new() = App(new MainWindow())

let loadWindow() =
    let app = App()
    app.Init()
    app.window.Root

[<STAThread>]
(new Application()).Run(loadWindow()) |> ignore