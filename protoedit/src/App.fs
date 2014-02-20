module MainApp

open System
open System.Windows
open System.Windows.Controls
open FSharpx

type MainWindow = XAML<"src/MainWindow.xaml">

let loadWindow() =
   let window = MainWindow()
   
   window.Root

[<STAThread>]
(new Application()).Run(loadWindow()) |> ignore