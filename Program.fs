module Program

open System

open FSharpQt
open BuilderNode
open Reactor
open FSharpQt.Widgets
open BoxLayout
open GroupBox
open PushButton
open MainWindow
open WindowSet

open Tabs.Counter
open Tabs.DropTesting
open Tabs.PathStroking.PathStroking
open Tabs.TempConverter
open Tabs.FlightBooker
open Tabs.TimerPage
open Tabs.CRUD
open Tabs.CircleDrawer

[<RequireQualifiedAccess>]
type GuiKind =
    // 7guis
    | Counter
    | TempConverter
    | FlightBooker
    | TimerPage
    | CRUD
    | CircleDrawer
    | Spreadsheet
    // misc
    | DropTesting
    | PathStroking
    
type GuiInstance = {
    Key: int
    Kind: GuiKind
}

type State = {
    NextKey: int
    Instances: GuiInstance list
}

type Msg =
    | MainWindowClosed
    | LaunchInstance of kind: GuiKind
    | InstanceClosed of key: int

let init () =
    let nextState = {
        NextKey = 1
        Instances = []
    }
    nextState, Cmd.None

let update (state: State) (msg: Msg) =
    match msg with
    | MainWindowClosed ->
        state, Cmd.Signal QuitApplication
    | LaunchInstance kind ->
        let nextState =
            let nextInstances =
                let instance =
                    { Key = state.NextKey; Kind = kind }
                instance :: state.Instances
            { state with Instances = nextInstances; NextKey = state.NextKey + 1 }
        nextState, Cmd.None
    | InstanceClosed key ->
        let nextState =
            let nextInstances =
                let index =
                    state.Instances
                    |> List.findIndex (fun inst -> inst.Key = key)
                state.Instances
                |> List.removeAt index
            { state with Instances = nextInstances }
        nextState, Cmd.None
    
let view (state: State) =
    let topGroup =
        let buttons =
            [ "Counter", GuiKind.Counter
              "TempConv", GuiKind.TempConverter
              "FlightBooker", GuiKind.FlightBooker
              "Timer", GuiKind.TimerPage
              "CRUD", GuiKind.CRUD
              "CircleDrawer", GuiKind.CircleDrawer
              "Spreadsheet", GuiKind.Spreadsheet ]
            |> List.map (fun (name, kind) ->
                let enabled =
                    match kind with
                    | GuiKind.Spreadsheet -> false
                    | _ -> true
                PushButton(Text = name, Enabled = enabled, OnClicked = LaunchInstance kind))
        let items =
            buttons
            |> List.map BoxItem
        let vbox =
            VBoxLayout(Items = items)
        GroupBox(Title = "7GUIs", Layout = vbox)
        
    let bottomGroup =
        let buttons =
            [ "DropTesting", GuiKind.DropTesting
              "PathStroking", GuiKind.PathStroking ]
            |> List.map (fun (name, kind) ->
                PushButton(Text = name, OnClicked = LaunchInstance kind))
        let items =
            buttons
            |> List.map BoxItem
        let vbox =
            VBoxLayout(Items = items)
        GroupBox(Title = "Misc", Layout = vbox)
        
    let vbox =
        let items = [
            // listed explicitly (vs. List.map) to avoid casting to :> IWidgetNode above
            BoxItem(topGroup)
            BoxItem(bottomGroup)
            BoxItem(stretch = 1)
        ]
        VBoxLayout(Items = items)
        
    let mainWindow =
        let window =
            MainWindow(WindowTitle = "7GUIs in F#/Qt", CentralLayout = vbox, OnWindowClosed = MainWindowClosed)
        IntKey 0, window :> IWindowNode<Msg>
        
    let instanceWindows =
        state.Instances
        |> List.map (fun inst ->
            let title, node =
                match inst.Kind with
                | GuiKind.Counter -> "Counter", Counter() :> ILayoutNode<Msg>
                | GuiKind.TempConverter -> "Temperature Converter", TempConverter()
                | GuiKind.FlightBooker -> "Flight Booker", FlightBooker()
                | GuiKind.TimerPage -> "Timer", TimerPage()
                | GuiKind.CRUD -> "CRUD", CRUDPage()
                | GuiKind.CircleDrawer -> "Circle Drawer", CircleDrawer()
                | GuiKind.Spreadsheet -> failwith "not yet implemented"
                | GuiKind.DropTesting -> "Drop Testing", DropTesting()
                | GuiKind.PathStroking -> "Path Stroking", PathStroking()
            let window =
                MainWindow(WindowTitle = title, CentralLayout = node, OnWindowClosed = InstanceClosed inst.Key)
            IntKey inst.Key, window :> IWindowNode<Msg>)
        
    WindowSet(Windows = mainWindow :: instanceWindows)
    :> IBuilderNode<Msg>
    
[<EntryPoint>]
[<STAThread>]
let main argv =
    use app =
        createApplication init update view
    // app.SetStyle(Fusion)
    app.Run argv
