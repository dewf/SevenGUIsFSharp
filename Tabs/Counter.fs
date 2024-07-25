module Tabs.Counter

open FSharpQt
open BuilderNode
open FSharpQt.MiscTypes
open Reactor
open FSharpQt.Widgets
open BoxLayout
open Label
open PushButton

type Attr = unit
type Signal = unit

type State = {
    Count: int
}

type Msg =
    | Increment

let init () =
    { Count = 0 }, Cmd.None
    
let update (state: State) (msg: Msg) =
    match msg with
    | Increment ->
        { state with Count = state.Count + 1 }, Cmd.None
        
let view (state: State) =
    let label =
        Label(Text = $"Count: {state.Count}", Alignment = Center)
    let button =
        PushButton(Text = "Increment", OnClicked = Increment)
    HBoxLayout(
        Items = [
            BoxItem(label)
            BoxItem(button)
        ])
    :> ILayoutNode<Msg>

type Counter<'outerMsg>() =
    inherit LayoutReactorNode<'outerMsg, State, Msg, Signal>(init, update, view)
