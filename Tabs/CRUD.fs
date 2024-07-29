module Tabs.CRUD

open FSharpQt
open BuilderNode
open FSharpQt.MiscTypes
open Reactor

open FSharpQt.Widgets
open Label
open BoxLayout
open GridLayout
open PushButton
open LineEdit
open TreeView

open FSharpQt.Models
open ListModelNode
open SortFilterProxyModel
open TrackedRows

type Signal = unit

type Name = {
    First: string
    Last: string
}

type State = {
    Names: TrackedRows<Name>
    FilterPattern: string
    SelectedIndex: int option
    FirstEdit: string
    LastEdit: string
}

type Msg =
    | SetFilter of filter: string
    | SelectRawIndex of modelIndex: ModelIndexProxy
    | SelectActualIndex of index: int
    | SetFirst of string
    | SetLast of string
    | Create
    | Update
    | Delete

let init () =
    let state = {
        Names = TrackedRows.Init([
            { First = "Hans"; Last = "Emil" }
            { First = "Max"; Last = "MusterMann" }
            { First = "Roman"; Last = "Tisch" }             
        ])
        FilterPattern = ""
        SelectedIndex = None
        FirstEdit = ""
        LastEdit = ""
    }
    state, Cmd.None
    
let update (state: State) (msg: Msg) =
    match msg with
    | SetFilter filter ->
        let nextState =
            { state with
                FilterPattern = filter
                FirstEdit = ""
                LastEdit = "" }
        nextState, Cmd.None
    | SelectRawIndex index ->
        // this is the raw proxied index - need to convert to the actual source index
        let cmd =
            Cmd.ViewExec (fun bindings ->
                viewexec bindings {
                    let! proxyModel = SortFilterProxyModel.bindNode "proxymodel"
                    // note 'converted' would be safely GC'ed even if we didn't know it was disposable
                    use converted =
                        proxyModel.MapToSource(index)
                    if converted.IsValid then
                        // emit a new message to actually update state
                        return SelectActualIndex converted.Row
                })
        state, cmd
    | SelectActualIndex index ->
        let nextSelectedIndex, nextFirstEdit, nextLastEdit =
            state.Names.Rows
            |> List.item index
            |> (fun name -> Some index, name.First, name.Last)
        let nextState =
            { state with
                FirstEdit = nextFirstEdit
                LastEdit = nextLastEdit
                SelectedIndex = nextSelectedIndex }
        nextState, Cmd.None
    | SetFirst text ->
        { state with FirstEdit = text }, Cmd.None
    | SetLast text ->
        { state with LastEdit = text }, Cmd.None
    | Create ->
        let nextState =
            if state.FirstEdit.Length > 0 && state.LastEdit.Length > 0 then
                let name = { First = state.FirstEdit; Last = state.LastEdit }
                let nextNames =
                    state.Names
                        .BeginChanges()
                        .AddRow(name)
                { state with
                    Names = nextNames
                    FirstEdit = ""
                    LastEdit = ""
                    SelectedIndex = None }
            else
                state
        nextState, Cmd.None
    | Update ->
        let nextState =
            match state.SelectedIndex, state.FirstEdit.Length, state.LastEdit.Length with
            | Some index, firstLen, lastLen when firstLen > 0 && lastLen > 0 ->
                if index < state.Names.RowCount then
                    let nextNames =
                        state.Names
                            .BeginChanges()
                            .ReplaceAtIndex(index, { First = state.FirstEdit; Last = state.LastEdit })
                    { state with Names = nextNames; FirstEdit = ""; LastEdit = ""; SelectedIndex = None }
                else
                    state
            | _ ->
                state
        nextState, Cmd.None
    | Delete ->
        let nextState =
            match state.SelectedIndex with
            | Some index ->
                if index < state.Names.RowCount then
                    let nextNames =
                        state.Names
                            .BeginChanges()
                            .DeleteRow(index)
                    { state with Names = nextNames; FirstEdit = ""; LastEdit = ""; SelectedIndex = None }
                else
                    state
            | None ->
                state
        nextState, Cmd.None
        
let view (state: State) =
    let filterLabel = Label(Text = "Filter:")
    let filterEdit =
        LineEdit(
            Text = state.FilterPattern,
            ClearButtonEnabled = true,
            OnTextChanged = SetFilter)

    let model =
        let dataFunc row col role =
            match col, role with
            | 0, DisplayRole -> Variant.String row.Last
            | 1, DisplayRole -> Variant.String row.First
            | _ -> Variant.Empty
        ListModelNode(dataFunc, 2,
                      Rows = state.Names,
                      Headers = [ "Last"; "First" ])
        
    let filterModel =
        let regex =
            match state.FilterPattern with
            | "" -> Regex()
            | value -> Regex(value, [ RegexOption.CaseInsensitive ])
        SortFilterProxyModel(
            Name = "proxymodel",            // for method invocation in 'update'
            FilterRegularExpression = regex,
            FilterKeyColumn = None,
            SourceModel = model)
        
    let treeView =
        TreeView(SortingEnabled = true, TreeModel = filterModel, OnClicked = SelectRawIndex)

    let firstLabel = Label(Text = "First:")
    let firstEdit = LineEdit(Text = state.FirstEdit, OnTextChanged = SetFirst)

    let lastLabel = Label(Text = "Last:")
    let lastEdit = LineEdit(Text = state.LastEdit, OnTextChanged = SetLast)

    let createButton =
        let enabled =
            state.FirstEdit.Length > 0 && state.LastEdit.Length > 0
        PushButton(Text = "Create", Enabled = enabled, OnClicked = Create)

    let updateButton =
        let enabled =
            match state.SelectedIndex, state.FirstEdit.Length, state.LastEdit.Length with
            | Some _, firstLen, lastLen when firstLen > 0 && lastLen > 0 -> true
            | _ -> false
        PushButton(Text = "Update", Enabled = enabled, OnClicked = Update)
        
    let deleteButton =
        let enabled =
            match state.SelectedIndex with
            | Some _ -> true
            | None -> false
        PushButton(Text = "Delete", Enabled = enabled, OnClicked = Delete)
    
    GridLayout(
        ColumnConfigs = [
            ColConfig(3, minWidth = 120)
        ],
        Items = [
            GridItem(filterLabel, 0, 0)
            GridItem(filterEdit, 0, 1)
            GridItem(treeView, 1, 0, 4, 2)
            GridItem(firstLabel, 1, 2, align = Alignment.Right)
            GridItem(firstEdit, 1, 3)
            GridItem(lastLabel, 2, 2, align = Alignment.Right)
            GridItem(lastEdit, 2, 3)
            // buttons:
            let hbox =
                HBoxLayout(
                    ContentsMargins = (0, 0, 0, 0),
                    Items = [
                        BoxItem(createButton, stretch = 1)
                        BoxItem(updateButton, stretch = 1)
                        BoxItem(deleteButton, stretch = 1)
                        BoxItem(stretch = 1)
                    ])
            GridItem(hbox, 5, 0, 1, 4)
        ])
    :> ILayoutNode<Msg>

type CRUDPage<'outerMsg>() =
    inherit LayoutReactorNode<'outerMsg, State, Msg, Signal>(init, update, view)
