﻿[<JS>]
module WebSharper.KendoWrapper

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.Formlet

type K = KendoWrapper
type UI = K.kendo.ui

module Option =
    let conditional f = function
        | x when f x -> Some x
        | _ -> None

module Menu =
    type Item<'T> =
        | Selection of string * 'T
        | Choices of string * Item<'T> list

    let selection label value = Selection(label, value)
    let choices label items = Choices(label, items)
    
    let private setTag v (el: Element) = el.Body?ValueTag <- v; el

    let rec private build = function
        | Selection(label, v) ->
            LI [Text label] |> setTag (Some v)
        | Choices(label, items) ->
            let items = List.map build items
            LI [Text label] -- UL items
            |> setTag None

    let create (f: 'a -> unit) (structure: Item<'a> list) =
        let el = List.map build structure |> UL
        let configuration =
            UI.MenuOptions(_select = fun x ->
                x.item?ValueTag
                |> Option.iter f
            )
        UI.Menu.Create(As el.Dom, configuration) |> ignore
        el

module DropDown =
    type DropDownValue<'t> =
        {
            text: string
            value: 't
        }
    let internal configure current choices =
        let values =
            choices
            |> List.map (fun (k, v) -> { text = k; value = v })
            |> List.toArray

        match current with
        | None ->
            UI.DropDownListOptions(dataTextField = "text", dataValueField = "value", dataSource = values)
        | Some v ->
            let i = List.findIndex (fun (_, x) -> x = v) choices
            UI.DropDownListOptions(dataTextField = "text", dataValueField = "value", dataSource = values, index = float i)

    let create current choices =
        Input []
        |>! OnAfterRender (fun input ->
            UI.DropDownList.Create(As input.Body, configure (Some current) choices) |> ignore
        )

module Tabs =
    type T = { Name: string; Content: unit -> Element }

    let create name content = { Name = name; Content = content }

    let render = function
        | [] -> failwith "The list must contain at least one tab."
        | head::tail ->
            let li tab = LI [Text tab.Name]

            let headTab = li head |+ "k-state-active"
            let headContent = head.Content()

            let tabs, contents =
                tail
                |> List.map (fun tab ->
                    let t, body = li tab, Div[]
                    let content = lazy (body -- tab.Content() |> ignore)
                    t |> OnClick (fun _ _ -> content.Force())
                    t, body
                )
                |> List.unzip

            Div [
                yield headTab :: tabs |> UL
                yield! headContent :: contents
            ]
            |>! fun el ->
                let animation = UI.TabStripAnimation(close = As false, _open = As false)
                let option = UI.TabStripOptions(animation = animation)
                UI.TabStrip.Create(As el.Body, option)
                |> ignore

module Popup =
    type Window = UI.Window
    type T =
        {
            Content: ((Window.T -> unit) -> unit) -> Element
            Title: string
            Width: int
            Draggable: bool
            Modal: bool
            Resizable: bool
        }

    let frozen p = { p with Draggable = false }
    let locked p = { p with Resizable = false }
    let withoutOverlay p = { p with Modal = false }

    let close (w: Window.T) = w.destroy()
    
    let private render popup =
        let window: Window.T option ref = ref None
        let actOn f = !window |> Option.iter f
        
        let config =
            UI.WindowOptions(
                title = popup.Title,
                width = string popup.Width + "px",
                close = (fun _ -> actOn close),
                actions = [|"Close"|],
                animation = As false,
                draggable = popup.Draggable,
                resizable = popup.Resizable,
                modal = popup.Modal
            )
        
        window := Some (Window.Create(As (popup.Content actOn).Body, config))

        actOn (fun w ->
            w.bind("activate", As w.center) |> ignore
            w._open() |> ignore
        )

    let create title settings contentF =
        let applySettings = List.fold (>>) id settings
        {
            Content = contentF
            Title = title
            Width = 500
            Draggable = true
            Modal = true
            Resizable = true
        }
        |> applySettings
        |> render

module SaveActions =
    type T<'V> =
        {
            Changed: 'V [] -> unit
            Deleted: 'V [] -> unit
            Added: 'V [] -> unit
        }

    let internal zero = { Changed = ignore; Deleted = ignore; Added = ignore }

    let private (!) f = Option.conditional (Array.isEmpty >> not) >> Option.iter f

    let onChange f actions = { actions with Changed = !f }
    let onDelete f actions = { actions with Deleted = !f }
    let onAdd f actions = { actions with Added = !f }

module Schema =
    type Field = { editable: bool; ``type``: string }
    type Type = String | Number | Date | Bool
    type T = { Editable: bool option; Type: Type }

    let zero = { Editable = None; Type = String }
    let readonly schema = { schema with Editable = Some false }
    let editable schema = { schema with Editable = Some true }
    let typed typ schema = { schema with Type = typ }

    let create defaultEdit (schemas: (string * T) seq) =
        schemas
        |> Seq.fold (fun schema (fieldName, { Editable = editable; Type = typ } ) ->
            let typ =
                match typ with
                | String -> "string"
                | Number -> "number"
                | Date -> "date"
                | Bool -> "boolean"
            let fieldType = { editable = defaultArg editable defaultEdit; ``type`` = typ }
            (?<-) schema fieldName fieldType
            schema
        ) (obj())

module Column =
    type Field<'V> =
        {
            Field: string
            Format: string option
            Template: ('V -> string) option
            Editor: (string * string) list
            Schema: Schema.T
        }

    type Content<'V> =
        | Field of Field<'V>
        | CommandButton of string * ('V -> unit)

    type Attribute = { ``class``: string }

    type T<'V> =
        {
            Title: string
            Width: int option
            Attributes: Attribute option
            Content: Content<'V>
        }

    let private create title content =
        {
            Title = title
            Width = None
            Attributes = None
            Content = content
        }

    let field name title =
        Field {
            Field = name
            Format = None
            Template = None
            Editor = []
            Schema = Schema.zero
        }
        |> create title

    let command label action =
        CommandButton (label, action)
        |> create ""

    let delete() = command "destroy" ignore

    let private mapContent f col =
        let content = 
            match col.Content with
            | Field c -> Field (f c)
            | CommandButton _ -> col.Content
        { col with Content = content }

    let editor name title choices =
        field name title
        |> mapContent (fun c -> { c with Editor = choices })

    let width width col = { col with Width = Some width }
    let withClass className col = { col with Attributes = Some ({ ``class`` = className }) }
    
    let private formatWithf templateFunc = mapContent (fun c -> { c with Template = Some (templateFunc c) })
    let formatWith templateFunc = mapContent (fun c -> { c with Template = Some templateFunc })
    let shortDateFormat x = formatWithf (fun f v -> K.kendo.toString(As<TypeScript.Lib.Date>((?) v f.Field), "d")) x
    let longDateFormat x = formatWithf (fun f v -> K.kendo.toString(As<TypeScript.Lib.Date>((?) v f.Field), "D")) x

    let formatField fmt = mapContent (fun c -> { c with Format = Some fmt })
    let rightAligned x = withClass "rightAligned" x
    let centered x = withClass "centered" x
    let currencyFormat x = rightAligned x |> formatField "{0:c}"
    let percentFormat precision x = rightAligned x |> formatWithf (fun f v -> K.kendo.toString(((?) v f.Field: float), "p" + string precision))

    let applySchema f = mapContent (fun c -> { c with Schema = f c.Schema })
    let editable c = applySchema Schema.editable c
    let readonly c = applySchema Schema.readonly c
    let typed typ = applySchema (Schema.typed typ)
    let numeric title name = field title name |> rightAligned |> typed Schema.Number
    let date title name = field title name |> typed Schema.Date
    let bool title name = field title name |> typed Schema.Bool

    let fromMapping (onGrid: (UI.Grid.T -> _) -> _) col =
        let column =
            match col.Content with
            | Field f ->
                UI.GridColumn(field = f.Field)
                |>! fun column ->
                    Option.iter (fun f -> column.format <- f) f.Format
                    Option.iter (fun t -> column.template <- t) f.Template
                    match f.Editor with
                    | [] -> ()
                    | choices ->
                        column.editor <- fun (container, options) ->
                            let format = "<input data-bind='value:" + options?field + "'/>"
                            let target = JQuery.JQuery.Of(format).AppendTo(As<JQuery.JQuery> container)
                            UI.DropDownList.Create(As target, DropDown.configure None choices)
                            |> ignore
            | CommandButton (text, action) ->
                let command =
                    UI.GridColumnCommandItem(name = text, click = As (fun e ->
                            onGrid (fun grid ->
                                JQuery.JQuery.Of(e?currentTarget: Dom.Node).Closest("TR").Get 0
                                |> As<TypeScript.Lib.Element>
                                |> grid.dataItem
                                |> As
                                |> action
                            )
                        )
                    )
                UI.GridColumn(command = [|command|])

        column.title <- col.Title
        Option.iter (fun a -> column.attributes <- a) col.Attributes
        Option.iter (fun w -> column.width <- w) col.Width

        column

module Grid =
    type Selectable<'V> =
        | Row of ('V -> unit)
        | Cell of ('V -> unit)

    type Paging =
        | Paging of int
        | Sizer of int

    type ToolButton<'V> =
        | Create
        | Cancel
        | Save of SaveActions.T<'V>
        | Label of string

    type Config<'V> =
        {
            Paging: Paging option
            Columns: Column.T<'V> seq
            Selectable: Selectable<'V> option
            Scrollable: bool
            Sortable: bool
            Resizable: bool
            Reorderable: bool
            Filterable: bool
            Groupable: bool
            Editable: bool
        }

    type T<'V> =
        | Plain of Config<'V>
        | WithToolbar of Config<'V> * ToolButton<'V> list

    let defaultPaging = Some (Paging 10)
    let pageSize = function
        | None -> 0
        | Some (Paging x | Sizer x) -> x

    let Default columns =
        Plain {
            Columns = columns
            Paging = defaultPaging
            Selectable = None
            Scrollable = true
            Sortable = false
            Resizable = false
            Reorderable = false
            Filterable = false
            Groupable = false
            Editable = false
        }

    let onConfig f = function
        | Plain gridConfig -> Plain (f gridConfig)
        | WithToolbar (gridConfig, toolButtons) -> WithToolbar (f gridConfig, toolButtons)

    let getConfiguration = function
        | Plain gridConfig -> gridConfig
        | WithToolbar (gridConfig, _) -> gridConfig

    let sortable gridConfig = onConfig (fun gridConfig -> { gridConfig with Sortable = true }) gridConfig
    let nonScrollable gridConfig = onConfig (fun gridConfig -> { gridConfig with Scrollable = false }) gridConfig
    let groupable gridConfig = onConfig (fun gridConfig -> { gridConfig with Groupable = true }) gridConfig
    let filterable gridConfig = onConfig (fun gridConfig -> { gridConfig with Filterable = true }) gridConfig
    let reorderable gridConfig = onConfig (fun gridConfig -> { gridConfig with Reorderable = true }) gridConfig
    let resizableColumn gridConfig = onConfig (fun gridConfig -> { gridConfig with Resizable = true }) gridConfig
    let withoutPaging gridConfig = onConfig (fun gridConfig -> { gridConfig with Paging = None }) gridConfig
    let editable gridConfig = onConfig (fun gridConfig -> { gridConfig with Editable = true }) gridConfig
    let selectableRow action = onConfig (fun gridConfig -> { gridConfig with Selectable = Some (Row action) })
    let selectableCell action = onConfig (fun gridConfig -> { gridConfig with Selectable = Some (Cell action) })

    let private withToolbarButton kind = function
        | Plain gridConfig -> WithToolbar (gridConfig, [kind])
        | WithToolbar (gridConfig, buttons) ->
            let buttons =
                kind :: buttons
                |> Seq.distinctBy (function Cancel -> 0 | Create -> 1 | Save _ -> 2 | Label _ -> 3)
                |> Seq.toList
            WithToolbar (gridConfig, buttons)
    
    let addToolbarTemplate (el: Element) gridConfig = withToolbarButton (Label el.Html) gridConfig
    let addButton gridConfig = withToolbarButton Create gridConfig
    let cancelButton gridConfig = withToolbarButton Cancel gridConfig
    let saveButton configurator = withToolbarButton (Save (configurator SaveActions.zero))

    let paging x =
        onConfig (fun gridConfig ->
            {
                gridConfig with
                    Paging =
                        match x, gridConfig.Paging with
                        | 0, _ -> None
                        | x, (None | Some (Paging _)) -> Some (Paging x)
                        | x, Some (Sizer _) -> Some (Sizer x)
            }
        )

    let adjustablePaging gridConfig =
        onConfig (fun gridConfig ->
            {
                gridConfig with
                    Paging =
                        gridConfig.Paging
                        |> Option.coalesce defaultPaging
                        |> Option.map (fun (Paging x | Sizer x) -> Sizer x)
            }
        ) gridConfig

    let buildConfig (onGrid: (UI.Grid.T -> _) -> _) configuration dataSource =
        let config = getConfiguration configuration
        let columns =
            config.Columns
            |> Seq.map (Column.fromMapping onGrid)
            |> Seq.toArray

        let gconf =
            UI.GridOptions (
                columns = columns,
                scrollable = As config.Scrollable,
                sortable = As config.Sortable,
                dataSource = dataSource,
                resizable = config.Resizable,
                filterable = As config.Filterable,
                reorderable = config.Reorderable,
                editable = UI.GridEditable(confirmation = false),
                groupable = As config.Groupable
            )

        config.Paging
        |> Option.iter (function
            | Paging x -> gconf.pageable <- As true
            | Sizer x ->
                gconf.pageable <- UI.GridPageable(pageSizes = [|5; 10; 20; 30; 50; 75; 100|])
        )

        config.Selectable
        |> Option.iter (fun selectable ->
            let action =
                match selectable with
                | Row action -> gconf.selectable <- "row"; action
                | Cell action -> gconf.selectable <- "cell"; action
            gconf.change <- fun _ -> onGrid (fun grid -> grid._select() |> grid.dataItem |> As |> action)
        )

        match configuration with
        | Plain _ | WithToolbar (_, []) -> ()
        | WithToolbar (_, buttons) ->
            let (!) label = UI.GridToolbarItem(name = label)
            gconf.toolbar <-
                buttons
                |> List.map (function
                    | Create -> !"create"
                    | Cancel -> !"cancel"
                    | Save _ -> !"save"
                    | Label markup -> UI.GridToolbarItem(template = markup)
                )
                |> List.toArray

        gconf

    let missingFrom second =
        let originalKeys =
            second
            |> Array.map (fun x -> x?uid)
            |> Set.ofArray

        Array.filter (fun x -> originalKeys.Contains x?uid |> not)

    let applyToolButtons (onGrid: (UI.Grid.T -> _) -> _) =
        let sourceData = ref [||]
        onGrid(fun grid -> sourceData := grid.dataSource.data() |> As |> Array.copy)

        Seq.tryPick (function
            | Save x -> Some x
            | _ -> None
        )
        >> Option.iter (fun gridActions ->
            onGrid (fun grid ->
                grid?saveChanges <- (fun () ->
                    let data = grid.dataSource.data() |> As
                    
                    data
                    |> missingFrom !sourceData
                    |> gridActions.Added

                    !sourceData
                    |> missingFrom data
                    |> gridActions.Deleted

                    data
                    |> Array.filter (fun x -> x?dirty)
                    |>! gridActions.Changed
                    |> Seq.iter (fun x -> x?dirty <- false)

                    sourceData := Array.copy data
                    grid.dataSource?success data
                )
            )
        )

    let render data configuration =
        let config = getConfiguration configuration
        let data = Seq.toArray data
        let grid = ref None
        let onGrid f = !grid |> Option.iter f
        
        let schema =
            config.Columns
            |> Seq.choose (function
                | { Content = Column.CommandButton _ } -> None
                | { Content = Column.Field field } -> Some (field.Field, field.Schema)
            )
            |> Schema.create config.Editable

        let gridConfig =
            K.kendo.data1.DataSourceOptions(
                data = data,
                pageSize = float (pageSize config.Paging),
                schema = K.kendo.data1.DataSourceSchema(model = schema)
            )
            |> buildConfig onGrid configuration

        let element = Div []

        grid := UI.Grid.Create(As element.Body, gridConfig) |> Some

        match configuration with
        | Plain _ -> ()
        | WithToolbar (_, xs) -> applyToolButtons onGrid xs

        element

module Piglet =
    open IntelliFactory.WebSharper.Piglets

    module Grid =
        let rowSelect (stream: Stream<_>) = Grid.selectableRow (Success >> stream.Trigger)