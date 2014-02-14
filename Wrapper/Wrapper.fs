[<JS>]
module WebSharper.Kendo

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.Formlet

open WebSharper.Kendo.Extension
open WebSharper.Kendo.Extension.UI

let private dropDownConfig choices =
    let choices =
        choices
        |> List.map (fun x -> DropDownValue x)
        |> List.toArray

    DropDownConfiguration("text", "value", choices)

module Controls =
    let dropDown choices =
        Input []
        |>! OnAfterRender (fun input ->
            DropDownList(input.Body, dropDownConfig choices)
            |> ignore
        )

module Tabs =
    type T = { Name: string; Content: unit -> Element }

    let create name content = { Name = name; Content = content }

    let createTabs = function
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
            |>! fun el -> TabStrip(el.Body, Open = false, Close = false) |> ignore

module Popup =
    type T =
        {
            Content: ((Window -> unit) -> unit) -> Element
            Title: string
            Width: int
            Draggable: bool
            Modal: bool
            Resizable: bool
        }

    let frozen p = { p with Draggable = false }
    let locked p = { p with Resizable = false }
    let withoutOverlay p = { p with Modal = false }

    let close (w: Window) = w.Destroy()
    
    let private render popup =
        let window: Window option ref = ref None
        let actOn f = !window |> Option.iter f

        let config =
            UI.WindowConfiguration (
                popup.Title,
                string popup.Width + "px",
                (fun () -> actOn close),
                [|"Close"|],
                Animation = false,
                Draggable = popup.Draggable,
                Resizable = popup.Resizable,
                Modal = popup.Modal
            )

        let w = UI.Window((popup.Content actOn).Body, config)

        w.Bind("activate", w.Center)
        window := Some w
        w.Open()

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

    let onChange f actions = { actions with Changed = f }
    let onDelete f actions = { actions with Deleted = f }
    let onAdd f actions = { actions with Added = f }

module Schema =
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
            let fieldType = FieldType(defaultArg editable defaultEdit, typ)
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

    type T<'V> =
        {
            Title: string
            Width: int option
            Attributes: Attributes option
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
    let withClass className col = { col with Attributes = Some (Attributes className) }
    
    let private formatWithf templateFunc = mapContent (fun c -> { c with Template = Some (templateFunc c) })
    let formatWith templateFunc = mapContent (fun c -> { c with Template = Some templateFunc })
    let shortDateFormat x = formatWithf (fun f v -> Kendo.ToString((?) v f.Field, "d")) x
    let longDateFormat x = formatWithf (fun f v -> Kendo.ToString((?) v f.Field, "D")) x

    let formatField fmt = mapContent (fun c -> { c with Format = Some fmt })
    let rightAligned x = withClass "rightAligned" x
    let centered x = withClass "centered" x
    let currencyFormat x = rightAligned x |> formatField "{0:c}"
    let percentFormat precision x = rightAligned x |> formatWithf (fun f v -> Kendo.ToString((?) v f.Field, "p" + string precision))

    let applySchema f = mapContent (fun c -> { c with Schema = f c.Schema })
    let editable c = applySchema Schema.editable c
    let readonly c = applySchema Schema.readonly c
    let typed typ = applySchema (Schema.typed typ)
    let numeric title name = field title name |> rightAligned |> typed Schema.Number
    let date title name = field title name |> typed Schema.Date
    let bool title name = field title name |> typed Schema.Bool

    let fromMapping (onGrid: (Grid<_> -> _) -> _) col =
        let column =
            match col.Content with
            | Field f ->
                Column(Field = f.Field)
                |>! fun column ->
                    Option.iter (fun f -> column.Format <- f) f.Format
                    Option.iter (fun t -> column.Template <- t) f.Template
                    match f.Editor with
                    | [] -> ()
                    | choices ->
                        column.Editor <- fun (container, options) ->
                            let format = "<input data-bind='value:" + options?field + "'/>"
                            let target = JQuery.JQuery.Of(format).AppendTo(container)
                            DropDownList(target, dropDownConfig choices)
                            |> ignore
            | CommandButton (text, action) ->
                Column(Command = Command(text, fun e ->
                        onGrid (fun grid ->
                            JQuery.JQuery.Of(e?currentTarget: Dom.Node).Closest("TR").Get 0
                            |> grid.DataItem
                            |> action
                        )
                    )
                )

        column.Title <- col.Title
        Option.iter (fun a -> column.Attributes <- a) col.Attributes
        Option.iter (fun w -> column.Width <- w) col.Width

        column

module Grid =
    type Selectable<'V> =
        | Row of ('V -> unit)
        | Cell of ('V -> unit)

    type Paging =
        | Paging of int
        | Sizer of int

    type ToolButton<'K, 'V> =
        | Create
        | Cancel
        | Save of (('V -> 'K) * SaveActions.T<'V>)

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

    type T<'K, 'V> =
        | Plain of Config<'V>
        | WithToolbar of Config<'V> * ToolButton<'K, 'V> list

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
                |> Seq.distinctBy (function Cancel -> 0 | Create -> 1 | Save _ -> 2)
                |> Seq.toList
            WithToolbar (gridConfig, buttons)
    
    let addButton gridConfig = withToolbarButton Create gridConfig
    let cancelButton gridConfig = withToolbarButton Cancel gridConfig
    let saveButton keySelector configurator = withToolbarButton (Save (keySelector, configurator SaveActions.zero))

    let paging x gridConfig =
        {
            gridConfig with
                Paging =
                    match x, gridConfig.Paging with
                    | 0, _ -> None
                    | x, (None | Some (Paging _)) -> Some (Paging x)
                    | x, Some (Sizer _) -> Some (Sizer x)
        }

    let adjustablePaging gridConfig =
        {
            gridConfig with
                Paging =
                    gridConfig.Paging
                    |> Option.coalesce defaultPaging
                    |> Option.map (fun (Paging x | Sizer x) -> Sizer x)
        }
            
    type Pageable = { pageSizes: bool }

    let buildConfig (onGrid: (Grid<_> -> _) -> _) configuration dataSource =
        let config = getConfiguration configuration
        let columns =
            config.Columns
            |> Seq.map (Column.fromMapping onGrid)
            |> Seq.toArray

        GridConfiguration (
            Columns = columns,
            Scrollable = config.Scrollable,
            Sortable = config.Sortable,
            DataSource = dataSource,
            Resizable = config.Resizable,
            Filterable = config.Filterable,
            Reorderable = config.Reorderable,
            Editable = true,
            Groupable = config.Groupable
        )
        |>! fun gconf ->
            config.Paging
            |> Option.iter (function
                | Paging x -> gconf.Pageable <- true
                | Sizer x -> gconf.Pageable <- { pageSizes = true }
            )

            config.Selectable
            |> Option.iter (fun selectable ->
                let action =
                    match selectable with
                    | Row action -> gconf.Selectable <- "row"; action
                    | Cell action -> gconf.Selectable <- "cell"; action
                gconf.Change <- fun _ -> onGrid (fun grid -> grid.Select() |> grid.DataItem |> action)
            )

            match configuration with
            | Plain _ | WithToolbar (_, []) -> ()
            | WithToolbar (_, buttons) ->
                let (!) label = ToolbarElement(Name = label)
                gconf.Toolbar <-
                    buttons
                    |> List.map (function
                        | Create -> !"create"
                        | Cancel -> !"cancel"
                        | Save _ -> !"save"
                    )
                    |> List.toArray

    let missingFrom keySelector second =
        let originalKeys =
            second
            |> Seq.map keySelector
            |> Set.ofSeq

        Array.filter (keySelector >> originalKeys.Contains >> not)

    let applyToolButtons (onGrid: (Grid<_> -> _) -> _) data =
        let sourceData = ref data

        Seq.tryPick (function
            | Save x -> Some x
            | _ -> None
        )
        >> Option.iter (fun (keySelector, gridActions) ->
            onGrid (fun grid ->
                grid.SaveChanges <- (fun () ->
                    let data = grid.DataSource.Data()

                    JavaScript.Alert "Kung Fu"

                    !sourceData
                    |> missingFrom keySelector data
                    |> gridActions.Deleted
                    
                    data
                    |> missingFrom keySelector !sourceData
                    |> gridActions.Added

                    let changed =
                        data
                        |> Array.filter (fun x -> x?dirty)
                        |>! gridActions.Changed

                    changed
                    |> Seq.iter (fun x -> x?dirty <- false)

                    sourceData := data
                    grid.DataSource.Success data
                )
            )
        )

    let renderData configuration data =
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
            DataSource(
                Data = data,
                PageSize = pageSize config.Paging,
                Schema = Schema(Model schema)
            )
            |> buildConfig onGrid configuration

        let element = Div []

        grid := Grid(element.Body, gridConfig) |> Some

        match configuration with
        | Plain _ -> ()
        | WithToolbar (_, xs) -> applyToolButtons onGrid data xs

        element