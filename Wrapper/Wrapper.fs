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
                    JQuery.JQuery.Of(t.Dom).One("mouseover", fun _ _ ->
                        async {
                            body -- tab.Content() |> ignore
                        } |> Async.Start
                    )
                    |> ignore
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
            Content: Element
            Title: string
            Width: int
            Draggable: bool
            Modal: bool
            Resizable: bool
        }

    let create title content =
        {
            Content = content
            Title = title
            Width = 500
            Draggable = true
            Modal = false
            Resizable = true
        }

    let frozen popup = { popup with Draggable = false }
    let locked popup = { popup with Resizable = false }
    let withOverlay popup = { popup with Modal = true }
    
    let render popup =
        let window: Window option ref = ref None

        let actOn f =
            match !window with
            | Some w -> f w
            | None -> ()

        let config =
            UI.WindowConfiguration (
                popup.Title,
                string popup.Width + "px",
                (fun () -> ()),
                [|"Close"|],
                Animation = false
            )
        UI.Window(popup.Content.Body, config)
        |>! fun w -> window := Some w
        

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
    type Field<'T> =
        {
            Field: string
            Format: string option
            Template: ('T -> string) option
            Editor: (string * string) list
            Schema: Schema.T
        }

    type Content<'T> =
        | Field of Field<'T>
        | CommandButton of string * ('T -> Grid<'T> -> unit)

    type T<'T> =
        {
            Title: string
            Width: int option
            Attributes: Attributes option
            Content: Content<'T>
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

    let private mapContent f col =
        let content = 
            match col.Content with
            | Field c -> Field (f c)
            | CommandButton _ -> col.Content
        { col with Content = content }

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

    let applySchema f = mapContent (fun c -> { c with Schema = f c.Schema })
    let editor choices = mapContent (fun c -> { c with Editor = choices })
    let editable c = applySchema Schema.editable c
    let readonly c = applySchema Schema.readonly c
    let typed typ = applySchema (Schema.typed typ)
    let numeric x = rightAligned x |> typed Schema.Number

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
                            |> action <| grid
                        )
                    )
                )

        column.Title <- col.Title
        Option.iter (fun a -> column.Attributes <- a) col.Attributes
        Option.iter (fun w -> column.Width <- w) col.Width

        column

module Grid =
    type Selectable<'T> =
        | Row of ('T -> unit)
        | Cell of ('T -> unit)

    type Paging =
        | Paging of int
        | Sizer of int

    type BuiltInButton = Create | Cancel
    type ToolButton =
        | BuiltIn of BuiltInButton
        | Template of Element

    type T<'T> =
        {
            Paging: Paging option
            Columns: Column.T<'T> seq
            Selectable: Selectable<'T> option
            Scrollable: bool
            Sortable: bool
            Resizable: bool
            Reorderable: bool
            Filterable: bool
            Groupable: bool
            Editable: bool
            ToolButtons: ToolButton list
        }

    let defaultPaging = Some (Paging 10)
    let pageSize = function
        | None -> 0
        | Some (Paging x | Sizer x) -> x

    let Default columns =
            {
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
                ToolButtons = []
            }

    let sortable gridConfig = { gridConfig with Sortable = true }
    let nonScrollable gridConfig = { gridConfig with Scrollable = false }
    let groupable gridConfig = { gridConfig with Groupable = true }
    let filterable gridConfig = { gridConfig with Filterable = true }
    let reorderable gridConfig = { gridConfig with Reorderable = true }
    let resizableColumn gridConfig = { gridConfig with Resizable = true }
    let withoutPaging gridConfig = { gridConfig with Paging = None }
    let selectableRow action gridConfig = { gridConfig with Selectable = Some (Row action) }
    let selectableCell action gridConfig = { gridConfig with Selectable = Some (Cell action) }
    let private withToolbarButton kind gridConfig = { gridConfig with ToolButtons = kind :: gridConfig.ToolButtons }
    let addButton gridConfig = withToolbarButton (BuiltIn Create) gridConfig
    let cancelButton gridConfig = withToolbarButton (BuiltIn Cancel) gridConfig
    let customToolButton e = withToolbarButton (Template e)
    let editable gridConfig = { gridConfig with Editable = true }

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

    let buildConfig (onGrid: (Grid<_> -> _) -> _) config dataSource =
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

            match config.ToolButtons with
            | [] -> ()
            | xs ->
                gconf.Toolbar <-
                    config.ToolButtons
                    |> List.map (function
                        | BuiltIn Create -> ToolbarElement(Name = "create")
                        | BuiltIn Cancel -> ToolbarElement(Name = "cancel")
                        | Template e -> ToolbarElement(Template = Kendo.Template(e.Body?outerHTML))
                    )
                    |> List.toArray

    let renderData config data =
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
                Data = Seq.toArray data,
                PageSize = pageSize config.Paging,
                Schema = Schema(Model schema)
            )
            |> buildConfig onGrid config

        Div []
        |>! fun el -> grid := Grid(el.Body, gridConfig) |> Some