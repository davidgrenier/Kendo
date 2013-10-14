[<JS>]
module WebSharper.Kendo.Wrapper

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.Formlet

open WebSharper.Kendo.Extension
open WebSharper.Kendo.Extension.UI

module Tabs =
    type T =
        {
            Name: string
            Content: unit -> Element
        }

    let create name content =
        {
            Name = name
            Content = content
        }

    let createTabs tabs =
        if List.isEmpty tabs then
            failwith "The list must contain at least one tab."
        
        let li tab = LI [Text tab.Name]

        Div [
            UL [
                yield tabs.Head |> li |+ "k-state-active"
                yield! tabs.Tail |> Seq.map li
            ]
        ] -< (tabs |> Seq.map (fun t -> t.Content()))
        |>! OnAfterRender (fun el ->
            let tabStrip = TabStrip el.Body
            tabStrip?options?animation?close <- false
            tabStrip?options?animation?``open`` <- false
        )

module Schema =
    type Type = String | Number | Date
    type T = { Editable: bool; Type: Type }

    let readonly = { Editable = false; Type = String }
    let editable schema = { schema with Editable = true }
    let typed typ schema = { schema with Type = typ }

    let create (schemas: (string * T) seq) =
        schemas
        |> Seq.fold (fun schema (fieldName, { Editable = editable; Type = typ } ) ->
            let typ =
                match typ with
                | String -> "string"
                | Number -> "number"
                | Date -> "date"
            (?<-) schema fieldName (FieldType(editable, typ))
            schema
        ) (obj())

module Column =
    type Field<'T> =
        {
            Field: string
            Format: string option
            Template: ('T -> string) option
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
            Schema = Schema.readonly
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
    let alignRight x = withClass "alignRight" x
    let currencyFormat x = alignRight x |> formatField "{0:c}"

    let applySchema f = mapContent (fun c -> { c with Schema = f c.Schema })
    let editable c = applySchema Schema.editable c
    let typed typ = applySchema (Schema.typed typ)

    let fromMapping (onGrid: (Grid<_> -> _) -> _) col =
        let column =
            match col.Content with
            | Field f ->
                Column(Field = f.Field)
                |>! fun column ->
                    Option.iter (fun f -> column.Format <- f) f.Format
                    Option.iter (fun t -> column.Template <- t) f.Template
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
                ToolButtons = []
            }

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
            |> Schema.create

        let gridConfig =
            DataSource(
                Data = Seq.toArray data,
                PageSize = pageSize config.Paging,
                Schema = Schema(Model schema)
            )
            |> buildConfig onGrid config

        Div []
        |>! fun el -> grid := Grid(el.Body, gridConfig) |> Some