[<JS>]
module Kendo.Controls

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.Formlet

open Kendo.UI
open Kendo.Data

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

type Action<'T> = 'T -> unit

module Schema =
    type T =
        {
            editable: bool
            ``type``: string
        }

    let readonly = { editable = false; ``type`` = "string" }
    let editable schema = { schema with editable = true }
    let asNumber schema = { schema with ``type`` = "number" }
    let asDate schema = { schema with ``type`` = "date" }

    let create (schemas: (string * T) seq) =
        schemas
        |> Seq.fold (fun schema (fieldName, fieldSchema) ->
            (?<-) schema fieldName fieldSchema
            schema
        ) (obj())

module Column =
    type Field<'T> =
        {
            Field: string
            Title: string
            Width: int option
            Attributes: Attributes option
            Format: string option
            Template: ('T -> string) option
            Schema: Schema.T
        }

    type T<'T> =
        | Field of Field<'T>
        | CommandButton of string * ('T -> unit)

    let field name title =
        Field {
            Field = name
            Title = title
            Width = None
            Attributes = None
            Format = None
            Template = None
            Schema = Schema.readonly
        }

    let command label action = CommandButton (label, action)

    let mapField f = function
        | Field c -> Field (f c)
        | CommandButton _ as c -> c

    let withWidth width = mapField (fun c -> { c with Width = Some width })
    let withClass className = mapField (fun c -> { c with Attributes = Some (Attributes className) })
    let withFormat fmt = mapField (fun c -> { c with Format = Some fmt })
    
    let private formatWithf templateFunc = mapField (fun c -> { c with Template = Some (templateFunc c) })
    let formatWith templateFunc = mapField (fun c -> { c with Template = Some templateFunc })
    let shortDateFormat x = formatWithf (fun f v -> Kendo.ToString((?) v f.Field, "d")) x
    let longDateFormat x = formatWithf (fun f v -> Kendo.ToString((?) v f.Field, "D")) x

    let numericFormat x = withClass "gridNumericValue" x
    let currencyFormat x = numericFormat x |> withFormat "{0:c}"

    let applySchema f = mapField (fun c -> { c with Schema = f c.Schema })
    let editable c = applySchema Schema.editable c
    let asNumber c = applySchema Schema.asNumber c
    let asDate c = applySchema Schema.asDate c

    let getValueFromButtonEvent o (grid: Grid<'T>) =
        JQuery.JQuery.Of(o?currentTarget : Dom.Node).Closest("TR").Get(0)
        |> grid.DataItem

    let fromMapping onGrid =
        function
        | Field f ->
            Column(Field = f.Field, Title = f.Title)
            |>! fun column ->
                f.Attributes |> Option.iter (fun attrib -> column.Attributes <- attrib)
                f.Width |> Option.iter (fun width -> column.Width <- width)
                f.Format |> Option.iter (fun fmt -> column.Format <- fmt)
                f.Template |> Option.iter (fun template -> column.Template <- template)

        | CommandButton (text, action) ->
            let click (e: obj) = onGrid (getValueFromButtonEvent e >> action)
            Column(Command = Command(text, click))

module Grid =
    type Selectable<'T> =
        | Row of Action<'T>
        | Cell of Action<'T>

    type Paging =
        | Paging of int
        | WithSizer of int

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
            Grouping: bool
        }

    let defaultPaging = Some (Paging 10)
    let pageSize = function
        | None -> 0
        | Some (Paging x | WithSizer x) -> x

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
                Grouping = false
            }

    let withGrouping gridConfig = { gridConfig with Grouping = true }
    let withFiltering gridConfig = { gridConfig with Filterable = true }
    let withReordering gridConfig = { gridConfig with Reorderable = true }
    let withColumnResizing gridConfig = { gridConfig with Resizable = true }
    let withoutPaging gridConfig = { gridConfig with Paging = None }
    let withRowSelect action gridConfig = { gridConfig with Selectable = Some (Row action) }
    let withCellSelect action gridConfig = { gridConfig with Selectable = Some (Cell action) }

    let withPaging x gridConfig =
        {
            gridConfig with
                Paging =
                    match x, gridConfig.Paging with
                    | 0, _ -> None
                    | x, (None | Some (Paging _)) -> Some (Paging x)
                    | x, Some (WithSizer _) -> Some (WithSizer x)
        }

    let withPageSizer gridConfig =
        {
            gridConfig with
                Paging =
                    gridConfig.Paging
                    |> Option.coalesce defaultPaging
                    |> Option.map (fun (Paging x | WithSizer x) -> WithSizer x)
        }
            
    let actOnRow action (grid: Grid<_>) =
        grid.Select() |> grid.DataItem |> action

    type Pageable = { pageSizes: bool }

    let buildConfig onGrid config dataSource =
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
            Groupable = config.Grouping
        )
        |>! fun gconf ->
            config.Paging
            |> Option.iter (function
                | Paging x -> gconf.Pageable <- true
                | WithSizer x -> gconf.Pageable <- { pageSizes = true }
            )

            config.Selectable
            |> Option.iter (fun selectable ->
                let action =
                    match selectable with
                    | Row action -> gconf.Selectable <- "row"; action
                    | Cell action -> gconf.Selectable <- "cell"; action
                gconf.Change <- fun _ -> onGrid (actOnRow action)
            )

    let renderData config data =
        let grid = ref None
        let onGrid f = !grid |> Option.iter f
        
        let schema =
            config.Columns
            |> Seq.choose (function
                | Column.CommandButton _ -> None
                | Column.Field field -> Some (field.Field, field.Schema)
            )
            |> Schema.create

        let gridConfig =
            Data.DataSource(
                Data = Seq.toArray data,
                PageSize = pageSize config.Paging,
                Schema = Schema(Model(schema, Id = "Name"))
            )
            |> buildConfig onGrid config

        Div []
        |>! fun el -> grid := Grid(el.Body, gridConfig) |> Some