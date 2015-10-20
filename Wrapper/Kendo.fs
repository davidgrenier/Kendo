﻿[<JS>]
module WebSharper.Kendo

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.Formlet
open IntelliFactory.WebSharper.KendoUI
open IntelliFactory.WebSharper.Piglets

let create<'T>(): 'T = As<'T>(obj())

module Option =
    let conditional test = function
        | x when test -> Some x
        | _ -> None

    let condition f x = conditional (f x) x

    let ofNull x =
        if x ==. JavaScript.Undefined then None
        else Some x

    let toNull = function
        | None -> null
        | Some x -> x

    let getOrElseF f = function
        | None -> f ()
        | Some x -> x

    let getOrElse v opt = defaultArg opt v

    let mapOrDefault f deflt = function
        | Some v -> f v
        | _ -> deflt

module Array =
    let Do f = function
        | xs when Array.isEmpty xs -> ()
        | xs -> f xs

module List =
    let Do f = function
        | [] -> ()
        | xs -> f xs

module Menu =
    type Item<'T> =
        | Selection of string * 'T
        | Choices of string * Item<'T> list

    let selection label value = Selection(label, value)
    let choices label items = Choices(label, items)

    let private setTag v (el: Element) = el.Body?ValueTag <- v

    let rec private build = function
        | Selection(label, v) ->
            LI [Text label] |>! setTag (Some v)
        | Choices(label, items) ->
            let items = List.map build items
            LI [Text label] -- UL items
            |>! setTag None

    let create (f: 'a -> unit) (structure: Item<'a> list) =
        let el = List.map build structure |> UL
        let configuration =
            ui.MenuOptions(_select = fun x ->
                x.item?ValueTag
                |> Option.iter f
            )
        ui.Menu.Create(As el.Dom, configuration) |> ignore
        el

module DropDown =
    type private T<'t> = { value: 't; text: string }

    let private buildDataSource choices =
        choices
        |> List.map (fun (k, l) -> { value = k; text = l })
        |> List.toArray

    module Piglets =
        let private configure (stream: Stream<_>) choices =
            ui.DropDownListOptions(
                dataTextField = "text",
                dataValueField = "value",
                dataSource = buildDataSource choices,
                change = fun p ->
                    let i : int = p.sender?selectedIndex
                    choices.[i] |> fst |> Success |> stream.Trigger
            )
            |>! fun opt ->
                stream.Subscribe(
                    let current = ref None
                    function
                    | Success v when !current <> Some v ->
                        current := Some v
                        let i = List.findIndex (fun (x, _) -> x = v) choices
                        opt.index <- float i
                    | _ -> ()
                )
                |> ignore

        let private configureMulti (stream: Stream<_>) choices =
            let values =
                choices
                |> List.mapi (fun i (_, label) -> i, label)
                |> buildDataSource

            let value =
                match stream.Latest with
                | Success x -> x |> Array.choose (fun y -> choices |> List.tryFindIndex (fun (choice, _) -> choice = y))
                | Failure _ -> [||]

            let change (evt: ui.MultiSelectChangeEvent) =
                evt.sender.value()
                |> As
                |> Array.map (fun i -> fst choices.[i])
                |> Success
                |> stream.Trigger

            ui.MultiSelectOptions(
                dataTextField = "text",
                dataValueField = "value",
                dataSource = values,
                value = value,
                change = change
            )

        let multi stream choices =
            Input[]
            |>! OnAfterRender (fun el -> ui.MultiSelect.Create(As el.Body, configureMulti stream choices) |> ignore)

        let createFromElement stream choices el =
            ui.DropDownList.Create(As el, configure stream choices) |> ignore

        let create stream choices =
            Input[]
            |>! OnAfterRender (fun el -> createFromElement stream choices el.Body)

    let create current choices =
        let stream = Stream(Success current)
        Piglets.create stream choices

    let createFromElement current choices el =
        let stream = Stream(Success current)
        Piglets.createFromElement stream choices el

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
                let animation = ui.TabStripAnimation(close = As false, _open = As false)
                let option = ui.TabStripOptions(animation = animation)
                ui.TabStrip.Create(As el.Body, option)
                |> ignore

module Popup =
    type T = private Window of ref<ui.Window.T option>
    type Config =
        private {
            Content: T -> Element
            Title: string
            Width: int
            Draggable: bool
            Modal: bool
            Resizable: bool
        }

    let frozen p = { p with Draggable = false }
    let locked p = { p with Resizable = false }
    let withoutOverlay p = { p with Modal = false }

    let private actOn f (Window w) = Option.iter f w.Value

    let close = actOn (fun w -> w.destroy())

    let create title content =
        {
            Content = content
            Title = title
            Width = 500
            Draggable = true
            Modal = true
            Resizable = true
        }

    let show config =
        let w = ref None
        let popup = Window w

        let windowConfig =
            ui.WindowOptions(
                title = config.Title,
                width = string config.Width + "px",
                close = (fun _ -> close popup),
                actions = [|"Close"|],
                animation = As false,
                draggable = config.Draggable,
                resizable = config.Resizable,
                modal = config.Modal
            )

        config.Content popup
        |>! OnAfterRender (fun elem ->
            w := ui.Window.Create(As elem.Body, windowConfig) |> Some

            actOn (fun w ->
                w.bind("activate", As w.center) |> ignore
                w._open() |> ignore
            ) popup
        )
        |> fun e -> e?Render()

module SaveActions =
    type T<'V> =
        {
            Changed: 'V [] -> unit
            Deleted: 'V [] -> unit
            Added: 'V [] -> unit
            RenderUnsaved: (Piglets.Reader<unit> -> Element) option
        }

    let internal zero = { Changed = ignore; Deleted = ignore; Added = ignore; RenderUnsaved = None }

    let withRenderUnsaved f actions = { actions with RenderUnsaved = Some f }

    let onChange f actions = { actions with Changed = Array.Do f }
    let onDelete f actions = { actions with Deleted = Array.Do f }
    let onAdd f actions = { actions with Added = Array.Do f }

module Schema =
    type Field = { editable: bool; ``type``: string }
    type Type = String | Number | Date | Bool
    type T = { Editable: bool option; Type: Type }

    let zero = { Editable = None; Type = String }
    let readonly schema = { schema with Editable = Some false }
    let editable schema = { schema with Editable = Some true }
    let typed typ schema = { schema with Type = typ }

    let create defaultEdit (schemas: (string * T) seq) =
        let fields =
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

        let model = create<data1.DataSourceSchemaModel>()
        model.fields <- fields

        create<data1.DataSourceSchema>()
        |>! fun schema -> schema.model <- model

module DataSource =
    type T<'V> =
        private {
            DataSource: data1.DataSource.T
            CurrentRow: string option
            TriggerUnsaved: unit -> unit
        }

    let internal create dataSource trigger = { DataSource = dataSource; CurrentRow = None; TriggerUnsaved = trigger }
    let internal withRow row dataSource = { dataSource with CurrentRow = Some row }
    let internal getData (dataSource: data1.DataSource.T) = dataSource.data() |> As<data1.Model.T[]>
    let internal values dataSource = getData dataSource |> As<_>

    let saveChange (dataSource: T<'a>) (value: 'a) =
        let data = getData dataSource.DataSource
        let v = data1.Model.Create value
        v?editable <- fun _ -> false
        v?isNew <- fun () -> false

        dataSource.CurrentRow
        |> Option.map (fun uid ->
            v.uid <- uid
            data
            |> Array.tryFindIndex(fun x -> x.uid = uid)
        )
        |> Option.getOrElseF (fun () ->
            dataSource.DataSource.insert (0.0, v) |> ignore
            None
        )
        |> Option.iter (fun i ->
            v.dirty <- true
            data.[i] <- v
        )

        dataSource.DataSource.fetch()

module Filter =
    type T =
        private
        | StartsWith of string
        | EndsWith of string
        | Contains of string
        | LessThan of obj
        | GreaterThan of obj
        | Equality of obj
        | OtherThan of obj

    let startsWith = StartsWith
    let endsWith = EndsWith
    let contains = Contains
    let lessThan (x: 'a when 'a: comparison) = LessThan x
    let greaterThan (x: 'a when 'a: comparison) = GreaterThan x
    let equals (x: 'a when 'a:equality) = Equality x
    let otherThan (x: 'a when 'a:equality) = OtherThan x

    let build =
        let f (name: string) (operator: string) (value: obj) =
            create<data1.DataSourceFilter>()
            |>! fun f ->
                f?field <- name
                f?operator <- operator
                f?value <- value
        Seq.map (fun (name, filter) ->
            match filter with
            | StartsWith v -> f name "startswith" v
            | EndsWith v -> f name "endswith" v
            | Contains v -> f name "contains" v
            | LessThan v -> f name "lt" v
            | GreaterThan v -> f name "gt" v
            | Equality v -> f name "eq" v
            | OtherThan v -> f name "neq" v
        )
        >> Seq.toArray
        >> Option.condition ((<>) [||])
        >> Option.map (fun filters ->
            create<data1.DataSourceFilters>()
            |>! fun fs -> fs.filters <- filters
        )

module Column =
    type Field<'V> =
        {
            Field: string
            Format: string option
            Template: (Field<'V> -> bool -> 'V -> Choice<string,Element>) option
            CallBack: ('V -> bool) option
            Editor: (string * string) list
            Schema: Schema.T
            Filter: Filter.T option
        }

    type CommandButton<'V> =
        {
            Text: string
            OnclickAction: (DataSource.T<'V> -> 'V -> unit)
        }

    type Settings = { Lockable: bool; Frozen: bool }
        with static member Default = { Lockable = true; Frozen = false }

    type Content<'V> =
        | Field of Settings * Field<'V>
        | CommandButton of Settings * CommandButton<'V>

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
        Field (Settings.Default,
            {
                Field = name
                Format = None
                Template = None
                CallBack = None
                Editor = []
                Schema = Schema.zero
                Filter = None
            }
        )
        |> create title

    let command label action =
        CommandButton (Settings.Default,
            {
                Text = label
                OnclickAction = action
            }
        )
        |> create ""

    let delete() = command "destroy" (fun _ _ -> ())

    let private mapColumn f col = f col

    let private mapContent f col =
        match col.Content with
        | Field (s, c) -> { col with Content = Field (s, f c) }
        | CommandButton _ -> col

    let private mapCommandContent f col =
        match col.Content with
        | Field _ -> col
        | CommandButton (s, c) -> { col with Content = CommandButton (s, f c) }

    let private mapSettings f col =
        let content =
            match col.Content with
            | Field (s, c) -> Field (f s, c)
            | CommandButton (s, c) -> CommandButton (f s, c)
        { col with Content = content }

    let filtered filter = mapContent (fun x -> { x with Filter = Some filter })

    let editor name title choices =
        field name title
        |> mapContent (fun c -> { c with Editor = choices })

    let width width col = { col with Width = Some width }
    let withClass className col = { col with Attributes = Some { ``class`` = className } }

    let private templateWithF templateFunc = mapContent (fun c -> { c with Template = Some templateFunc })
    let elementTemplate templateFunc = templateWithF (fun _ _ -> templateFunc >> Choice2Of2)
    let stringTemplate templateFunc = templateWithF (fun _ _ -> templateFunc >> Choice1Of2)

    let formatField fmt = mapContent (fun c -> { c with Format = Some fmt })
    let rightAligned x = withClass "gridNumericValue" x
    let centered x = withClass "gridCenteredValue" x
    let currencyFormat x = rightAligned x |> formatField "{0:c}"

    let onClick callback = mapContent (fun c -> { c with CallBack = Some callback })

    let private dateString format = function
        | null -> ""
        | (value: string) -> Pervasives.toString(As<TypeScript.Lib.Date> value, format)

    let shortDateFormat x = templateWithF (fun f _ v -> Choice1Of2 (dateString "d" ((?) v f.Field))) x
    let longDateFormat x = templateWithF (fun f _ v -> Choice1Of2 (dateString "D" ((?) v f.Field))) x

    let percentFormat precision x =
        rightAligned x
        |> templateWithF (fun f _ v ->
            Choice1Of2 (Pervasives.toString(((?) v f.Field: float), "p" + string precision))
        )

    let frozen x = mapSettings (fun s -> { s with Frozen = true }) x
    let noWrap c = withClass "kendoGridColumnEllipsis" c
    let applySchema f = mapContent (fun c -> { c with Schema = f c.Schema })
    let editable c = applySchema Schema.editable c
    let readonly c = applySchema Schema.readonly c
    let typed typ = applySchema (Schema.typed typ)
    let numeric name title = field name title |> rightAligned |> typed Schema.Number
    let date name title = field name title |> typed Schema.Date
    let bool name title =
        field name title
        |> typed Schema.Bool
        |> templateWithF (fun f editable v ->
            let checkd = if (?) v f.Field then "checked='checked'" else ""
            let editable =
                match f.Schema.Editable, editable with
                | Some false, _ | None, false -> "disabled='disabled'"
                | _ -> ""

            Choice1Of2 ("<input class='k-checkbox' name='" + name + "' type='checkbox' " + checkd + " " + editable + " />")
        )

    let createSchema columns editable =
        columns
        |> Seq.choose (function
            | { Content = CommandButton _ } -> None
            | { Content = Field (_, field)} -> Some (field.Field, field.Schema)
        )
        |> Schema.create editable

    let fromMapping (onGrid: (ui.Grid.T -> _) -> _) trigger editable index col =
        let column =
            match col.Content with
            | Field (s, f) ->
                ui.GridColumn(field = f.Field, lockable = s.Lockable, locked = s.Frozen)
                |>! fun column ->
                    f.Format |> Option.iter (fun e -> column.format <- e)
                    f.Template |> Option.iter (fun t ->
                        column.template <- fun x ->
                            match t f editable x with
                            | Choice1Of2 s -> s
                            | Choice2Of2 e ->
                                let callbackAttribute =
                                    f.CallBack
                                    |> Option.mapOrDefault (fun _ -> [NewAttr "data-callbackid" (string index)]) []
                                (Div [e -< callbackAttribute]).Html
                    )
                    match f.Editor with
                    | [] -> ()
                    | (key,_) :: _ as choices ->
                        column.editor <- fun (container, options) ->
                            let elem =
                                "<input data-bind='value:" + options?field + "'/>"
                                |> JQuery.JQuery.Of
                            elem.AppendTo(As<JQuery.JQuery> container)
                            |> DropDown.createFromElement key choices
                        column.filterable <-
                            ui.GridColumnFilterable()
                            |>! fun filterSettings ->
                                filterSettings.ui <- DropDown.createFromElement "" (("", "") :: choices)
            | CommandButton (s, c) ->
                let command =
                    ui.GridColumnCommandItem(name = c.Text, click = As (fun e ->
                            onGrid (fun grid ->
                                let item =
                                    JQuery.JQuery.Of(e?currentTarget: Dom.Node).Closest("TR").Get 0
                                    |> As<TypeScript.Lib.Element>
                                    |> grid.dataItem

                                let dataSource =
                                    DataSource.create grid.dataSource trigger
                                    |> DataSource.withRow item?uid

                                item
                                |> As
                                |> c.OnclickAction dataSource
                            )
                        )
                    )

                ui.GridColumn(command = [|command|], lockable = s.Lockable, locked = s.Frozen)

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
        | CustomCreate of (DataSource.T<'V> -> unit)
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
            Menus: bool
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
            Menus = false
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
    let withMenu gridConfig = onConfig (fun gridConfig -> { gridConfig with Menus = true }) gridConfig

    let private withToolbarButton kind = function
        | Plain gridConfig -> WithToolbar (gridConfig, [kind])
        | WithToolbar (gridConfig, buttons) ->
            let buttons =
                kind :: buttons
                |> Seq.distinctBy (function Cancel -> 0 | CustomCreate _ | Create -> 1 | Save _ -> 2 | Label _ -> 3)
                |> Seq.toList
            WithToolbar (gridConfig, buttons)

    let addToolbarTemplate (el: Element) gridConfig = withToolbarButton (Label el.Html) gridConfig
    let addButton gridConfig = withToolbarButton Create gridConfig
    let customAddButton clickAction gridConfig = withToolbarButton (CustomCreate clickAction) gridConfig
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

    let buildConfig (onGrid: (ui.Grid.T -> _) -> _) configuration trigger dataSource =
        let config = getConfiguration configuration
        let columns =
            config.Columns
            |> Seq.mapi (fun i col -> Column.fromMapping onGrid trigger config.Editable i col)
            |> Seq.toArray

        let gconf =
            ui.GridOptions (
                columns = columns,
                scrollable = As config.Scrollable,
                sortable = As config.Sortable,
                dataSource = dataSource,
                resizable = config.Resizable,
                filterable = (if config.Filterable then ui.GridFilterable(extra = false) else As false),
                reorderable = config.Reorderable,
                editable = ui.GridEditable(confirmation = false),
                groupable = As config.Groupable,
                columnMenu = As config.Menus
            )

        config.Paging
        |> Option.iter (function
            | Paging _ -> gconf.pageable <- As true
            | Sizer _ -> gconf.pageable <- ui.GridPageable(pageSizes = [|5; 10; 20; 30; 50; 75; 100|])
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
            let (!) label = ui.GridToolbarItem(name = label)
            gconf.toolbar <-
                buttons
                |> List.map (function
                    | CustomCreate _ ->
                        let template =
                            Div [
                                A [Span [] |+ "k-icon k-add"]
                                -< [HRef "\\#"; Text "Add new record"]
                                |+ "k-button k-button-icontext addCustom"
                            ]
                        ui.GridToolbarItem(template = template.Html)
                    | Create -> !"create"
                    | Cancel -> !"cancel"
                    | Save _ -> !"save"
                    | Label markup -> ui.GridToolbarItem(template = markup)
                )
                |> List.toArray

        gconf

    let differentOf second f =
        let originalKeys =
            second
            |> Array.map (fun x -> x?uid)
            |> Set.ofArray

        Array.filter (fun x -> originalKeys.Contains x?uid |> f)

    let missingFrom second = differentOf second not
    let presentIn second = differentOf second id

    let applyToolButtons getData (onGrid: (ui.Grid.T -> _) -> _) clear =
        let sourceData = ref [||]
        onGrid(fun grid -> sourceData := DataSource.values grid.dataSource |> Array.copy)

        Seq.tryPick (function
            | Save x -> Some x
            | _ -> None
        )
        >> Option.iter (fun gridActions ->
            onGrid (fun grid ->
                grid?cancelChanges <- (fun () ->
                    getData()
                    |> grid.dataSource.data

                    clear ()
                    sourceData := DataSource.values grid.dataSource |> Array.copy
                )

                grid?saveChanges <- (fun () ->
                    let data = DataSource.values grid.dataSource
                    data
                    |> missingFrom !sourceData
                    |> Array.Do gridActions.Added

                    !sourceData
                    |> missingFrom data
                    |> Array.Do gridActions.Deleted

                    data
                    |> presentIn !sourceData
                    |> Array.filter (fun x -> x?dirty)
                    |> Array.Do gridActions.Changed

                    sourceData := Array.copy data

                    grid.dataSource.sync()
                    clear ()
                )
            )
        )

    type Schema = { model: obj }
    type Test = { data: obj; pageSize: float; schema: Schema }

    let checkboxDisplayFix (element: Element) (grid: ui.Grid.T) =
        let gridElem = JQuery.JQuery.Of element.Dom

        gridElem.On("click", fun event ->
            let target = event?target
            if (target?className : string).Contains "k-edit-cell" && target?lastChild?``type`` = "checkbox" then
                grid.closeCell()
                false
            else true
        )

        gridElem.On("click" , ".k-checkbox", fun event ->
            let dom = event?target : Dom.Node
            let target =
                JQuery.JQuery.Of(dom).Closest "TR"
                |> As<TypeScript.Lib.Element>
                |> grid.dataItem

            (?<-) target dom?name dom?``checked``

            target?dirty <- true
            grid.dataSource.trigger("change") |> ignore

            true
        )

    let render getData configuration =
        let config = getConfiguration configuration
        let getData = getData >> Seq.toArray
        let grid = ref None
        let onGrid f = !grid |> Option.iter f

        let trigger, clear, appendUnsavedTo =
                match configuration with
                | Plain _ -> []
                | WithToolbar (_, xs) -> xs
                |> Seq.tryPick(function
                    | Save {RenderUnsaved = render} -> render
                    | _ -> None
                )
                |> Option.map (fun render ->
                    let stream = Stream(Success ())
                    let triggerFailure () = stream.Trigger(Result.Failwith "You have unsaved changes.")
                    let clear () = stream.Trigger(Success ())
                    triggerFailure, clear, fun (node: Dom.Node) -> (render stream).Dom |> node.AppendChild |> ignore
                )
                |> Option.getOrElse (id, id, ignore)

        let schema = Column.createSchema config.Columns config.Editable

        let filters =
            config.Columns
            |> Seq.choose (function
                | { Column.Content = Column.Field (_, { Field = name; Filter = filter }) }->
                    Option.map (fun f -> name, f) filter
                | { Column.Content = Column.CommandButton _ } ->
                    None
            )
            |> Filter.build

        let gridConfig =
            create<data1.DataSourceOptions>()
            |>! fun x ->
                x.data <- getData()
                x.pageSize <- float (pageSize config.Paging)
                x.schema <- schema
                filters |> Option.iter (fun fs -> x.filter <- fs)
            |> buildConfig onGrid configuration trigger

        Div []
        |>! OnAfterRender (fun el ->
            grid := ui.Grid.Create(As el.Dom, gridConfig) |> Some

            let jqDom = JQuery.JQuery.Of el.Dom

            match configuration with
            | Plain _ -> ()
            | WithToolbar (_, xs) ->
                applyToolButtons getData onGrid clear xs
                appendUnsavedTo el.Dom.ChildNodes.[0]
                xs
                |> Seq.tryPick (function
                    | CustomCreate x -> Some x
                    | _ -> None
                )
                |> Option.iter (fun onClickAction ->
                    onGrid (fun grid ->
                        let addButton = jqDom.Find ".addCustom"

                        let dataSource = DataSource.create grid.dataSource trigger

                        addButton.Bind("click", fun _ _ -> onClickAction dataSource)
                        |> ignore
                    )
                )

            onGrid (checkboxDisplayFix el)

            onGrid (fun grid ->
                grid.dataSource.bind ("change", As (fun e ->
                        match e?action with
                        | "remove"
                        | "add" -> trigger ()
                        | _ ->
                            grid.dataSource
                            |> DataSource.getData
                            |> Array.tryFind (fun x -> x.dirty || not (x.isNew()))
                            |> Option.iter (fun _ -> trigger ())
                    )
                )
                |> ignore
            )

            let callbacks =
                config.Columns
                |> Seq.mapi (fun i col ->
                    match col.Content with
                    | Column.Field (_, f) -> f.CallBack |> Option.map (fun cb -> i, cb)
                    | _ -> None
                )
                |> Seq.choose id
                |> Map.ofSeq

            jqDom.On("click" , "[data-callbackid]", fun event ->
                let jqTarget = JQuery.JQuery.Of (event?target : Dom.Node)

                let index = jqTarget.Data "callbackid" |> As<int>
                        
                onGrid (fun grid ->
                    let changed =
                        jqTarget.Closest "TR"
                        |> As<TypeScript.Lib.Element>
                        |> grid.dataItem
                        |> As
                        |> callbacks.[index]

                    if changed then
                        grid.dataSource.data <| getData ()
                )

                true
            )
        )

    module Piglet =
        open IntelliFactory.WebSharper.Piglets

        let rowSelect (stream: Stream<_>) = selectableRow (Success >> stream.Trigger)
        let render (reader: Reader<_ -> _ []>) config =
            Div[]
            |>! fun div ->
                reader.Subscribe(function
                    | Success getData ->
                        div.Clear()
                        render getData config
                        |> div.Append
                    | Failure _ ->
                        div.Clear()
                )
                |> ignore
        let renderResult (reader: Reader<_ -> Result<_ []>>) config =
            reader
            |> Reader.Map (fun f -> f >> function Success xs -> xs | Failure _ -> [||])
            |> render <| config

type DateFormat = LongDate | ShortDate

module DatePicker =
    open IntelliFactory.WebSharper.Piglets

    module Piglet =
        let formatDate = function
            | LongDate -> "yyyy/MM/dd HH:mm"
            | ShortDate -> "yyyy/MM/dd"

        let create format (stream: Stream<System.DateTime>) =
            Input []
            |>! OnAfterRender (fun input ->
                let option = ui.DatePickerOptions(format = formatDate format)
                stream.Subscribe (
                    let last = ref None
                    function
                    | Success v when !last <> Some v ->
                        last := Option.ofNull v

                        option.value <-
                            !last
                            |> Option.map (fun v -> v.ToEcma() |> As)
                            |> Option.toNull
                    | _ -> ()
                )
                |> ignore
                option.change <- fun evt ->
                    let newValue = evt.sender.value() |> As<EcmaScript.Date>
                    newValue.ToDotNet() |> Success |> stream.Trigger
                ui.DatePicker.Create(As input.Body, option)
                |> ignore
            )

    let create format date =
        Stream(Success date)
        |> Piglet.create format

module Piglets =
    module DatePicker =
        let create = DatePicker.Piglet.create

    module DropDown =
        let multi stream choices = DropDown.Piglets.multi stream choices
        let create stream choices = DropDown.Piglets.create stream choices

    module Numeric =
        let create format decimals min max step (stream: Stream<decimal>) =
            Input[]
            |>! OnAfterRender (fun input ->
                let option = ui.NumericTextBoxOptions(format = format, decimals = float decimals, spinners = false)
                min |> Option.iter (fun x -> option.min <- x)
                max |> Option.iter (fun x -> option.max <- x)
                step |> Option.iter (fun x -> option.step <- x)

                stream.Subscribe (
                    let last = ref (EcmaScript.Number().ToDotNet() : decimal)
                    function
                    | Failure _ -> ()
                    | Success value when !last = value -> ()
                    | Success value ->
                        last := value
                        option.value <- float !last
                )
                |> ignore

                option.change <- fun evt ->
                    let value : decimal = evt.sender.value().ToEcma().ToDotNet()
                    Success value |> stream.Trigger

                ui.NumericTextBox.Create(As input.Body, option)
                |> ignore
            )

        let integer = create "n0" 0 None None None
        let currency = create "c" 3 None None None
        let decimal = create "n2" 2 None None None
        let percent = create "p0" 2 (Some 0.0) (Some 1.0) (Some 0.01)

module TreeView =
    type Content<'T> =
        | Children of Node<'T> list
        | Value of 'T * bool

    and Node<'T> =
        internal {
            Label: string
            Value: Content<'T>
        }

    type ChangeType<'T> = Checked of 'T | Unchecked of 'T

    type Config<'T> =
        {
            DataSource: Node<'T> list
            ChangeAction: Option<ChangeType<'T> list -> unit>
            Expanded: bool
            Checkboxes: Option<bool>
        }

    let collapsed config = { config with Expanded = false }

    module private Tree =
        type T<'T> =
            {
                text: string
                value: Option<'T>
                items: T<'T> []
                ``checked``: bool
            }

        let node label value = { Label = label; Value = value }

        let rec buildDataSource data =
            data
            |> Seq.map (fun x ->
                match x.Value with
                | Children nodes ->
                    {
                        text = x.Label
                        value = None
                        items = buildDataSource nodes
                        ``checked`` = false
                    }
                | Value (value, checkd) ->
                    {
                        text = x.Label
                        value = Some value
                        items = [||]
                        ``checked`` = checkd
                    }
            )
            |> Seq.toArray

        let create checkboxes sourceData  =
            {
                DataSource = sourceData
                ChangeAction = None
                Expanded = true
                Checkboxes = checkboxes
            }

        let rec backup' dataSource =
            seq {
                for node in dataSource do
                    match node.value with
                    | Some v -> yield v, node.``checked``
                    | None -> yield! backup' node.items
            } |> Set.ofSeq

        let backup (dataSource: data1.HierarchicalDataSource.T) =
            dataSource.view().toJSON()
            |> As
            |> backup'

    let render config =
        let dataSource = data1.HierarchicalDataSource.Create ()
        dataSource.options.data <- Tree.buildDataSource config.DataSource
        dataSource.init()

        let options = ui.TreeViewOptions(dataSource = dataSource, loadOnDemand = false)

        config.Checkboxes
        |> Option.iter (fun cascade -> options.checkboxes <- ui.TreeViewCheckboxes(checkChildren = cascade))

        let element = Div[]
        let treeView = ui.TreeView.Create(As element.Body, options)

        if config.Expanded then
            treeView.expand ".k-item"

        config.ChangeAction
        |> Option.iter (fun action ->
            let original = Tree.backup dataSource |> ref

            dataSource.bind ("change", (fun () ->
                    let newVersion = Tree.backup dataSource
                    let changes = newVersion - !original
                    original := newVersion
                    changes
                    |> Seq.map (function
                        | v, true -> Checked v
                        | v, _ -> Unchecked v
                    )
                    |> Seq.toList
                    |> List.Do action
                ) |> As
            )
            |> ignore
        )

        element

    module Checkable =
        let leaf label value checkd = Tree.node label (Value (value, checkd))
        let node label children = Tree.node label (Children children)

        let withoutCascade config = { config with Checkboxes = Some false }
        let create dataSource = Tree.create (Some true) dataSource
        let changeAction action config = { config with ChangeAction = Some action }

    module Uncheckable =
        let node label children = Checkable.node label children
        let leaf label value = Checkable.leaf label value false

        let create dataSource = Tree.create None dataSource

type Position =
    | Center
    | Right
    | Bottom
    | Top
    | Left

type Visibility = AutoHide | CloseIcon

module Tooltip =
    let show (element: Element) (tt: ui.Tooltip.T) =
        let id = element.Id
        element
        |> OnAfterRender (fun _ ->
            JQuery.JQuery.Of("#" + id)
            |> As
            |> tt.show
        )

    let custom position (shown: _ -> unit) permanent addOptions (contentF: _ -> string) (element: Element) =
        let option =
            ui.TooltipOptions(
                autoHide = (permanent <> CloseIcon),
                content = As contentF,
                position =
                    match position with
                    | Center -> "center"
                    | Right -> "right"
                    | Bottom -> "bottom"
                    | Top -> "top"
                    | Left -> "left"
            )
            |> addOptions

        ui.Tooltip.Create(As element.Dom, option)
        |> shown

    let create (text: string) = custom Center ignore AutoHide id (fun _ -> text)
    let right (text: string) = custom Right ignore AutoHide id (fun _ -> text)
    let onEllipsis e = custom Right ignore AutoHide (fun x -> x.filter <- ".kendoGridColumnEllipsis"; x) (fun e -> e?target?context?textContent) e

module Notification =
    open IntelliFactory.WebSharper.Piglets

    type Q = JQuery.JQuery

    let custom visibility (reader: Reader<string>) =
        let position = ui.NotificationPosition(top = 5.0)

        let option =
            match visibility with
            | AutoHide -> ui.NotificationOptions(position = position, stacking = "down")
            | CloseIcon -> ui.NotificationOptions(autoHideAfter = 0.0, button = true, hideOnClick = false, position = position, stacking = "down")

        let e = Div[]
        Q.Of("body").Append e.Dom |> ignore

        let notif = ui.Notification.Create(As e.Dom, option)

        reader.Subscribe(fun result ->
            match result with
            | Success content ->
                notif.show(content, "info")
            | Failure content ->
                let error = content |> List.map (fun x -> x.Message) |> String.concat "\n"
                notif.show(error, "error")

            Q.Of("div.k-animation-container:has(div.k-notification)").Css("left", "50%").Css("transform", "translateX(-50%)")
            |> ignore
        )
        |> ignore

    let create reader = custom CloseIcon reader
    let temporary reader = custom AutoHide reader

module Upload =
    let private addFileToStream (stream: Stream<_>) fileName content =
        let existing =
            match stream.Latest with
            | Success fileList -> fileList
            | Failure _ -> Map.empty

        existing
        |> Map.add fileName content
        |> Success
        |> stream.Trigger

    let private addAsByteArray (stream: Stream<Map<string, byte[]>>) file =
        let reader = Html5.BinaryFileReader()
        reader.Onloadend <- fun _ ->
            let nastyArray = new Html5.Uint8Array(reader.Result)
            let cleanArray = Array.init nastyArray.Length nastyArray.Get
            
            addFileToStream stream file?name cleanArray

        reader.ReadAsArrayBuffer file?rawFile

    let private addAsText (stream: Stream<Map<string, string>>) file =
        let reader = Html5.TextFileReader()
        reader.Onloadend <- fun _ -> addFileToStream stream file?name reader.Result
        reader.ReadAsText file?rawFile

    let private removeFromStream (stream: Stream<Map<_, _>>) file =
        stream.Latest
        |> Result.Map (fun fileList -> fileList.Remove file?name)
        |> stream.Trigger

    let private custom stream fileProjection =
        Input [Attr.Type "file"; Attr.Id "files"; Attr.Name "files"]
        |>! OnAfterRender (fun el ->
            let asyncConfig = ui.UploadAsync(autoUpload = false, saveUrl = "")
            let uploadConfig =
                ui.UploadOptions(
                    async = asyncConfig,
                    _select = (fun e -> As e.files |> Seq.iter (fileProjection stream)),
                    remove = fun e -> As e.files |> Seq.iter (removeFromStream stream)
                )

            ui.Upload.Create(As el.Body, uploadConfig)
            |> ignore
        )

    module Binary =
        let create stream = custom stream addAsByteArray

    module Text =
        let create stream = custom stream addAsText

module Culture =
    let french() = Pervasives.culture "fr-CA"
    let english() = Pervasives.culture "en-CA"

    type English() =
        inherit Resources.BaseResource("http://cdn.kendostatic.com/2014.1.318/js/cultures/kendo.culture.en-CA.min.js")

    type French() =
        inherit Resources.BaseResource("http://cdn.kendostatic.com/2014.1.318/js/cultures/kendo.culture.fr-CA.min.js")

module Themes =
    open IntelliFactory.WebSharper.KendoUI.Resources

    type Silver = SilverTheme