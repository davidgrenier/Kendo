[<JS>]
module WebSharper.Kendo

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.Formlet
open IntelliFactory.WebSharper.KendoUI

let create<'T>(): 'T = As<'T>(obj())

module Option =
    let condition test = function
        | x when test -> Some x
        | _ -> None

    let conditional f x = condition (f x) x

    let ofNull x =
        if x ==. JavaScript.Undefined then None
        else Some x

    let toNull = function
        | None -> null
        | Some x -> x

    let getOrElseF f = function
        | None -> f ()
        | Some x -> x

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
            ui.DropDownListOptions(dataTextField = "text", dataValueField = "value", dataSource = values)
        | Some v ->
            let i = List.findIndex (fun (_, x) -> x = v) choices
            ui.DropDownListOptions(dataTextField = "text", dataValueField = "value", dataSource = values, index = float i)

    let create current choices =
        Input []
        |>! OnAfterRender (fun input ->
            ui.DropDownList.Create(As input.Body, configure (Some current) choices) |> ignore
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
                let animation = ui.TabStripAnimation(close = As false, _open = As false)
                let option = ui.TabStripOptions(animation = animation)
                ui.TabStrip.Create(As el.Body, option)
                |> ignore

module Popup =
    type T =
        private {
            Window: ref<ui.Window.T option>
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

    let private actOn f { Window = w} = w.Value |> Option.iter f

    let close = actOn (fun w -> w.destroy())

    let private render popup =
        let actOn f = !popup.Window |> Option.iter f
        
        let config =
            ui.WindowOptions(
                title = popup.Title,
                width = string popup.Width + "px",
                close = (fun _ -> close popup),
                actions = [|"Close"|],
                animation = As false,
                draggable = popup.Draggable,
                resizable = popup.Resizable,
                modal = popup.Modal
            )
        
        popup.Window := Some (ui.Window.Create(As (popup.Content popup).Body, config))

        actOn (fun w ->
            w.bind("activate", As w.center) |> ignore
            w._open() |> ignore
        )

    let create title settings contentF =
        let applySettings = List.fold (>>) id settings
        {
            Window = ref None
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
        let model = create<data1.DataSourceSchemaModel>()

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
        |> model.set_fields
        
        create<data1.DataSourceSchema>()
        |>! fun schema -> schema.model <- model

module Column =
    type Field<'V> =
        {
            Field: string
            Format: string option
            Template: (Field<'V> -> bool -> 'V -> string) option
            Editor: (string * string) list
            Schema: Schema.T
        }

    type Settings =
        {
            Lockable: bool
            Frozen: bool
        }

        with static member Default = { Lockable = true; Frozen = false }

    type Content<'V> =
        | Field of Settings * Field<'V>
        | CommandButton of Settings * string * ('V -> unit)

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
                Editor = []
                Schema = Schema.zero
            }
        )
        |> create title

    let command label action =
        CommandButton (Settings.Default, label, action)
        |> create ""

    let delete() = command "destroy" ignore

    let private mapContent f col =
        match col.Content with
        | Field (s, c) -> { col with Content = Field (s, f c) }
        | CommandButton _ -> col

    let private mapSettings f col =
        let content =
            match col.Content with
            | Field (s, c) -> Field (f s, c)
            | CommandButton (s, n, a) -> CommandButton (f s, n, a)
        { col with Content = content }

    let editor name title choices =
        field name title
        |> mapContent (fun c -> { c with Editor = choices })

    let width width col = { col with Width = Some width }
    let withClass className col = { col with Attributes = Some ({ ``class`` = className }) }
    
    let private formatWithf templateFunc = mapContent (fun c -> { c with Template = Some templateFunc })
    let formatWith templateFunc = mapContent (fun c -> { c with Template = Some (fun _ _ -> templateFunc) })
    let private dateString format = function
        | null -> ""
        | (value: string) -> Pervasives.toString(As<TypeScript.Lib.Date> value, format)

    let shortDateFormat x = formatWithf (fun f _ v -> dateString "d" ((?) v f.Field)) x
    let longDateFormat x = formatWithf (fun f _ v -> dateString "D" ((?) v f.Field)) x

    let formatField fmt = mapContent (fun c -> { c with Format = Some fmt })
    let rightAligned x = withClass "gridNumericValue" x
    let centered x = withClass "gridCenteredValue" x
    let currencyFormat x = rightAligned x |> formatField "{0:c}"
    let percentFormat precision x = rightAligned x |> formatWithf (fun f _ v -> Pervasives.toString(((?) v f.Field: float), "p" + string precision))

    let frozen x = mapSettings (fun s -> { s with Frozen = true }) x
    let applySchema f = mapContent (fun c -> { c with Schema = f c.Schema })
    let editable c = applySchema Schema.editable c
    let readonly c = applySchema Schema.readonly c
    let typed typ = applySchema (Schema.typed typ)
    let numeric name title = field name title |> rightAligned |> typed Schema.Number
    let date name title = field name title |> typed Schema.Date
    let bool name title =
        field name title
        |> typed Schema.Bool 
        |> formatWithf (fun f editable v ->
            let checkd = if (?) v f.Field then "checked='checked'" else ""
            let editable =
                match f.Schema.Editable, editable with
                | Some false, _ | None, false -> "disabled='disabled'"
                | _ -> ""

            "<input class='k-checkbox' type='checkbox' " + checkd + " " + editable + " />"
        ) 

    let fromMapping (onGrid: (ui.Grid.T -> _) -> _) editable col =
        let column =
            match col.Content with
            | Field (s, f) ->
                ui.GridColumn(field = f.Field, lockable = s.Lockable, locked = s.Frozen)
                |>! fun column ->
                    f.Format |> Option.iter (fun f -> column.format <- f)
                    f.Template |> Option.iter (fun t -> column.template <- t f editable) 
                    match f.Editor with
                    | [] -> ()
                    | choices ->
                        column.editor <- fun (container, options) ->
                            let format = "<input data-bind='value:" + options?field + "'/>"
                            let target = JQuery.JQuery.Of(format).AppendTo(As<JQuery.JQuery> container)
                            ui.DropDownList.Create(As target, DropDown.configure None choices)
                            |> ignore
                        
                        column.filterable <-
                            ui.GridColumnFilterable()
                            |>! fun filterSettings ->
                                filterSettings.ui <- fun input ->
                                    ui.DropDownList.Create(input, DropDown.configure None choices)
//                                    input?kendoColorPicker()
            | CommandButton (s, text, action) ->
                let command =
                    ui.GridColumnCommandItem(name = text, click = As (fun e ->
                            onGrid (fun grid ->
                                JQuery.JQuery.Of(e?currentTarget: Dom.Node).Closest("TR").Get 0
                                |> As<TypeScript.Lib.Element>
                                |> grid.dataItem
                                |> As
                                |> action
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

    let buildConfig (onGrid: (ui.Grid.T -> _) -> _) configuration dataSource =
        let config = getConfiguration configuration
        let columns =
            config.Columns
            |> Seq.map (Column.fromMapping onGrid config.Editable)
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
            | Paging x -> gconf.pageable <- As true
            | Sizer x -> gconf.pageable <- ui.GridPageable(pageSizes = [|5; 10; 20; 30; 50; 75; 100|])
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
                    | Create -> !"create"
                    | Cancel -> !"cancel"
                    | Save _ -> !"save"
                    | Label markup -> ui.GridToolbarItem(template = markup)
                )
                |> List.toArray

        gconf

    let missingFrom second =
        let originalKeys =
            second
            |> Array.map (fun x -> x?uid)
            |> Set.ofArray

        Array.filter (fun x -> originalKeys.Contains x?uid |> not)

    let applyToolButtons getData (onGrid: (ui.Grid.T -> _) -> _) =
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
                    |> Array.Do gridActions.Added

                    !sourceData
                    |> missingFrom data
                    |> Array.Do gridActions.Deleted

                    data
                    |> Array.filter (fun x -> x?dirty)
                    |> Array.Do gridActions.Changed

                    sourceData := Array.copy data

                    grid.dataSource.sync()
                )
                grid?cancelChanges <- (fun () ->
                    getData()
                    |> grid.dataSource.data

                    sourceData := grid.dataSource.data() |> As |> Array.copy
                )
            )
        )
        
    type Schema = { model: obj }
    type Test = { data: obj; pageSize: float; schema: Schema }

    let checkboxDisplayFix (element: Element) (grid: ui.Grid.T) =
        let gridElem = JQuery.JQuery.Of(element.Dom)

        gridElem.On("click", fun ele ->
            if ele?target?className = "k-edit-cell" && ele?target?lastChild?``type`` = "checkbox" then
                grid.closeCell()
                false
            else true
        )

        gridElem.On("click" , ".k-checkbox", fun ele ->
            let target = 
                JQuery.JQuery.Of(ele?target: Dom.Node).Closest("TR")
                |> As<TypeScript.Lib.Element>
                |> grid.dataItem
                    
            target?Alive <- ele?target?``checked``
            target?dirty <- true

            true
        )

    let render getData configuration =
        let config = getConfiguration configuration
        let getData = getData >> Seq.toArray
        let grid = ref None
        let onGrid f = !grid |> Option.iter f
        
        let schema =
            config.Columns
            |> Seq.choose (function
                | { Content = Column.CommandButton _ } -> None
                | { Content = Column.Field (_, field)} -> Some (field.Field, field.Schema)
            )
            |> Schema.create config.Editable

        let gridConfig =
            create<data1.DataSourceOptions>()
            |>! fun x ->
                x.data <- getData()
                x.pageSize <- float (pageSize config.Paging)
                x.schema <- schema
            |> buildConfig onGrid configuration

        Div []
        |>! OnAfterRender (fun el ->
            grid := ui.Grid.Create(As el.Dom, gridConfig) |> Some

            match configuration with
            | Plain _ -> ()
            | WithToolbar (_, xs) -> applyToolButtons getData onGrid xs

            onGrid (checkboxDisplayFix el)
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

module DatePicker =
    open IntelliFactory.WebSharper.Piglets

    module Piglet =
        let create (stream: Stream<System.DateTime>) =
            Input []
            |>! OnAfterRender (fun input ->
                let option = ui.DatePickerOptions(format = "yyyy/MM/dd HH:mm")
                stream.Subscribe (
                    let last = ref None
                    function
                    | Success value ->
                        match !last with
                        | Some v when v = value -> ()
                        | _ ->
                            Option.ofNull value
                            |>! fun x -> last := x
                            |> Option.map (fun v -> v.ToEcma() |> As)
                            |> Option.toNull
                            |> fun x -> option.value <- x
                    | _ -> ()
                )
                |> ignore
                option.change <- fun _ -> (As<EcmaScript.Date> option.value).ToDotNet() |> Success |> stream.Trigger
                ui.DatePicker.Create(As input.Body, option)
                |> ignore
            )

    let create date =
        Stream(Success date)
        |> Piglet.create

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
            |> List.map (fun x ->
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
    let custom position shown permanent (text: string) (element: Element) =
        let option =
            ui.TooltipOptions(
                autoHide = (permanent <> CloseIcon),
                content = As text,
                position =
                    match position with
                    | Center -> "center"
                    | Right -> "right"
                    | Bottom -> "bottom"
                    | Top -> "top"
                    | Left -> "left"
            )

        let tt = ui.Tooltip.Create(As element.Dom, option)

        if shown then
            let id = element.Id
            element
            |> OnAfterRender (fun _ ->
                JQuery.JQuery.Of("#" + id)
                |> As
                |> tt.show
            )

    let create text = custom Center false AutoHide text
    let right text = custom Right false AutoHide text

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
                let error = content |> List.map (fun x -> x.Message) |> String.concat ", "
                notif.show(error, "error")
                
            Q.Of("div.k-animation-container:has(div.k-notification)").Css("left", "50%").Css("transform", "translateX(-50%)")
            |> ignore
        )
        |> ignore

    let create reader = custom CloseIcon reader

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