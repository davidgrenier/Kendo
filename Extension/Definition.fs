namespace Extension

open IntelliFactory.WebSharper

module Definition =
    open IntelliFactory.WebSharper.InterfaceGenerator

    let TabStrip =
        Class "kendo.ui.TabStrip"
        |+> [Constructor T<obj>]

    let Model =
        let model = Type.New()
        Pattern.Config "kendo.ui.Model" {
            Required = ["fields", T<obj>]
            Optional = ["id", T<string>]
        }
        |=> model
        |+> ["define" => T<obj> ^-> model]

    let Schema =
        Pattern.Config "kendo.ui.Schema" {
            Required = ["model", Model.Type]
            Optional = []
        }

    let Attributes =
        Pattern.Config "kendo.ui.Attributes" {
            Required = ["class", T<string>]
            Optional = []
        }

    let Command =
        Pattern.Config "kendo.ui.Command" {
            Required = ["name", T<string>; "click", T<obj> ^-> T<unit>]
            Optional = []
        }

    let Column =
        Generic / fun t ->
            Pattern.Config "kendo.ui.Column" {
                Required = []
                Optional =
                    [
                        "field", T<string>
                        "title", T<string>
                        "command", Command.Type
    //                    "editor", T<obj> * ColumnConfigurationOptions ^-> T<unit>
                        "filterable", T<bool>
                        "format", T<string>
                        "sortable", T<bool>
                        "template", t ^-> T<string>
                        "width", T<int>
                        "attributes", Attributes.Type
                    ]
            }

    let DataSource =
        Generic / fun t ->
            Pattern.Config "kendo.data.DataSource" {
                Required = []
                Optional =
                    [
                        "data", Type.ArrayOf t
                        "pageSize", T<int>
                        "aggregate", T<obj>
                        "filter", T<obj>
                        "group", T<obj>
                        "onChange", T<unit -> unit>
                        "onError", T<obj -> unit>
                        "page", T<int>
                        "schema", Schema.Type
                    ]
            }

    let ToolButton =
        Pattern.Config "kendo.ui.ToolbarElement" {
            Required = []
            Optional =
                [
                    "template", T<obj> //T<unit> ^-> T<obj>
                    "name", T<string>
                ]
        }

    let GridConfiguration =
        Generic / fun t ->
            Pattern.Config "kendo.ui.GridConfiguration" {
                Required = []
                Optional =
                    [
                        "columns", Type.ArrayOf (Column t)
                        "dataSource", (DataSource t).Type
                        "selectable", T<string>
                        "change", T<obj -> unit>
                        "resizable", T<bool>
                        "filterable", T<bool>
                        "reorderable", T<bool>
                        "editable", T<bool>
                        "groupable", T<bool>
                        "scrollable", T<bool>
                        "sortable", T<bool>
                        "pageable", T<obj>
                        "toolbar", Type.ArrayOf ToolButton
                    ]
            }

    let Grid =
        Generic / fun t ->
            Class "kendo.ui.Grid"
            |+> [
                Constructor T<obj>
                Constructor (T<obj> * GridConfiguration t)
            ]
            |+> Protocol [
                "select" => T<unit> ^-> T<obj>
                "dataItem" => T<obj> ^-> t
            ]

    let kResource name file =
        sprintf "http://cdn.kendostatic.com/2013.2.918/%s" file
        |> Resource name

    let KendoAPI = kResource "KendoAPI" "js/kendo.web.min.js"
    let ThemeCommon = kResource "ThemeCommon" "styles/kendo.common.min.css"
    let Jquery = Resource "Jquery" "http://code.jquery.com/jquery-1.9.1.min.js"

    let Kendo =
        Class "kendo"
        |+> [
            "culture" => T<string> ^-> T<unit>
            "template" => T<string> ^-> T<obj>
            "toString" => T<obj> * T<string> ^-> T<string>
        ]
        |> WithSourceName "Kendo"
        |> Requires [Jquery; KendoAPI; ThemeCommon]

    let Assembly =
        Assembly [
            Namespace "Kendo" [
                Generic - DataSource
                Kendo
            ]
            Namespace "Kendo.UI" [
                Attributes
                TabStrip
                Command
                Model
                Schema
                ToolButton
                Generic - Column
                Generic - GridConfiguration
                Generic - Grid
            ]
            Namespace "Kendo.Dependencies" [
                Jquery
                KendoAPI
            ]
            Namespace "Kendo.Culture" [
                ThemeCommon
                kResource "English" "js/cultures/kendo.culture.en-CA.min.js"
                kResource "French" "js/cultures/kendo.culture.fr-CA.min.js"
            ]
            Namespace "Kendo.Themes" [
                kResource "Default" "styles/kendo.default.min.css"
                kResource "Silver" "styles/kendo.silver.min.css"
            ]
        ]

open IntelliFactory.WebSharper.InterfaceGenerator

[<Sealed>]
type Extension() =
    interface IExtension with
        member x.Assembly = Definition.Assembly

[<assembly: Extension(typeof<Extension>)>]
do ()