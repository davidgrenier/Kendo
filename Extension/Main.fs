namespace Extension

open IntelliFactory.WebSharper

module Definition =
    open IntelliFactory.WebSharper.InterfaceGenerator

    let TabStrip =
        Class "kendo.ui.TabStrip"
        |+> [Constructor T<obj>]

    let ColumnValue =
        Pattern.Config "kendo.ui.ColumnValue" {
            Required = ["text", T<string>; "value", T<obj>]
            Optional = []
        }

    let Model =
        let model = Type.New()
        Pattern.Config "kendo.ui.Model" {
            Required = ["fields", T<obj>]
            Optional = ["id", T<string>]
        }
        |=> model
        |+> [
            "define" => T<obj> ^-> model
        ]

    let Schema =
        Pattern.Config "kendo.ui.Schema" {
            Required = ["model", Model.Type]
            Optional = []
        }

    let ColumnConfigurationOptions =
        Pattern.Config "kendo.ui.ColumnsConfiguration.Options" {
            Required = []
            Optional =
                [
                    "field", T<string>
                    "format", T<string>
                    "model", T<obj>
                    "values", Type.ArrayOf ColumnValue
                ]
        }

    let Attributes =
        Pattern.Config "kendo.ui.Attributes" {
            Required = ["class", T<string>]
            Optional = []
        }

    let ColumnsConfiguration =
        Pattern.Config "kendo.ui.ColumnsConfiguration" {
            Required = []
            Optional =
                [
                    "field", T<string>
                    "title", T<string>
                    "command", T<obj>
                    "editor", T<obj> * ColumnConfigurationOptions ^-> T<unit>
                    "filterable", T<bool>
                    "format", T<string>
                    "sortable", T<bool>
                    "template", T<string>
                    "width", T<int>
                    "attributes", Attributes.Type
                ]
        }

    let DataSourceConfiguration =
        Generic / fun t ->
            Pattern.Config "kendo.data.DataSourceConfiguration" {
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

    let DataSource =
        Generic / fun t ->
            Class "kendo.data.DataSource"
            |+> [
                    Constructor T<unit>
                    Constructor (DataSourceConfiguration t)
            ]

    let GridConfiguration =
        Pattern.Config "kendo.ui.GridConfiguration" {
            Required = []
            Optional =
                [
                    "scrollable", T<bool>
                    "sortable", T<bool>
                    "columns", Type.ArrayOf ColumnsConfiguration
                    "dataSource", T<obj>
                    "selectable", T<string>
                    "change", T<obj -> unit>
                    "resizable", T<bool>
                    "filterable", T<bool>
                    "reorderable", T<bool>
                    "editable", T<bool>
                    "groupable", T<bool>
                    "pageable", T<obj>
                ]
        }
        |=> Nested [
            ColumnConfigurationOptions
            ColumnsConfiguration
            ColumnValue
        ]

    let Grid =
        Generic / fun t ->
            Class "kendo.ui.Grid"
            |+> [
                Constructor T<obj>
                Constructor (T<obj> * GridConfiguration)
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
// Replace the above two with this to reproduce
//    let KendoAPI =
//        Resources "KendoAPI" "http://cdn.kendostatic.com/2013.2.918/" [
//            "js/kendo.web.min.js"
//            "styles/kendo.common.min.css"
//        ]
    let Jquery = Resource "Jquery" "http://code.jquery.com/jquery-1.9.1.min.js"

    let Kendo =
        Class "kendo"
        |+> ["culture" => T<string> ^-> T<unit>]
        |> WithSourceName "Kendo"
        |> Requires [Jquery; KendoAPI; ThemeCommon]

    let Assembly =
        Assembly [
            Namespace "Kendo" [Kendo]
            Namespace "Kendo.UI" [
                Attributes
                TabStrip
                Generic - Grid
                GridConfiguration
                Model
                Schema
            ]
            Namespace "Kendo.Data" [
                Generic - DataSourceConfiguration
                Generic - DataSource
            ]
            Namespace "Kendo.Resources" [Jquery; KendoAPI]
            Namespace "Kendo.Resources.Culture" [
                ThemeCommon
                kResource "English" "js/cultures/kendo.culture.en-CA.min.js"
                kResource "French" "js/cultures/kendo.culture.fr-CA.min.js"
            ]
            Namespace "Kendo.Resources.Themes" [
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