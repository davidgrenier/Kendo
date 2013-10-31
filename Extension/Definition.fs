namespace Extension

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.InterfaceGenerator

module Definition =
    let (=@) (name: string) t =
        let names = name.Split '.'
        names.[names.Length - 1] =@ t
        |> WithSetterInline (System.String.Format("void ($this.{0} = $value)", name))
        |> WithGetterInline (System.String.Format("$this.{0}", name))

    let TabStrip =
        Class "kendo.ui.TabStrip"
        |+> [Constructor T<Dom.Element>]
        |+> Protocol [
            "options.animation.open" =@ T<bool>
            "options.animation.close" =@ T<bool>
            "options.select" =@ T<obj>
        ]

    let FieldType =
        Pattern.Config "kendo.ui.FieldType" {
            Required = ["editable", T<bool>; "type", T<string>]
            Optional = []
        }

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
                "saveRow" => T<unit> ^-> T<unit>
            ]

    let kResource name file =
        sprintf "http://cdn.kendostatic.com/2013.3.1030/%s" file
        |> Resource name

    let KendoAPI = kResource "KendoAPI" "js/kendo.web.min.js"
    let ThemeCommon = kResource "ThemeCommon" "styles/kendo.common.min.css"
    // Kendo seems to work just fine with jquery 1.10.2
    // Matching WebSharper's version takes care of not downloading more than once.
    let Jquery = Resource "Jquery" "http://code.jquery.com/jquery-1.10.2.min.js"

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
            Namespace "WebSharper.Kendo.Extension" [
                Generic - DataSource
                Kendo
            ]
            Namespace "WebSharper.Kendo.Extension.UI" [
                Attributes
                TabStrip
                Command
                Model
                FieldType
                Schema
                ToolButton
                Generic - Column
                Generic - GridConfiguration
                Generic - Grid
            ]
            Namespace "WebSharper.Kendo.Dependencies" [
                Jquery
                KendoAPI
            ]
            Namespace "WebSharper.Kendo.Culture" [
                ThemeCommon
                kResource "English" "js/cultures/kendo.culture.en-CA.min.js"
                kResource "French" "js/cultures/kendo.culture.fr-CA.min.js"
            ]
            Namespace "WebSharper.Kendo.Themes" [
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