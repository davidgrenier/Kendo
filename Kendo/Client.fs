[<JS>]
module Kendo.Client

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.EcmaScript
open WebSharper.Kendo

module C = Column
module G = Grid

type Philosopher =
    {
        P: Data.Philosopher
        Died: Date
    }

    with static member ofPerson (p: Data.Philosopher) =
            {
                P = p
                Died = p.Died.ToEcma()
            }

let renderData =
    G.Default [
        C.field "P.Name" "Name"
            |> C.width 150
        C.field "P.LastName" "Last Name"
        C.field "P.Age" "Age"
            |> C.typed Schema.Number
            |> C.editable
            |> C.rightAligned
            |> C.width 120
        C.field "Died" "Died On"
            |> C.shortDateFormat
            |> C.typed Schema.Date
            |> C.editable
            |> C.width 150
        C.field "P.Alive" "Alive"
            |> C.typed Schema.Bool
            |> C.editable
            |> C.width 70
        C.command "Show JSON" (fun v _ -> Json.Stringify v |> JavaScript.Alert)
            |> C.width 140
            |> C.centered
    ]
    |> G.paging 5
    |> G.adjustablePaging
    |> G.groupable
    |> G.filterable
    |> G.resizableColumn
    |> G.reorderable
    |> G.addButton
    |> G.cancelButton
    |> G.renderData

let page() =
    let grid =
        Data.philosophers()
        |> Seq.map Philosopher.ofPerson
        |> renderData
    Div [
        Tabs.createTabs [
            Tabs.create "Grid" (fun () -> grid)
            Tabs.create "TreeView" (fun () -> Div [Text "test"])
        ]
    ]