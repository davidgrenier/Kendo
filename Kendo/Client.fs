[<JS>]
module Kendo.Client

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open WebSharper.Kendo

module C = Column
module G = Grid

type Philosopher =
    {
        Name: string
        LastName: string
        Age: int
        Died: EcmaScript.Date
        Alive: bool
    }

    with static member ofPerson (p: Data.Philosopher) =
            {
                Name = p.Name
                LastName = p.LastName
                Age = p.Age
                Died = p.Died.ToEcma()
                Alive = p.Alive
            }

let renderData =
    G.Default [
        C.field "Name" "Name"
            |> C.width 150
        C.field "LastName" "Last Name"
        C.field "Age" "Age"
            |> C.typed Schema.Number
            |> C.editable
            |> C.rightAligned
            |> C.width 120
        C.field "Died" "Died On"
            |> C.shortDateFormat
            |> C.typed Schema.Date
            |> C.editable
            |> C.width 150
        C.field "Alive" "Alive"
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