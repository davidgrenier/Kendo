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
    }

    with static member ofPerson (p: Data.Philosopher) =
            {
                Name = p.Name
                LastName = p.LastName
                Age = p.Age
                Died = p.Died.ToEcma()
            }

let renderData =
    G.Default [
        C.field "Name" "Name"
            |> C.width 150
        C.field "LastName" "Last Name"
        C.field "Age" "Age"
            |> C.typed Schema.Number
            |> C.editable
            |> C.alignRight
            |> C.width 120
        C.field "Died" "Died On"
            |> C.shortDateFormat
            |> C.typed Schema.Date
            |> C.editable
            |> C.width 150
        C.command "Show JSON" (fun v _ -> Json.Stringify v |> JavaScript.Alert)
            |> C.width 140
    ]
    |> G.paging 5
    |> G.adjustablePaging
    |> G.groupable
    |> G.filterable
    |> G.resizableColumn
    |> G.reorderable
    |> G.addButton
    |> G.cancelButton
//    |> Grid.withRowSelect (Json.Stringify >> JavaScript.Alert)
    |> G.renderData

let example =
        Pre [Text """
module C = Column
module G = Grid

let renderData =
    G.Default [
        C.field "Name" "Name"
            |> C.width 150
        C.field "LastName" "Last Name"
        C.field "Age" "Age"
            |> C.typed Schema.Number
            |> C.editable
            |> C.alignRight
            |> C.width 120
        C.field "Died" "Died On"
            |> C.shortDateFormat
            |> C.typed Schema.Date
            |> C.editable
            |> C.width 150
        C.command "Show JSON" (fun v _ -> Json.Stringify v |> JavaScript.Alert)
            |> C.width 140
    ]
    |> G.paging 5
    |> G.adjustablePaging
    |> G.groupable
    |> G.filterable
    |> G.resizableColumn
    |> G.reorderable
    |> G.addButton
    |> G.cancelButton
    |> G.renderData"""] |+ "code"

let page() =
    let grid =
        Data.philosophers()
        |> Seq.map Philosopher.ofPerson
        |> renderData
    Div [
        Tabs.createTabs [
            Tabs.create "Grid" (fun () -> Div [example; grid])
            Tabs.create "TreeView" (fun () -> Div [Text "test"])
        ]
    ]