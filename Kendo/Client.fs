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
        Name: string
        LastName: string
        Age: int
        Died: Date
        Alive: bool
        Test: string
        Door: Data.Door
    }

    with static member ofPerson (p: Data.Philosopher) =
            {
                Name = p.Name
                LastName = p.LastName
                Age = p.Age
                Died = p.Died.ToEcma()
                Alive = p.Alive
                Test =
                    match p.Name with
                    | "Isaac" -> "House"
                    | _ -> "Card"
                Door = p.Door
            }

open WebSharper.Kendo.Extension.UI

type Test = House | Card

let create() =
    Controls.dropDown [
        "House", House
        "Card", Card
    ]

let renderData =
    G.Default [
        C.field "Name" "Name"
            |> C.width 150
            |> C.readonly
        C.field "LastName" "Last Name"
            |> C.readonly
        C.field "Age" "Age"
            |> C.numeric
            |> C.width 120
        C.field "Died" "Died On"
            |> C.shortDateFormat
            |> C.typed Schema.Date
            |> C.width 150
        C.field "Alive" "Alive"
            |> C.typed Schema.Bool
            |> C.width 70
        C.field "Test" "Test"
            |> C.editor [
                "Select...", ""
                "House", "House"
                "Card", "Card"
            ]
        C.field "Door" "Door"
        C.command "Show JSON" (fun v _ -> Json.Stringify v |> JavaScript.Alert)
            |> C.width 140
            |> C.centered
    ]
    |> G.editable
    |> G.addButton
    |> G.cancelButton
    |> G.renderData

let page() =
    let grid =
        Div [
            Data.philosophers()
            |> Seq.map Philosopher.ofPerson
            |> renderData
        ]
    Div [
        create()
        Tabs.createTabs [
            Tabs.create "Grid" (fun () -> grid)
            Tabs.create "TreeView" (fun () -> Div [Text "test"])
        ]
    ]
