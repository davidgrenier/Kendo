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
        C.field "Name" "Name" |> C.width 150 |> C.readonly
        C.field "LastName" "Last Name" |> C.readonly
        C.numeric "Age" "Age" |> C.width 120
        C.date "Died" "Died On"
        |> C.shortDateFormat
        |> C.width 150
        C.bool "Alive" "Alive" |> C.width 70
        C.editor "Test" "Test" [
            "Select...", ""
            "House", "House"
            "Card", "Card"
        ]
        C.field "Door" "Door"
        C.command "Show JSON" (fun v _ ->
            Popup.create "Testing Window" [] (fun onWindow ->
                Div [
                    Div [Text <| Json.Stringify v]
                ] -< [
                    Formlet.Controls.Button "test"
                    |> Formlet.Formlet.Run (fun _ -> onWindow Popup.close)
                ]
            )
        )
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
