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

type Test = House | Card

let create() =
    Controls.dropDown [
        "House", House
        "Card", Card
    ]

open IntelliFactory.WebSharper.Formlet

type CC = { Type: string; Number: int }

let paymentFormlet content =
    let ccType =
        [
            "MC", "Master Card"
            "Visa", "VISA"
        ]
    let ccTypeF = 
        Controls.Select 0 ccType
        |> Enhance.WithTextLabel "Card Type"

    let numberF = 
        Controls.Input content
        |> Formlet.MapElement (fun el -> el.AddClass "test"; el)
        |> Enhance.WithCssClass "test"
        |> Enhance.WithTextLabel "Number"
        |> Validator.IsInt "Credit card number must be numeric"
        |> Enhance.WithValidationIcon

    Formlet.Yield (fun typeId num -> { Type = typeId; Number = int num })
    <*> ccTypeF
    <*> numberF
    |> Enhance.WithSubmitButton
    |> Enhance.WithFormContainer

let renderData =
    G.Default [
        C.field "Name" "Name" |> C.width 120 |> C.readonly
        C.field "LastName" "Last Name" |> C.readonly
        C.numeric "Age" "Age" |> C.width 120 |> C.percentFormat 0
        C.date "Died" "Died On" |> C.shortDateFormat |> C.width 150
        C.bool "Alive" "Alive" |> C.width 70
        C.editor "Test" "Test" [
            "Select...", ""
            "House", "House"
            "Card", "Card"
        ]
        C.field "Door" "Door"
        C.command "Show JSON" (fun v ->
            Popup.create "Testing Window" [] (fun onWindow ->
                Div [
                    Json.Stringify v
                    |> paymentFormlet
                    |> Formlet.Formlet.Run (fun _ -> onWindow Popup.close)
                ]
            )
        )
        |> C.width 120
        |> C.centered
        C.delete() |> C.width 120
    ]
    |> G.editable
    |> G.addButton
    |> G.cancelButton
    |> G.saveButton (fun x -> x.Name + x.LastName) (
        SaveActions.onAdd (fun added -> Json.Stringify added |> fun xs -> JavaScript.Alert ("Added ==> " + xs))
        >> SaveActions.onChange (fun added -> Json.Stringify added |> fun xs -> JavaScript.Alert ("Changed ==> " + xs))
        >> SaveActions.onDelete (fun added -> Json.Stringify added |> fun xs -> JavaScript.Alert ("Deleted ==> " + xs))
    )
    |> G.renderData

open WebSharper.Kendo.Extension.UI

let menu =
    let conf = MenuConfiguration(fun x -> Json.Stringify x |> JavaScript.Alert)

    UL [
        LI [
            Text "Menu1"
        ] -- UL [
            LI [Text "test"]
            LI [Text "toto"]
        ]
        LI [
            Text "Menu1"
        ] -- UL [
            LI [Text "test"] -- UL [LI [Text "deep"]]
            LI [Text "toto"]
        ]
    ]
    |>! fun data -> Menu(data.Dom, conf) |> ignore

let page() =
    let grid =
        Div [
            Data.philosophers()
            |> Seq.map Philosopher.ofPerson
            |> renderData
        ]
    Div [
        menu
        create()
        Tabs.createTabs [
            Tabs.create "Grid" (fun () -> grid)
            Tabs.create "TreeView" (fun () -> Div [Text "test"])
        ]
    ]
