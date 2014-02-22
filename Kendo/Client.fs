[<JS>]
module Kendo.Client

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.EcmaScript
open IntelliFactory.WebSharper.Piglets

open WebSharper.Kendo
open WebSharper.Kendo.Extension.UI

module C = Column
module G = Grid
module V = Piglet.Validation

type Philosopher =
    {
        Id: int
        Name: string
        LastName: string
        Age: int
        Died: Date
        Alive: bool
        Test: string
        Door: Data.Door
    }

    static member ofPerson (p: Data.Philosopher) =
        {
            Id = p.Id
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

    static member toPerson (p: Philosopher): Data.Philosopher =
        {
            Id = p.Id
            Name = p.Name
            LastName = p.LastName
            Age = p.Age
            Died = p.Died.ToDotNet()
            Alive = p.Alive
            Door = Data.Open
        }

type CardType = MC | Visa

type CC = { Type: CardType; Number: int }

let paymentForm onSubmit content =
    Piglet.Return (fun typeId num -> { Type = typeId; Number = int num})
    <*> Piglet.Yield MC
    <*> (
        Piglet.Yield content
        |> V.Is (fun x ->
            let rx = new RegExp(@"\d{16,18}")
            rx.Test x
        ) "Credit card number must be numeric"
    )
    |> Piglet.WithSubmit
    |> Piglet.Run onSubmit
    |> Piglet.Render (fun cardType number submit ->
        Div [
            [
                MC, "Master Card"
                Visa, "VISA"
            ]
            |> Controls.Select cardType
            |> Controls.WithLabel "Card Type"

            Controls.Input number
            |> Controls.WithLabel "Number"

            Controls.Submit submit
        ]
    )

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
                    |> paymentForm (fun _ -> onWindow Popup.close)
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
    |> G.saveButton (
        let act action = Array.map Philosopher.toPerson >> Data.actOn action
        SaveActions.onAdd (act Data.Added)
        >> SaveActions.onChange (act Data.Updated)
        >> SaveActions.onDelete (act Data.Removed)
    )
    |> G.renderData

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

let renderGrid() =
    Data.philosophers()
    |> Seq.map Philosopher.ofPerson
    |> renderData

type Test = Editing | Grouping

let gridSetup() =
    Piglet.Yield Editing
    |> Piglet.Render (fun kind ->
        Div [
            [
                Editing, "Editing"
                Grouping, "Grouping"
            ]
            |> Controls.Select kind

            Div []
            |> Controls.Show kind (function
                | Editing -> [renderGrid()]
                | Grouping -> []
            )
        ]
    )

let page() =
    Div [
        menu
        [
            "Editing", Editing
            "Grouping", Grouping
        ]
        |> DropDown.create Editing
        gridSetup()
    ]