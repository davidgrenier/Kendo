[<JS>]
module Kendo.Client

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html

open Kendo.Wrapper

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
    Grid.Default [
        Column.field "Name" "Name"
        |> Column.withWidth 150
        Column.field "LastName" "Last Name"
        Column.field "Age" "Age"
        |> Column.asNumber
        |> Column.editable
        |> Column.alignRight
        |> Column.withWidth 120
        Column.field "Died" "Died On"
        |> Column.shortDateFormat
        |> Column.asDate |> Column.editable
        |> Column.withWidth 150
        Column.command "Show JSON" (Json.Stringify >> JavaScript.Alert)
        |> Column.withWidth 140
    ]
    |> Grid.withPaging 3
    |> Grid.withPageSizer
    |> Grid.withGrouping
    |> Grid.withFiltering
    |> Grid.withColumnResizing
    |> Grid.withReordering
    |> Grid.withCreate
    |> Grid.withCancel
    |> Grid.renderData

let page() =
    let grid =
        Data.philosophers()
        |> Seq.map Philosopher.ofPerson
        |> renderData
    Div [
        Tabs.createTabs [
            Tabs.create "Grid" (fun () -> Div [grid])
            Tabs.create "TreeView" (fun () -> Div [Text "test"])
        ]
    ]