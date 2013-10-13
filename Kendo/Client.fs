[<JS>]
module Kendo.Client

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html

open Kendo.Controls

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

let grid =
    Grid.Default [
        Column.field "Name" "Name"
        Column.field "LastName" "Last Name"
        Column.field "Age" "Age"
        |> Column.asNumber |> Column.editable
        Column.field "Died" "Died On"
        |> Column.shortDateFormat
        |> Column.asDate |> Column.editable
        Column.command "test" (Json.Stringify >> JavaScript.Alert)
    ]
    |> Grid.withPaging 3
    |> Grid.withPageSizer
    |> Grid.withGrouping
    |> Grid.withFiltering
    |> Grid.withColumnResizing
    |> Grid.withReordering
    |> Grid.renderData

let page() =
    let grid =
        Data.philosophers()
        |> Seq.map Philosopher.ofPerson
        |> grid
    Div [
        Tabs.createTabs [
            Tabs.create "Grid" (fun () -> Div [grid])
            Tabs.create "TreeView" (fun () -> Div [Text "test"])
        ]
    ]