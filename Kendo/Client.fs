[<JS>]
module Kendo.Client

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html
open IntelliFactory.WebSharper.EcmaScript
open IntelliFactory.WebSharper.Piglets

open WebSharper.Kendo

module C = Column
module G = Grid
module M = Menu
module V = Piglet.Validation

module Option =
    let toNull = function
        | None -> null
        | Some x -> x

module String =
    let notBlank = function
        | null | "" -> None
        | x -> Some x

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
            Died = p.Died |> Option.map (fun d -> d.ToEcma()) |> Option.toNull
            Alive = p.Alive
            Test =
                match p.Name with
                | "Isaac" -> "House"
                | _ -> "Card"
            Door = p.Door
        }

    static member toPerson p : Data.Philosopher =
        {
            Id = p.Id
            Name = p.Name
            LastName = p.LastName
            Age = p.Age
            Died = p.Died |> Option.ofNull |> Option.map (fun d -> d.ToDotNet())
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
        |> V.Is (RegExp(@"\d{16,18}").Test) "Credit card number must be numeric"
    )
    |> Piglet.WithSubmit
    |> Piglet.Run onSubmit
    |> Piglet.Render (fun cardType number submit ->
        Div [
            [MC, "Master Card"; Visa, "VISA"]
            |> Controls.Select cardType
            |> Controls.WithLabel "Card Type"

            Br[]

            Controls.Input number
            |> Controls.WithLabel "Number"

            Div []
            |> Controls.ShowErrors number (fun errs ->
                errs
                |> Seq.map (fun x -> Span [Text x])
            )

            Controls.Submit submit
        ]
    )

let philoGrid data =
    G.Default [
        C.delete() |> C.width 100 |> C.frozen
        C.numeric "Id" "Id" |> C.width 50 |> C.frozen
        C.field "Name" "Name" |> C.width 170 |> C.readonly
        C.field "LastName" "Last Name" |> C.width 170
        C.numeric "Age" "Age" |> C.width 120
        C.date "Died" "Died On" |> C.shortDateFormat |> C.width 180
        C.bool "Alive" "Alive" |> C.width 100
        C.bool "Alive2" "Alive2" |> C.width 100 |> C.readonly 
        C.editor "Test" "Test" [
            "Select...", ""
            "House", "House"
            "Card", "Card"
        ] |> C.width 150
        C.field "Door" "Door" |> C.width 150
        C.command "Show JSON" (fun v ->
            Popup.create "Testing Window" [] (fun onWindow ->
                Div [
                    Json.Stringify v
                    |> paymentForm (fun _ -> onWindow Popup.close)
                ]
            )
        )
        |> C.width 220
        |> C.centered
    ]
    |> G.editable
    |> G.withMenu
    |> G.filterable
    |> G.addButton
    |> G.cancelButton
    |> G.adjustablePaging
    |> G.saveButton (
        let act action = Array.map Philosopher.toPerson >> Data.actOn action >> JavaScript.Log
        SaveActions.onAdd (act Data.Added)
        >> SaveActions.onChange (act Data.Updated)
        >> SaveActions.onDelete (act Data.Removed)
    )
    |> G.render data

let dependentPhilo data =
    Piglet.YieldFailure()
    |> Piglet.Render (fun row ->
        Div [
            G.Default [
                C.field "Name" "Name"
                C.field "LastName" "LastName"
            ]
            |> G.addToolbarTemplate (Span [Text "Hello World!"])
            |> G.Piglet.rowSelect row
            |> G.render data

            Div[]
            |> Controls.ShowString row (fun x -> "Your name is: " + x.Name)
        ]
    )

type Actions =
    | Test
    | Toto
    | Deep

let menu =
    [
        M.choices "Menu1" [
            M.selection "test" Test
            M.selection "toto" Toto
        ]
        M.choices "Menu2" [
            M.choices "Nested" [
                M.selection "deep" Deep
            ]
            M.selection "toto" Toto
        ]
    ]
    |> M.create (function
        | Test -> JavaScript.Alert "Test clicked"
        | Toto -> JavaScript.Alert "Toto clicked"
        | Deep -> JavaScript.Alert "Deep clicked"
    )

type Test = Editing | Grouping

let gridKind() =
    Piglet.Yield Editing
    |> Piglet.Render (fun kind ->
        Div [
            Controls.Select kind [
                Editing, "Editing"
                Grouping, "Grouping"
            ]

            Div []
            |> Controls.Show kind (fun kind ->
                let data =
                    Data.getPhilosophers
                    >> Seq.map Philosopher.ofPerson
                
                match kind with
                | Editing -> [philoGrid data]
                | Grouping -> [dependentPhilo data]
            )
        ]
    )

module T = TreeView

let groupValBy valueF keyF elements =
   elements
   |> Seq.groupBy keyF
   |> Seq.map (fun (key, values) -> key, values |> Seq.map valueF)
    
let rec build path tokensLists =
    let children, parents =
        tokensLists
        |> List.partitioned (function
            | [], child, checkd ->
                Choice1Of2 (T.Checkable.leaf child (path + child) checkd)
            | parent :: children, child, checkd ->
                Choice2Of2 (parent, children, child, checkd)
        )
    parents
    |> groupValBy (fun (_, children, child, checkd) -> children, child, checkd) (fun (p, _, _, _) -> p)
    |> Seq.map (fun (key, children) ->
        children 
        |> Seq.toList
        |> build (path + key + "/") 
        |> T.Checkable.node key
    )
    |> Seq.toList
    |> List.append children

let showResult (reader: Reader<'a>) (render: Result<'a> -> #seq<#IPagelet>) (container: Element) =
    reader.Subscribe(fun x ->
        container.Clear()
        for e in render x do
            container.Append(e :> IPagelet)
    )
    |> ignore
    container

let showErrors reader render =
    showResult reader (function
        | Success _
        | Failure [] -> []
        | Failure xs -> xs |> List.map (fun x -> x.Message) |> render
    )

let validationIcon reader =
    Div []
    |> showErrors reader (fun errors ->
        [
            Span []
            |+ "k-icon k-i-note"
            |>! Tooltip.right (String.concat "," errors)
        ]
    )

let errorIcon() =
    let error = Stream(Result.Failwith "An error")

    async {
        do! Async.Sleep 4000
        error.Trigger(Result.Failwith "Another error")
    } |> Async.Start

    validationIcon error

let notification () =
    let content = Stream(Success "anticonstitutionnellement")
    async {
        do! Async.Sleep 2000
        content.Trigger(Result.Failwith "dichlorodiphenyltrichloroethane")
    } |> Async.Start
    Notification.create content

let page() =
    notification()
    Div [
        menu
        [
            "Editing", Editing
            "Grouping", Grouping
        ]
        |> DropDown.create Editing
        errorIcon()

        gridKind()
        Div [
            [
                "Reports/ParcelPost/ParcelPost.asp", true
                "Reports/ParcelPost/Search.asp", false
                "Reports/PurchaseSummary/main.asp", true
                "Actions/Certificate/Certificate.asp", false
            ]
            |> List.map (fun (path, checkd) ->
                let splitedPath =
                    path.Split '/'
                    |> Array.rev
                    |> Array.toList
                List.rev splitedPath.Tail, splitedPath.Head, checkd
            )
            |> build ""
            |> T.Checkable.create
            |> T.Checkable.changeAction (Json.Stringify >> JavaScript.Log)
            |> T.collapsed
            |> T.render
        ]
    ]