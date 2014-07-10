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

let calculateField data = 
    JavaScript.Log data
    2
    

let philoGrid data =
    G.Default [
        C.numeric "Id" "Id" |> C.width 50 |> C.frozen
        C.field "Name" "Name" |> C.width 220 //|> C.readonly
        C.field "LastName" "Last Name" |> C.width 220
        C.numeric "Age" "Age" |> C.width 220 |> C.percentFormat 0 
        C.date "Died" "Died On" |> C.shortDateFormat |> C.width 220 |> C.formatField "#= (Died == null) ? ' ' : kendo.toString(firstDate, 'yyyy-mm-dd') #"
        C.bool "Alive" "Alive" |> C.width 100
        C.bool "Alive2" "Alive2" |> C.width 100 |> C.readonly 
        C.editor "Test" "Test" [
            "Select...", ""
            "House", "House"
            "Card", "Card"
        ] |> C.width 220
        C.field "Door" "Door" |> C.width 220
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
        C.delete() |> C.width 220
    ]
    |> G.editable
    |> G.withMenu
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
            |> Piglet.Grid.rowSelect row
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
                    Data.philosophers()
                    |> Seq.map Philosopher.ofPerson
                
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
    tokensLists
    |> List.choose (function
        | [] -> None
        | parent :: child -> Some (parent, child)
    ) 
    |> groupValBy snd fst
    |> Seq.map (fun (key, children) -> 
        let children =
            children
            |> Seq.filter ((<>) [])
            |> Seq.toList
        let path = path + key
        match children with
        | [] ->
            T.Checkable.leaf key path true
        | _ -> 
            build (path + "/") children
            |> T.Checkable.node key 
    )
    |> Seq.toList
    
let rec build2 path tokensLists =
    let children, parents =
        tokensLists
        |> List.partitioned (function
            | [], child, checkd ->
                Choice1Of2 (T.Checkable.leaf child path checkd)
            | parent :: children, child, checkd ->
                Choice2Of2 (parent, children, child, checkd)
        )
    parents
    |> groupValBy (fun (_, children, child, checkd) -> children, child, checkd) (fun (p, _, _, _) -> p)
    |> Seq.map (fun (key, children) ->
        children 
        |> Seq.toList
        |> build2 (path + key + "/") 
        |> T.Checkable.node key
    )
    |> Seq.toList
    |> List.append children

let validationIcon reader =
    Div []
    |> Piglets.Controls.ShowErrors reader (fun errors ->
        [
            Span [] |+ "k-icon k-i-note"
            |>! Tooltip.right (String.concat "," errors)
        ]
    )

let errorIcon() =
    let error = Stream(Failure[])
    error.Trigger(Failure [ErrorMessage.Create "Fail" error])

    async {
        do! Async.Sleep 4000
        error.Trigger(Failure [ErrorMessage.Create "toto" error])
    } |> Async.Start

    validationIcon error

let page() =
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
            |> List.map (fun x ->
                let splitedPath =
                    (fst x).Split '/'
                    |> Array.rev
                    |> Array.toList
                splitedPath.Tail |> List.rev, splitedPath.Head, snd x
            )
            |> build2 ""
            |> T.Checkable.create
            |> T.Checkable.changeAction (JavaScript.Log)
            |> T.collapsed
            |> T.render
        ]
    ]