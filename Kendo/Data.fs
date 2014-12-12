module Kendo.Data

type Door =
    | Open
    | Locked

    with
        [<JS>]
        override x.ToString() =
            match x with
            | Open -> "Open"
            | Locked -> "Locked"

let getCases<'t> () =
    Microsoft.FSharp.Reflection.FSharpType.GetUnionCases typeof<'t>
    |> Array.map (fun x -> x.Name)

[<RPC>]
let getDoorCases = getCases<Door>

type Philosopher =
    {
        Id: int
        Name: string
        LastName: string
        Age: int
        Died: System.DateTime option
        Alive: bool
        Door: Door
    }

let philosopher id name last age deathDate =
    let died =
        deathDate
        |> Option.map (fun (year, month, day) -> System.DateTime(year, month, day))
    {
        Id = id
        Name = name
        LastName = last
        Age = age
        Died = died
        Alive = died.IsNone
        Door = Locked
    }

[<RPC>]
let deadPhilo id name last age year month day = philosopher id name last age (Some(year, month, day))
let philo id name last age = philosopher id name last age None

let philosophers =
    [
        deadPhilo 0 "Isaac" "Newton" 46 1727 3 20
        deadPhilo 1 "Ludwig" "Wittgenstein" 62 1947 4 29
        philo 2 "Érasme" "de Rotterdam" 69
        deadPhilo 3 "Heraclitus" "of Ephesus" 60 1 1 1
        deadPhilo 4 "Friedrich" "Nietzsche" 55 1900 08 25
        deadPhilo 5 "Friedrich" "Nietzsche" 55 1900 08 25
        deadPhilo 6 "Friedrich" "Nietzsche" 55 1900 08 25
        deadPhilo 7 "Friedrich" "Nietzsche" 55 1900 08 25
        deadPhilo 8 "Friedrich" "Nietzsche" 55 1900 08 25
        deadPhilo 9 "Friedrich" "Nietzsche" 55 1900 08 25
        deadPhilo 10 "Friedrich" "Nietzsche" 55 1900 08 25
        deadPhilo 11 "Friedrich" "Nietzsche" 55 1900 08 25
        deadPhilo 12 "Friedrich" "Nietzsche" 55 1900 08 25
        deadPhilo 13 "Friedrich" "Nietzsche" 55 1900 08 25
        deadPhilo 14 "Friedrich" "Nietzsche" 55 1900 08 25
        deadPhilo 15 "Friedrich" "Nietzsche" 55 1900 08 25
        deadPhilo 16 "Friedrich" "Nietzsche" 55 1900 08 25
        deadPhilo 17 "Friedrich" "Nietzsche" 55 1900 08 25
        deadPhilo 18 "Friedrich" "Nietzsche" 55 1900 08 25
        deadPhilo 19 "Friedrich" "Nietzsche" 55 1900 08 25
        deadPhilo 20 "Friedrich" "Nietzsche" 55 1900 08 25
        deadPhilo 21 "Friedrich" "Nietzsche" 55 1900 08 25
        deadPhilo 22 "Friedrich" "Nietzsche" 55 1900 08 25
    ]
    |> List.map (fun x -> x.Id, x)
    |> Map.ofSeq
    |> ref

let nextId =
    let id =
        philosophers.Value
        |> Seq.maxBy (fun (KeyValue(id, _)) -> id)
        |> fun x -> x.Key
        |> ref
    fun () -> incr id; !id

[<RPC>]
let getPhilosophers() =
    !philosophers
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.toArray

type Action =
    | Added
    | Removed
    | Updated

[<RPC>]
let actOn (action: Action) changes =
    changes
    |> Array.iter (fun x ->
        match action with
        | Added ->
            let newId = nextId()
            philosophers := philosophers.Value.Add(newId, { x with Id = newId })
        | Removed ->
            philosophers := philosophers.Value.Remove x.Id
        | Updated ->
            philosophers := philosophers.Value.Add(x.Id, x)
    )

[<RPC>]
let actOnNow (action: Action) changes =
    actOn action changes
    true

[<RPC>]
let rightNow() = System.DateTime.Now

[<RPC>]
let nextDay (date: System.DateTime) = date.AddDays 1.0