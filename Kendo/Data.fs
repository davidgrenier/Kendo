module Kendo.Data

type Door =
    | Open
    | Locked
    | ClosedBy of string

    with
        [<JS>]
        override x.ToString() =
            match x with
            | Open -> "Open"
            | Locked -> "Locked"
            | ClosedBy _ -> "Closed"

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

let deadPhilo id name last age year month day = philosopher id name last age (Some(year, month, day))
let philo id name last age = philosopher id name last age None

[<RPC>]
let philosophers() =
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

type Action =
    | Added
    | Removed
    | Updated

[<RPC>]
let actOn (action: Action) philosophers =
    philosophers
    |> Array.map (fun x -> sprintf "%A: %i - %s" action x.Id x.LastName)
    |> sprintf "%A"