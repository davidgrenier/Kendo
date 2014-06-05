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
        Died: System.DateTime
        Alive: bool
        Door: Door
    }

let philosopher id name last age year month day =
    {
        Id = id
        Name = name
        LastName = last
        Age = age
        Died = System.DateTime(year, month, day)
        Alive = false
        Door = Locked
    }

[<RPC>]
let philosophers() =
    [
        philosopher 0 "Isaac" "Newton" 46 1727 3 20
        philosopher 1 "Ludwig" "Wittgenstein" 62 1947 4 29
        { philosopher 2 "Érasme" "de Rotterdam" 69 1536 6 12 with Alive = true }
        philosopher 3 "Heraclitus" "of Ephesus" 60 1 1 1
        philosopher 4 "Friedrich" "Nietzsche" 55 1900 08 25
        philosopher 5 "Friedrich" "Nietzsche" 55 1900 08 25
        philosopher 6 "Friedrich" "Nietzsche" 55 1900 08 25
        philosopher 7 "Friedrich" "Nietzsche" 55 1900 08 25
        philosopher 8 "Friedrich" "Nietzsche" 55 1900 08 25
        philosopher 9 "Friedrich" "Nietzsche" 55 1900 08 25
        philosopher 10 "Friedrich" "Nietzsche" 55 1900 08 25
        philosopher 11 "Friedrich" "Nietzsche" 55 1900 08 25
        philosopher 12 "Friedrich" "Nietzsche" 55 1900 08 25
        philosopher 13 "Friedrich" "Nietzsche" 55 1900 08 25
        philosopher 14 "Friedrich" "Nietzsche" 55 1900 08 25
        philosopher 15 "Friedrich" "Nietzsche" 55 1900 08 25
        philosopher 16 "Friedrich" "Nietzsche" 55 1900 08 25
        philosopher 17 "Friedrich" "Nietzsche" 55 1900 08 25
        philosopher 18 "Friedrich" "Nietzsche" 55 1900 08 25
        philosopher 19 "Friedrich" "Nietzsche" 55 1900 08 25
        philosopher 20 "Friedrich" "Nietzsche" 55 1900 08 25
        philosopher 21 "Friedrich" "Nietzsche" 55 1900 08 25
        philosopher 22 "Friedrich" "Nietzsche" 55 1900 08 25
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