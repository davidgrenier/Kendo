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
        Name: string
        LastName: string
        Age: int
        Died: System.DateTime
        Alive: bool
        Door: Door
    }

let philosopher name last age year month day =
    {
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
        philosopher "Isaac" "Newton" 46 1727 3 20
        philosopher "Ludwig" "Wittgenstein" 62 1947 4 29
        philosopher "Érasme" "de Rotterdam" 69 1536 6 12
        philosopher "Heraclitus" "of Ephesus" 60 1 1 1
        philosopher "Friedrich" "Nietzsche" 55 1900 08 25
    ]