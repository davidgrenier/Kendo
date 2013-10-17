[<AutoOpen>]
module WebSharper.Pervasives

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html

type JS = JavaScriptAttribute

[<JS; AutoOpen>]
module Web =
    let (|+) (e: Element) (clazz: string) = e.AddClass clazz; e

    module Option =
        let coalesce b a =
            match a, b with
            | Some _, _ -> a
            | _, b -> b