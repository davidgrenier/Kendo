namespace Kendo

open IntelliFactory.Html
open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Sitelets
open WebSharper.Kendo

type Action =
    | [<CompiledName "">] EN
    | FR

[<Require(typeof<Themes.Silver>); Require(typeof<Culture.French>)>]
type FrenchPage() =
    inherit Web.Control()
    
    [<JS>]
    override this.Body =
        Culture.french()
        Client.page() :> _

[<Require(typeof<Themes.Silver>); Require(typeof<Culture.English>)>]
type EnglishPage() =
    inherit Web.Control()

    [<JS>]
    override this.Body =
        Culture.english()
        Client.page() :> _

[<Sealed>]
type Site() =
    interface IWebsite<Action> with
        member x.Sitelet =
            fun page ->
                Content.PageContent <| fun _ ->
                    {
                        Page.Default with
                            Title = Some "Kendo samples"
                            Body =
                                match page with
                                | EN -> [Div [new EnglishPage()]]
                                | FR -> [Div [new FrenchPage()]]
                            Head = [Link [Rel "stylesheet"; HRef "Stylesheets/Default.css" ]]
                    }
            |> Sitelet.Infer
        member x.Actions = []

[<assembly: Website(typeof<Site>)>]
do ()