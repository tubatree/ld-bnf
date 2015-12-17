namespace Bnf
open FSharp.Data

module Content =
  type contentProvider = XmlProvider<"supercontent.xml", Global=true, SampleIsList=true>

  open Shared

  type Title =
    | Title of string
    override __.ToString () = match __ with | Title x -> x

  type Content = | Content of Id * Title * contentProvider.Body option * Content list

module ContentParser =
  open Content
  open Shared

  type Content with
    static member parse (x:contentProvider.Topic) =
      let c = x.Topics |> Array.map Content.parse |> Array.toList
      Content(Id(x.Id), Title(x.Title),x.Body,c)
