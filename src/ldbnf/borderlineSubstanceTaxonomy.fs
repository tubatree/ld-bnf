namespace Bnf
open FSharp.Data
open Shared

module BorderlineSubstanceTaxonomy =
  type Title = | Title of string

  type BorderlineSubstanceTaxonomy = {
    id:Id;
    title: Title;
    general: drugProvider.Section option
    substances: Id list;
    categories: Id list;
    }

//<section outputclass="general">

module BorderlineSubstanceTaxonomyParser =
  open BorderlineSubstanceTaxonomy
  open prelude

  type BorderlineSubstanceTaxonomy with
    static member parse (x:drugProvider.Topic) =
      let title = Title(x.Title.Value |? "")
      let general = x |> sections "general" |> Array.tryPick Some
      let ids = match x.Body with
                   | Some b -> b.Sections
                                |> Array.collect (fun s -> s.Ps)
                                |> Array.collect (fun p -> p.Xrefs)
                                |> Array.map (fun x -> x.Href |> Id)
                                |> Array.toList
                   | None -> []
      let cats = x.Xrefs |> Array.map (fun x -> x.Href |> Id) |> Array.toList
      {id=Id(x.Id);title=title;general=general;substances=ids;categories=cats}
