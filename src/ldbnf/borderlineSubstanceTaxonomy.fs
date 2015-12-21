namespace Bnf
open FSharp.Data
open Shared

module BorderlineSubstanceTaxonomy =
  type Title = | Title of string

  type BorderlineSubstanceCategory =
    | BorderlineSubstanceCategory of Title * Id list

module BorderlineSubstanceTaxonomyParser =
  open BorderlineSubstanceTaxonomy
  open prelude

  type BorderlineSubstanceCategory with
    static member parse (x:drugProvider.Topic) =
      let title = Title(x.Title.Value |? "")
      let ids = match x.Body with
                   | Some b -> b.Sections
                                |> Array.collect (fun s -> s.Ps)
                                |> Array.collect (fun p -> p.Xrefs)
                                |> Array.map (fun x -> x.Href |> Id)
                                |> Array.toList
                   | None -> []
      BorderlineSubstanceCategory(title, ids)
