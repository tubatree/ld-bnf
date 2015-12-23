namespace Bnf
open FSharp.RDF
open FSharp.Data.Runtime

module WoundManagementRdf =
  open prelude
  open resource
  open Bnf.WoundManagement
  open Assertion
  open rdf
  open Shared
  open Rdf
  open RdfUris

  let dp n = xsd.string >> dataProperty !!("nicebnf:has" + n)

  type Graph with
    static member setupGraph = Graph.ReallyEmpty ["nicebnf",!!Uri.nicebnf
                                                  "rdfs",!!"http://www.w3.org/2000/01/rdf-schema#"
                                                  "bnfsite",!!Uri.bnfsite]
    static member fromTitle (Title t) = dataProperty !!"rdfs:label" (t^^xsd.string)

    static member from (x:WoundManagement) =
      let s = optionlist {
               yield a Uri.WoundManagementEntity
               yield x.general >>= (string >> (dp "General") >> Some)
               yield x.title |> Graph.fromTitle
               yield! x.dressingChoices |> List.map Graph.fromWoundType
               yield! x.productGroups |> List.map Graph.fromProductGroup
              }

      let dr r = resource (Uri.from x) r

      [dr s]
      |> Assert.graph Graph.setupGraph

    static member fromDescription (Description sd) =
      dataProperty !!"nicebnf:hasDitaContent" (sd |> (string >> xsd.xmlliteral))

    static member fromwml (x:WoundManagementLink) =
      objectProperty !!"nicebnf:hasWoundManagment" (Uri.totopic(x.rel,x.id))

    static member fromExudate (WoundExudate(s,wmls)) =
      blank !!"nicebnf:WoundExudate"
        (dataProperty !!"nicebnf:hasRate" (s^^xsd.string) :: (wmls |> List.map Graph.fromwml))

    static member fromWoundType (WoundType(TypeOfWound(t),d,wes)) =
      blank !!"nicebnf:hasWoundType"
        (optionlist {
            yield t |> dp "TypeOfWound"
            yield d >>= (Graph.fromDescription >> Some)
            yield! wes |> List.map Graph.fromExudate
          })

    static member fromProduct (x:Product) =
      one !!"nicebnf:hasProduct" (Uri.from x)
       [x.manufacturer |> dp "Manufacturer"
        x.name |> dp "Name"
        x.price |> (string >> (dp "Price"))]

    static member fromProductGroup (ProductGroup(t,d,pl)) =
      blank !!"nicebnf:hasProductGroup"
        (optionlist {
            yield t |>  Graph.fromTitle
            yield d >>= (Graph.fromDescription >> Some)
            yield! pl |> List.map Graph.fromProduct
          })
