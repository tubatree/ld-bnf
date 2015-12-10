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

  type Graph with
    static member setupGraph = Graph.ReallyEmpty ["nicebnf",!!Uri.nicebnf
                                                  "rdfs",!!"http://www.w3.org/2000/01/rdf-schema#"
                                                  "bnfsite",!!Uri.bnfsite]
    static member from (x:WoundManagement) =
      let s = optionlist {
               yield a Uri.WoundManagementEntity
               yield x.general >>= (string >> xsd.string >> (dataProperty !!"bnfsite:hasGeneral") >> Some)
               yield dataProperty !!"nicebnf:hasTitle" ((string x.title)^^xsd.string)
              }

      let dr r = resource (Uri.from x) r

      [dr s]
      |> Assert.graph Graph.setupGraph
