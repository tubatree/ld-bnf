module ldbnf.Tests
open NUnit.Framework
open FSharp.RDF
open Assertion
open rdf
open Bnf.DrugRdf
open resource

[<Test>]
let ``Ensure that addOrdering adds the order property to a single resource`` () =
  let po =
   (P !!"base:pUri", O(Node.Uri !!"base:oUri", lazy[resource !!"base:rUri"
     [ dataProperty !!"base:someDataProperty" ("dataValue"^^xsd.string) ]
     ]))
  ()