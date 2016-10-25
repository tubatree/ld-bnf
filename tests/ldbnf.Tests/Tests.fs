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

  let count = ref 0
  let newPo = Graph.addOrder(po, count)

  match newPo with
  | (P p, O(Node.Uri u, resources)) ->

    let resource = 
      resources.Force() 
      |> List.head
      |> Graph.getResource 

    let property =
      resource.Statements
      |> List.head
      |> Graph.getDataProperty 

    Assert.AreEqual(!!"nicebnf:hasOrder", property.Uri)
    Assert.AreEqual("1", property.Value)