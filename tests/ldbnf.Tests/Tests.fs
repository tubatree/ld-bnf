module ldbnf.Tests
open NUnit.Framework
open FSharp.RDF
open Assertion
open rdf
open Bnf.DrugRdf
open resource
open Bnf.Order

[<Test>]
let ``Ensure that addOrder creates an add order node for external resource`` () =
  let drug =
   [resource !!"nicebnf:paracetamol" [
     one !!"nicebnf:hasMedicinalForm" !!"nicebnf:PHP5356" []
     one !!"nicebnf:hasMedicinalForm" !!"nicebnf:PHP5373" []
     ]
   ] |> addOrder !!"nicebnf:paracetamol"

  let resource =
    drug
    |> List.head
    |> getResource

  let statements =
    resource.Statements

  let (resourceOneOrderNodeUri, resourceOneStatements) = 
    List.nth statements 0
    |> getBlankNodeFrom

  let resourceOneHasOrder =
    List.nth resourceOneStatements 0
    |> getDataProperty

  Assert.AreEqual(!!"nicebnf:hasMedicinalFormOrder", resourceOneOrderNodeUri)
  Assert.AreEqual(!!"nicebnf:hasOrder", resourceOneHasOrder.Uri)
  Assert.AreEqual("1", resourceOneHasOrder.Value)

  let (resourceTwoOrderNodeUri, resourceTwoStatements) = 
    List.nth statements 1
    |> getBlankNodeFrom

  let resourceTwoHasOrder =
    List.nth resourceTwoStatements 0
    |> getDataProperty

  Assert.AreEqual(!!"nicebnf:hasMedicinalFormOrder", resourceTwoOrderNodeUri)
  Assert.AreEqual(!!"nicebnf:hasOrder", resourceTwoHasOrder.Uri)
  Assert.AreEqual("2", resourceTwoHasOrder.Value)

[<Test>]
let ``Ensure that addOrder creates an hasOrder property within a resource with more than one internal resource`` () =
  let drug =
   [resource !!"nicebnf:paracetamol" [
       one !!"nicebnf:hasIndicationAndDose" !!"nicebnf:paracetamol#36542"
                [dataProperty !!"nicebnf:hasDitaContent" ("abc"^^xsd.string) ]
       one !!"nicebnf:hasIndicationAndDose" !!"nicebnf:paracetamol#25728"
                [dataProperty !!"nicebnf:hasDitaContent" ("abc"^^xsd.string) ]
      ]
   ] |> addOrder !!"nicebnf:paracetamol"

  let resource =
    drug
    |> List.head
    |> getResource

  let statements =
    resource.Statements

  let resourceOne = 
    List.nth statements 0
    |> getResourceFromStatement

  let resourceOneInternalResource = 
    resourceOne
    |> List.head
    |> getResource

  let resourceOneInternalResourceStatements = resourceOneInternalResource.Statements

  let resourceOneProperty = 
    List.nth resourceOneInternalResourceStatements 0
    |> getDataProperty

  Assert.AreEqual(!!"nicebnf:hasOrder", resourceOneProperty.Uri)
  Assert.AreEqual("1", resourceOneProperty.Value)

  let resourceTwo = 
    List.nth statements 1
    |> getResourceFromStatement

  let resourceTwoInternalResource = 
    resourceTwo
    |> List.head
    |> getResource

  let resourceTwoInternalResourceStatements = resourceTwoInternalResource.Statements

  let resourceTwoProperty = 
    List.nth resourceTwoInternalResourceStatements 0
    |> getDataProperty

  Assert.AreEqual(!!"nicebnf:hasOrder", resourceTwoProperty.Uri)
  Assert.AreEqual("2", resourceTwoProperty.Value)