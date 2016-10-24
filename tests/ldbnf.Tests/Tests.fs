module ldbnf.Tests
open NUnit.Framework
open FSharp.RDF
open Assertion
open rdf
open Bnf.DrugRdf
open Bnf.RdfUris
open Bnf.Drug
open Bnf.DrugParser
open Bnf.Shared
open System.IO
open System
open resource
open System.Collections
open System.Collections.Generic

[<Test>]

let ``add order to every drug triple`` () =

  //arrange
  let fi = new StreamReader ("C:/apps/ld-bnf/process/xml/drug/betamethasone.xml")
  let xml = fi |> drugProvider.Load |> Drug.parse
  let sec = Graph.fromsec (Uri.fromsec xml)
  let triples = xml.sections |> Seq.map sec |> Seq.collect id |> Seq.toList


  let count = ref 0

  let addOrder x = match x with
               | x -> ((count := !count + 1) |> (fun c-> List.append x [dataProperty !!"nicebnf:hasOrder2" (count.Value.ToString()^^xsd.string)]))

  let rec processXS xs = seq {
                   for (p,o) in xs do
                        match p,o with
                        | (p, O(Node.Blank(Blank.Blank(xst')),_))
                               -> yield (p,o) 
                        | (p, O(Node.Uri(_), xr)) -> for R(s, xss) in xr.Value do
                                                         yield (p,o)
                        | (p,o) -> yield (p,o) 
                 }

  let statementTuple = processXS triples |> Seq.toList
      
  Assert.IsNotEmpty(triples)

//  let loopThroughEachResourceListAndAppendHasOrderToStatementList x = match x with
//       R(s, xs) ->  R(s, processXS xs |> Seq.toList)
//
//  let modifyResource (p,o) = match o with
//        | O(n,r) -> (p,O(n, List.map loopThroughEachResourceListAndAppendHasOrderToStatementList r.Value |> (fun l -> lazy(l))))
//
//  let processStatement x = match x with
//      | (p,o) -> modifyResource (p,o)

//  let rec processSL x = match x with
//      | head :: tail ->  processStatement head :: processSL tail
//      | [] -> []

let rebuildResource x = x
                           

let rebuildPO x = match x with
                        | (p, O(u, xr)) -> (p, O(u, rebuildResource xr))

let recurse x = match x with
                | (p,o) -> rebuildPO((p,o))

[<Test>]
let ``simple case`` () =
  // Arrange
  let uri1 = !!"http://test.com/uri1"
  let po =
    (P uri1, O(Node.Uri uri1, lazy[resource uri1
      [ blank !!"base:someBlankProperty"
          [ dataProperty !!"base:someDataProperty" ("value2"^^xsd.string) 
            dataProperty !!"base:someDataProperty2" ("value3"^^xsd.string) ]
        dataProperty !!"base:someDataProperty3" ("value4"^^xsd.string) ]
      ]))

  // Act
  let newPO = recurse po

  Assert.AreEqual(po, newPO)

type Resource = {
  Uri : FSharp.RDF.Uri
  Statements : Statement list
}

type StringDataProperty = {
  Uri : FSharp.RDF.Uri
  Value : string
}

let getResource r = 
  match r with
  | R(S(uri), statements) ->
    { Uri = uri; Statements = statements }

let getBlankNodeFrom s = 
  match s with
  | P(pUri), O(Node.Blank(Blank.Blank(statements)),_) -> 
    (pUri, statements)
     
let getDataProperty s = 
  match s with
  | (P(pUri), O(Node.Literal(Literal.String value), _)) -> 
    {Uri = pUri; Value = value} 
  | (P(pUri), o) ->    
    {Uri = pUri; Value = "no match"} 

let addOrderDataProperty c s =
    s |> List.append [dataProperty !!"nicebnf:hasOrder" (c.ToString()^^xsd.string)]

let addOrderToBlankNode x =
    let bCount = ref 0
    x
    |> List.map (fun s -> 
                  match s with
                  | P(pUri), O(Node.Blank(Blank.Blank(statements)),_) -> 
                       (bCount := !bCount + 1) 
                       P(pUri), O(Node.Blank(Blank.Blank(statements |> addOrderDataProperty bCount.Value)), lazy [])
                  | _ -> s)
     
let addOrdering p =
  let rCount = ref 0
  match p with
  | (P p, O(n, resources)) -> 
    let newRes = 
      resources.Value
      |> List.map (fun r -> 
                   match r with
                   | R(s, statements) -> 
                     (rCount := !rCount + 1) 
                     |> (fun a -> R(s, statements 
                                       |> addOrderDataProperty rCount.Value
                                       |> addOrderToBlankNode)))

    (P p, O(n, lazy newRes))
       
[<Test>]
let ``Ensure that addOrdering adds the order property to a single resource`` () =

  let po =
   (P !!"base:pUri", O(Node.Uri !!"base:oUri", lazy[resource !!"base:rUri"
     [ dataProperty !!"base:someDataProperty" ("dataValue"^^xsd.string) ]
     ]))

  let newPo = addOrdering po

  match newPo with
  | (P p, O(Node.Uri u, resources)) ->

    let resource = 
      resources.Force() 
      |> List.head
      |> getResource 

    let property =
      resource.Statements
      |> List.head
      |> getDataProperty 

    Assert.AreEqual(!!"nicebnf:hasOrder", property.Uri)
    Assert.AreEqual("1", property.Value)

[<Test>]
let ``Ensure that addOrdering adds order to multiple resources and increments order`` () =
  let po =
   (P !!"base:pUri", O(Node.Uri !!"base:oUri", lazy[
         resource !!"base:rUri1" [ dataProperty !!"base:someDataProperty" ("dataValue"^^xsd.string) ]
         resource !!"base:rUri2" [ dataProperty !!"base:someDataProperty" ("dataValue"^^xsd.string) ]]))

  let newPo = addOrdering po

  match newPo with
  | (P p, O(Node.Uri u, resources)) ->

    let resources = 
      resources.Force() 
      |> List.map getResource

    let statements =
      resources
      |> List.map (fun s-> s.Statements)

    let resourceOneProperty = 
      List.nth statements 0
      |> List.head
      |> getDataProperty

    let resourceTwoProperty = 
      List.nth statements 1
      |> List.head
      |> getDataProperty

    Assert.AreEqual(!!"nicebnf:hasOrder", resourceOneProperty.Uri)
    Assert.AreEqual("1", resourceOneProperty.Value)

    Assert.AreEqual(!!"nicebnf:hasOrder", resourceTwoProperty.Uri)
    Assert.AreEqual("2", resourceTwoProperty.Value)

[<Test>]
let ``Ensure that addOrdering adds order to a blank node``() = 
  let po =   
   (P !!"base:pUri", O(Node.Uri !!"base:oUri", lazy[resource !!"base:rUri"
     [ blank !!"base:someBlankProperty"
         [ dataProperty !!"base:someBlankDataProperty" ("blankValue"^^xsd.string)]]]))
  
  let newPo = addOrdering po

  match newPo with
  | (P p, O(Node.Uri u, resources)) ->
    Assert.AreEqual(!!"base:pUri", p)
    Assert.AreEqual(!!"base:oUri", u)

    let resource =
      resources.Force()
      |> List.head
      |> getResource

    let (bUri, bStatements) = 
      List.nth resource.Statements 1
      |> getBlankNodeFrom

    let property =
      bStatements
      |> List.head
      |> getDataProperty

    Assert.AreEqual(!!"nicebnf:hasOrder", property.Uri)
    Assert.AreEqual("1", property.Value)

[<Test>]
let ``Ensure that addOrdering adds order to multipe blank nodes and increments order``() = 
  let po =   
   (P !!"base:pUri", O(Node.Uri !!"base:oUri", lazy[resource !!"base:rUri"
     [ blank !!"base:someBlankProperty1" [ dataProperty !!"base:someBlankDataProperty" ("blankValue"^^xsd.string) ]
       blank !!"base:someBlankProperty2" [ dataProperty !!"base:someBlankDataProperty" ("blankValue"^^xsd.string) ]]]))
  
  let newPo = addOrdering po

  match newPo with
  | (P p, O(Node.Uri u, resources)) ->
    Assert.AreEqual(!!"base:pUri", p)
    Assert.AreEqual(!!"base:oUri", u)

    let resource =
      resources.Force()
      |> List.head
      |> getResource

    let (bnAUri, bnAStatements) = 
      List.nth resource.Statements 1
      |> getBlankNodeFrom
   
    let bnAProperty =
      List.nth bnAStatements 0
      |> getDataProperty 

    Assert.AreEqual(!!"nicebnf:hasOrder", bnAProperty.Uri)
    Assert.AreEqual("1", bnAProperty.Value)

    let (bnBUri, bnBStatements) = 
      List.nth resource.Statements 2
      |> getBlankNodeFrom
   
    let bnBProperty =
      List.nth bnBStatements 0
      |> getDataProperty 

    Assert.AreEqual(!!"nicebnf:hasOrder", bnBProperty.Uri)
    Assert.AreEqual("2", bnBProperty.Value)

[<Test>]
let ``Ensure that addOrdering maintains the existing structure`` () =

  let po =
   (P !!"base:pUri", O(Node.Uri !!"base:oUri", lazy[resource !!"base:rUri"
     [ blank !!"base:someBlankProperty"
         [ dataProperty !!"base:someBlankDataProperty" ("blankValue"^^xsd.string)]
       dataProperty !!"base:someDataProperty" ("dataValue"^^xsd.string) ]
     ]))

  let newPo = addOrdering po

  match newPo with
  | (P p, O(Node.Uri u, resources)) ->
    Assert.AreEqual(!!"base:pUri", p)
    Assert.AreEqual(!!"base:oUri", u)

    let resource = 
      resources.Force() 
      |> List.head
      |> getResource 

    Assert.AreEqual(!!"base:rUri", resource.Uri)

    let property =
      List.nth resource.Statements 2
      |> getDataProperty 

    Assert.AreEqual(!!"base:someDataProperty", property.Uri)
    Assert.AreEqual("dataValue", property.Value)

    let (bUri, bStatements) = 
      List.nth resource.Statements 1
      |> getBlankNodeFrom

    Assert.AreEqual(!!"base:someBlankProperty", bUri)

    let property =
      List.nth bStatements 1
      |> getDataProperty 

    Assert.AreEqual(!!"base:someBlankDataProperty", property.Uri)
    Assert.AreEqual("blankValue", property.Value)