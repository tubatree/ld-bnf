module ldbnf.Tests
open NUnit.Framework
open FSharp.RDF
open Assertion
open rdf
open Bnf.DrugRdf
open resource

[<Test>]
let ``Ensure that addOrdering adds the order property to a single resource`` () =
   ()