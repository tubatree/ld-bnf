namespace Bnf
open FSharp.RDF
open Assertion
open rdf
open resource
open Bnf.RdfUris

module Order =
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
     | FSharp.RDF.P(pUri), O(Node.Blank(Blank.Blank(statements)),_) -> 
       (pUri, statements)
     
   let getDataProperty s = 
     match s with
     | (FSharp.RDF.P(pUri), O(Node.Literal(Literal.String value), _)) -> 
       {Uri = pUri; Value = value} 
     | (FSharp.RDF.P(pUri), o) ->    
       {Uri = pUri; Value = "no match"} 

   let addOrderDataProperty c s =
       s |> List.append [dataProperty !!"nicebnf:hasOrder" (c.ToString()^^xsd.string)]

   let addOrderToBlankNode c x =
       x
       |> List.map (fun s -> 
                     match s with
                     | FSharp.RDF.P(pUri), O(Node.Blank(Blank.Blank(statements)),_) -> 
                          (c := !c + 1) 
                          FSharp.RDF.P(pUri), O(Node.Blank(Blank.Blank(statements |> addOrderDataProperty c.Value)), lazy [])
                     | _ -> s)

   let addOrderToNestedResources c x =
       x
       |> List.map (fun s -> 
                     match s with
                     | (FSharp.RDF.P p, O(Node.Uri(o), xr)) -> 
                          (c := !c + 1) 
                          if o.ToString().Contains("http://ld.nice.org.uk/ns/bnf")
                          then
                          (FSharp.RDF.P p, O(Node.Uri(o), xr))
                          else
                           let resources = List.map (fun r -> 
                                                     match r with
                                                      | R(S(uri), statements) ->
                                                        R(S(uri), statements |> addOrderDataProperty c.Value)) xr.Value
                           (FSharp.RDF.P p, O(Node.Uri(o), lazy resources))
                     | _ -> s)
   let addOrder p c =
     match p with
     | (FSharp.RDF.P p, O(n, resources)) -> 
       let newRes = 
         resources.Value
         |> List.map (fun r -> 
                      match r with
                      | R(s, statements) -> 
                        (c := !c + 1) 
                        |> (fun a -> R(s, statements 
                                          |> addOrderDataProperty c.Value
                                          |> addOrderToNestedResources c
                                          |> addOrderToBlankNode c)))

       (FSharp.RDF.P p, O(n, lazy newRes))