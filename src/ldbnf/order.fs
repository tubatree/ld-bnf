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

   let isEligibleForOrder x label =
     let count = ref 0
     x |> List.iter (fun x -> 
                     match x with
                     | (FSharp.RDF.P p, O(Node.Uri(o), xr)) ->
                       if p = label
                       then
                       (count := !count + 1)
                     | _ -> ())
     count

   let addOrderDataProperty id c s =
       let xs = ref List.Empty
       s
       |> List.map(fun s ->
             match s with
             | (FSharp.RDF.P p, O(Node.Uri(o), xr)) ->
               (if o.ToString().Contains(id) = true
               then
               xs := List.append !xs [dataProperty !!"nicebnf:hasOrder" (c.ToString()^^xsd.string)])
               (FSharp.RDF.P p, O(Node.Uri(o), xr))
             | _ -> s)
       |> List.append xs.Value

   let addOrderToBlankNode x =
       let count = ref 0
       x
       |> List.map (fun s -> 
                     match s with
                     | FSharp.RDF.P(pUri), O(Node.Blank(Blank.Blank(statements)),_) -> 
                          (count := !count + 1) 
                          FSharp.RDF.P(pUri), O(Node.Blank(Blank.Blank(statements |> addOrderDataProperty "" count.Value)), lazy [])
                     | _ -> s)

   let addOrderNode (o, p, c) =
    blank !!(p.ToString() + "Order")
           (optionlist {
            yield dataProperty !!"nicebnf:hasOrder" (c.ToString()^^xsd.string)
            yield dataProperty !!(p.ToString()) (o.ToString()^^xsd.string)
            })
   let addOrderToNestedResources xs x:List<Statement> =
        let count = ref 0
        x
        |> List.map (fun s -> 
                      match s with
                      | (FSharp.RDF.P p, O(Node.Uri(o), xr)) -> 
                           (count := !count + 1) 
                           (if (isEligibleForOrder x p).Value > 1 && p.ToString() <> "rdf:type"
                           then
                            xs := List.append !xs [addOrderNode(o, p, count.Value)])
                           (FSharp.RDF.P p, O(Node.Uri(o), xr))
                      | _ -> s)
   let addOrder id resources =
     let parentResources = ref List.Empty
     let count = ref 0
     resources
     |> List.map(fun r ->
         match r with
         | R(s, statements) ->
           R(s, statements 
                |> List.map (fun s ->
                     let nestedResource = ref List.Empty
                     match s with
                     | (FSharp.RDF.P p, O(Node.Uri(o), resources)) -> 
                       (count := !count + 1) 
                       (if (isEligibleForOrder statements p).Value > 1 
                           && p.ToString() <> "rdf:type" && o.ToString().Contains(id) = false
                       then
                        parentResources := List.append !parentResources [addOrderNode(o, p, !count)])
                       let newRes = 
                         resources.Value
                         |> List.map(fun r -> 
                                      match r with
                                      | R(s, statements) -> 
                                           R(s, statements
                                                |> addOrderToBlankNode
                                                |> addOrderToNestedResources nestedResource
                                                |> List.append nestedResource.Value
                                                |> (fun c-> nestedResource := List.empty; c)
                                                |> addOrderDataProperty id count.Value))
                       (FSharp.RDF.P p, O(Node.Uri(o), lazy newRes))
                     | _ -> s) |> List.append parentResources.Value |> (fun c -> parentResources:= List.Empty; c)))