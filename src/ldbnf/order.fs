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

   let addOrderNode (o, p, c) =
    blank !!(p.ToString() + "Order")
           (optionlist {
            yield dataProperty !!"nicebnf:hasOrder" (c.ToString()^^xsd.string)
            yield dataProperty !!(p.ToString()) (o.ToString()^^xsd.string)
            })

   let addOrderToResources id xs x:List<Statement> =
        let count = ref 0
        x
        |> List.map (fun s -> 
                      match s with
                      | (FSharp.RDF.P p, O(Node.Uri(o), xr)) -> 
                           (count := !count + 1) 
                           if (isEligibleForOrder x p).Value > 1 && p.ToString() <> "rdf:type"
                             && o.ToString().Contains(id) = false && p.ToString() <> "nicebnf:hasClinicalMedicinalProductInformation"
                           then
                            xs := List.append !xs [addOrderNode(o, p, count.Value)]
                            (FSharp.RDF.P p, O(Node.Uri(o), xr))
                           elif (isEligibleForOrder x p).Value > 1 && p.ToString() <> "rdf:type"
                               && o.ToString().Contains(id) = true
                           then
                            let newXr = xr.Value 
                                        |> List.map(fun s-> match s with
                                              R(s, statements) -> 
                                              (count := !count + 1) 
                                              R(s, statements |> List.append [dataProperty !!"nicebnf:hasOrder" (count.Value.ToString()^^xsd.string)]))
                            (FSharp.RDF.P p, O(Node.Uri(o), lazy newXr))
                           else
                            (FSharp.RDF.P p, O(Node.Uri(o), xr))
                      | _ -> s)

   let rec addOrderToResourcesWithinBlankNodes id s = match s with
                      | FSharp.RDF.P(pUri), O(Node.Blank(Blank.Blank(statements)),_) -> 
                        let xs = ref List.Empty
                        let x = statements 
                                |> addOrderToResources id xs 
                                |> List.append xs.Value 
                                |> List.map (addOrderToResourcesWithinBlankNodes id)
                        FSharp.RDF.P(pUri), O(Node.Blank(Blank.Blank(x)), lazy [])
                      | _ -> s

   let addOrder id resources =
     let xs = ref List.Empty
     let id = id.ToString()
     resources
     |> List.map(fun r ->
         match r with
         | R(s, statements) ->
           R(s, statements |> addOrderToResources id xs
                           |> List.append !xs 
                           |> (fun s -> xs:= List.Empty; s)
                           |> List.map (addOrderToResourcesWithinBlankNodes id)))