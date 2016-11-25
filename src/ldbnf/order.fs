namespace Bnf
open FSharp.RDF
open Assertion
open rdf
open resource
open Bnf.RdfUris
open System.IO
open System
open DrugParser
open Shared
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

   let getResourceFromStatement s = 
     match s with
     | (FSharp.RDF.P p, O(Node.Uri(o), xr)) ->
       xr.Value
     | _ -> []

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

   let unreservedChars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-_.~";

   let urlEncode str =
       String.init (String.length str) (fun i ->
           let symbol = str.[i]
           if unreservedChars.IndexOf(symbol) = -1 then
               "%" + String.Format("{0:X2}", int symbol)
           else
               string symbol)

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
            yield dataProperty !!(p.ToString()) (Uri.EscapeUriString(o.ToString())^^xsd.string)
            })

   
   let getCountFromFile o path =
       let xmlDirectory = Environment.GetCommandLineArgs() |> Array.filter (fun x-> x.Contains("xml")) |> Array.filter (fun x-> x <> "--xmldirectory")
       let fi =  File.OpenText(xmlDirectory.[0]+path)
       let list = fi |> drugProvider.Load 
                     |> (fun x -> x.Xrefs) 
                     |> Array.map (fun xref -> xref.Href)
       let segments = (new Uri(o.ToString())).Segments 
       let uri = (segments |> Array.toList |> List.tail)
       let order = list |> Array.tryFindIndex(fun i -> i.Contains(uri.Tail.Head+".xml"))
       match order with
             | Some(order) -> order
             | None -> 0

   let rec addOrderToResources id xs x:List<Statement> =
        let count = ref 0
        x
        |> List.map (fun s -> 
                      match s with
                      | (FSharp.RDF.P p, O(Node.Uri(o), xr)) -> 
                           if p.ToString() = "nicebnf:hasClinicalMedicinalProductInformation"
                           then
                            let path = "/clinical-medicinal-product-information/clinicalMedicinalProductInformation.xml"
                            xs := List.append !xs [addOrderNode(o, p, getCountFromFile o path)]
                            (FSharp.RDF.P p, O(Node.Uri(o), xr))
                           elif (isEligibleForOrder x p).Value > 1 && p.ToString() = "nicebnf:inheritsFromClass"
                           then
                            let path = "/drug-classes/drugClasses.xml"
                            xs := List.append !xs [addOrderNode(o, p, getCountFromFile o path)]
                            (FSharp.RDF.P p, O(Node.Uri(o), xr))
                           elif (isEligibleForOrder x p).Value > 1 && p.ToString() <> "rdf:type"
                             && o.ToString().Contains(id) = false
                           then
                            (count := !count + 1) 
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

   let rec addOrderToNestedResources id s = match s with
                      | (FSharp.RDF.P p, O(Node.Uri(o), xr)) -> 
                         let xs = ref List.Empty
                         let newXr = xr.Value 
                                         |> List.map(fun s-> match s with
                                               R(s, statements) -> 
                                               R(s, statements 
                                                 |> (addOrderToResources id xs)
                                                 |> List.append xs.Value
                                                 |> List.map (addOrderToNestedResources id)))
                         (FSharp.RDF.P p, O(Node.Uri(o), lazy newXr))
                      | _ -> s
   let addOrder id resources =
     let xs = ref List.Empty
     let id = id.ToString()
     resources
     |> List.map(fun r ->
         match r with
         | R(s, statements) ->
           R(s, statements |> addOrderToResources id xs
                           |> List.append xs.Value
                           |> (fun s-> xs := List.Empty; s)
                           |> List.map (addOrderToNestedResources id)
                           |> List.map (addOrderToResourcesWithinBlankNodes id)))