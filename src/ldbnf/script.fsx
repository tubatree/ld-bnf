#r "../../packages/FSharp.Data/lib/net40/FSharp.Data.dll"
#I "../../packages/FSharp.RDF/lib/"
#r "../../packages/FSharp.RDF/lib/FSharp.RDF.dll"
#r "System.Xml.Linq"

open System.Xml.Linq
open System.Xml.XPath
open System.Linq


#load "./prelude.fs"
#load "./monograph.fs"

open Bnf.Shared
open Bnf.Drug


open System

type StringBuilder = B of (Text.StringBuilder -> unit)

let build (B f) =
    let b = new Text.StringBuilder()
    do f b
    b.ToString ()

type StringBuilderM () =
    let (!) = function B f -> f
    member __.Yield (txt : string) = B(fun b -> b.Append txt |> ignore)
    member __.Yield (c : char) = B(fun b -> b.Append c |> ignore)
    member __.YieldFrom f = f : StringBuilder

    member __.Combine(f,g) = B(fun b -> !f b; !g b)
    member __.Delay f = B(fun b -> !(f ()) b) : StringBuilder
    member __.Zero () = B(fun _ -> ())
    member __.For (xs : 'a seq, f : 'a -> StringBuilder) =
                    B(fun b ->
                        let e = xs.GetEnumerator ()
                        while e.MoveNext() do
                            !(f e.Current) b)
    member __.While (p : unit -> bool, f : StringBuilder) =
                    B(fun b -> while p () do !f b)

let string = new StringBuilderM ()


let xml = drugProvider.GetSamples().[0].XElement.XPathSelectElements("//section[@outputclass='medicinalProduct']").First()

let rec stringify (node:XNode) =
  match node with
    | :? XText as text -> text.Value
    | :? XElement as element ->
         string {
           for node in element.Nodes() -> stringify node
           if (element.Name.ToString() = "p") then
             yield "\n\n"
           } |> build
    | _ -> ""

open Bnf.DrugParser

let bob = Drug.parse
