namespace Bnf
open System.IO
open System
open System.Xml.Linq
open System.Xml.XPath

module Utils =
  let file (fn:string) = 
     use f = File.OpenText fn
     f.ReadToEnd()
  let xmlDirectory = Environment.GetCommandLineArgs() |> Array.filter (fun x-> x.Contains("xml")) |> Array.filter (fun x-> x <> "--xmldirectory")
  let filePath = xmlDirectory.[0]+"/drug/"
  let files  = Directory.EnumerateFiles(filePath, @"*.xml")
  let classifications = files |> Seq.map(fun x-> file(filePath+Path.GetFileName(x)))
                              |> Seq.map(fun x-> x |> XDocument.Parse
                                                    |> (fun x -> x.XPathSelectElements(".//data[@name='classifications']"))
                                                    |> Seq.map(fun f -> f.ToString())
                                                    |> System.String.Concat)
                              |> System.String.Concat
                              |> (fun x -> "<root>" + x + "</root>")
                              |> XDocument.Parse
  let parseClassfications (id) =
    let result = classifications.XPathSelectElements(".//data[@name='classifications']//data[@name='drugClassification' and text()='"+id+"']")  |> Seq.toList
    result