namespace Bnf

open System
open System.IO
open FSharp.Data
open Nessos.Argu
open FSharp.Control
open FSharpx.Control
open System.Text.RegularExpressions
open FSharp.Collections.ParallelSeq

open Shared

open System.Xml.Linq
open FSharp.Data
open Bnf.Publication
open Bnf.Drug
open Bnf.DrugParser
open Bnf.TreatmentSummary
open Bnf.TreatmentSummaryParser
open Bnf.DrugClassification
open Bnf.DrugClassificationParser
open Bnf.MedicinalForm
open Bnf.MedicinalFormParser
open Bnf.BorderlineSubstance
open Bnf.BorderlineSubstanceParser
open Bnf.Interaction
open Bnf.InteracitonParser
open Bnf.MedicalDeviceType
open Bnf.MedicalDeviceTypeParser
open Bnf.WoundManagement
open Bnf.WoundManagementParser
open Bnf.Content
open Bnf.ContentParser
open FSharp.RDF

open resource
open Assertion
open rdf
open Bnf.DrugRdf
open Bnf.MedicinalFormRdf
open Bnf.TreatmentSummary
open Bnf.TreatmentSummaryRdf
open Bnf.DrugClassificationRdf
open Bnf.BorderlineSubstanceRdf
open Bnf.InteractionRdf
open Bnf.MedicalDeviceTypeRdf
open Bnf.WoundManagementRdf
open Bnf.ContentRdf
open Bnf.PublicationRdf


module Iterator =

  let private fileasync (fileName : string) = async { use! file = File.AsyncOpenText fileName
                                                      return file.ReadToEnd() }
  let private file (fn:string) = File.OpenText fn

  let private xmlFromFileSynch (fileName : string) =
    let file = File.OpenText fileName
    drugProvider.Load file

  /// writes a file
  let private toFile fileName (contents : string) =
     async {
      File.AsyncWriteAllText(fileName, contents) |> Async.Start
      return ()
    }

  let private toFileSynch fileName (contents : string) =
    File.WriteAllText(fileName, contents)
    ()

  let (++) a b = System.IO.Path.Combine(a, b)

  type Arguments =
    | [<Mandatory>] XmlDirectory of string
    | [<Mandatory>] OutputDirectory of string
  with
    interface IArgParserTemplate with
      member s.Usage = 
        match s with
          | XmlDirectory _ -> "Specify a directoy for the source xml"
          | OutputDirectory _ -> "Specify an output directory for the ttl"

  type Work<'a,'b> =
    | Done of 'a
    | NotDone of 'b

  let generate f =
    //get the type from the filename, somehow
    let t = Directory.GetParent(f).Name
    use fi = file f
    let content n = fi |> contentProvider.Load |> Content.parse |> (Graph.fromContent n) |> Done
    //parse in different ways for differnt types
    let m = match t with
            | "drug" -> fi |> drugProvider.Load |> Drug.parse |> Graph.from |> Done
            | "medicinalForm" -> fi |> drugProvider.Load |> MedicinalForm.parse |> Graph.from |> Done
            | "treatmentSummary" -> fi |> tsProvider.Load |> TreatmentSummary.parse |> Graph.from |> Done
            | "drugClassifications" -> fi |> dcProvider.Load |> DrugClassifications.parse |> Graph.from |> Done
            | "drugClass" -> fi |> drugProvider.Load |> DrugClass.parse |> Graph.from |> Done
            | "clinicalMedicinalProductInformation" -> fi |> drugProvider.Load |> CMPI.parse |> Graph.from |> Done
            | "borderlineSubstance" -> fi |> bsProvider.Load |> BorderlineSubstance.parse |> Graph.from |> Done
            | "interaction" -> fi |> inProvider.Load |> InteractionList.parse |> Graph.from |> Done
            | "medicalDeviceType" -> fi |> drugProvider.Load |> MedicalDeviceType.parse |> Graph.from |> Done
            | "woundManagement" -> fi |> wmProvider.Load |> WoundManagement.parse |> Graph.from |> Done
            | "PHP101868" -> content "dentalPreparations"
            | "PHP101869" -> content "nursePrescribers"
            | "borderlineSubstanceAcbs" -> content "borderlineSubstanceAcbs"
            | "guidance" -> content "guidance"
            | "about" -> content "about"
            | "interactions" -> content "interactions"
            | "publication" -> fi |> drugProvider.Load |> Publication.parse |> Graph.fromPublication |> Done
            | _ -> sprintf "%s %s" t f |> NotDone

    match m with
        | Done graph ->
          let sb = new System.Text.StringBuilder()
          use tw = toString sb
          graph |> Graph.writeTtl tw |> ignore
          let fn = Path.GetFileName f
          let nfn = Path.ChangeExtension(fn,"ttl")
          Done(sb.ToString(),nfn,t)
        | NotDone s -> NotDone s

  let apply o f =
      printfn "%s" f
      match (generate f) with
       | Done(text,fn,t) ->
           let dir = (o ++ t)
           if (not(Directory.Exists(dir))) then
             Directory.CreateDirectory(dir) |> ignore
           let fn = (dir ++ fn)
           toFileSynch fn text
           Done(fn)
       | NotDone s -> NotDone s

  [<EntryPoint>]
  let main args = 
    let parser = ArgumentParser.Create<Arguments>()
    let results = parser.ParseCommandLine(args,errorHandler=ProcessExiter())
    let xmlDirectory = results.GetResult <@ XmlDirectory @>
    let outputDirectory = results.GetResult <@ OutputDirectory @>
    printfn "%s" xmlDirectory

    let fs = Directory.EnumerateFiles(xmlDirectory,"*.*",SearchOption.AllDirectories)
    fs |> Seq.map (apply outputDirectory)
       //|> Seq.choose id
    |> Seq.iter (function
                  | Done s -> s |>  printfn "Processed %s"
                  | NotDone s -> s |>  printfn "Not Done %s" )
    //AsyncSeq.ofSeq fs
    //    |> AsyncSeq.map (apply outputDirectory)
    //    |> AsyncSeq.iter (fun s -> printfn "%s" (Async.RunSynchronously s))
    //    |> Async.RunSynchronously
    0
