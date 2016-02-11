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
open Bnf.MedicalDevice
open Bnf.MedicalDeviceParser
open Bnf.WoundManagement
open Bnf.WoundManagementParser
open Bnf.Generic
open Bnf.GenericParser
open Bnf.BorderlineSubstanceTaxonomy
open Bnf.BorderlineSubstanceTaxonomyParser
open Bnf.Index
open Bnf.IndexParser
open Bnf.Sections
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
open Bnf.GenericRdf
open Bnf.PublicationRdf
open Bnf.MedicalDeviceRdf
open Bnf.BorderlineSubstanceTaxonomyRdf
open Bnf.IndexRdf
open Bnf.SectionsRdf

module Iterator =

  let private fileasync (fileName : string) = async { use! file = File.AsyncOpenText fileName
                                                      return file }
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

  let tottl f t (fi:StreamReader) =
    let content n = fi |> genericProvider.Load |> Generic.parse |> (Graph.fromGeneric n) |> Done
    match t with
            | "drug" -> fi |> drugProvider.Load |> Drug.parse |> Graph.from |> Done
            | "medicinalForm" -> fi |> drugProvider.Load |> MedicinalForm.parse |> Graph.from |> Done
            | "treatment-summary" -> fi |> tsProvider.Load |> TreatmentSummary.parse |> Graph.from |> Done
            | "drug-classifications" -> fi |> dcProvider.Load |> DrugClassifications.parse |> Graph.from |> Done
            | "drug-class" -> fi |> drugProvider.Load |> DrugClass.parse |> Graph.from |> Done
            | "clinical-medicinal-product-information" -> fi |> drugProvider.Load |> CMPI.parse |> Graph.from |> Done
            | "borderline-substance" -> fi |> bsProvider.Load |> BorderlineSubstance.parse |> Graph.from |> Done
            | "interaction" -> fi |> inProvider.Load |> InteractionList.parse |> Graph.from |> Done
            | "medical-device-type" -> fi |> drugProvider.Load |> MedicalDeviceType.parse |> Graph.from |> Done
            | "wound-management" -> fi |> wmProvider.Load |> WoundManagement.parse |> Graph.from |> Done
            | "PHP101868" -> content "DentalPractitionersFormulary"
            | "PHP101869" -> content "NursePrescribersFormulary"
            | "borderlineSubstanceAcbs" -> content "BorderlineSubstanceAcbs"
            | "guidance" -> fi |> tsProvider.Load |> TreatmentSummary.parse |> Graph.from |> Done
            | "about" -> fi |> tsProvider.Load |> TreatmentSummary.parse |> Graph.from |> Done
            | "interactions" -> content "InteractionsIntroduction"
            | "labels" -> content "Labels"
            | "cautionaryAndAdvisoryLabels" -> content "CautionaryAndAdvisoryLabels"
            | "publication" -> fi |> drugProvider.Load |> Publication.parse |> Graph.fromPublication |> Done
            | "medical-device" -> fi |> drugProvider.Load |> MedicalDevice.parse |> Graph.frommedicaldevice |> Done
            | "borderlineSubstanceTaxonomy" -> fi |> drugProvider.Load |> BorderlineSubstanceTaxonomy.parse |> Graph.from |> Done
            | "medicalDevices" -> fi |> indexProvider.Load |> Index.parse |> (Graph.fromindex "MedicalDevice") |> Done
            | "borderlineSubstances" -> fi |> indexProvider.Load |> Index.parse |> Graph.fromindex "BorderlineSubstance" |> Done
            | "electrolytes" -> fi |> sectionProvider.Load |> FluidAndElectrolytes.parse |> Graph.fromFluidAndElectrolytes |> Done
            | "parenteralFeeding" -> fi |> sectionProvider.Load |> ParenteralFeeding.parse |> Graph.fromParenteralFeeding |> Done
            | "hrtRisks" -> fi |> sectionProvider.Load |> HrtRisks.parse |> Graph.fromHrtRisks |> Done
            | "bloodMonitoringStrips" -> fi |> sectionProvider.Load |> BloodMonitoringStrips.parse |> Graph.fromBloodMonitoringStrips |> Done
            | "antiTuberculosisTreatments" -> fi |> sectionProvider.Load |> AntiTuberculosisTreatments.parse |> Graph.fromAntiTuberculosisTreatments |> Done
            | "helicobacterPyloriRegimens" -> fi |> sectionProvider.Load |> HelicobacterPyloriRegimens.parse |> Graph.fromHelicobacterPyloriRegimens |> Done
            | "malariaProphylaxisRegimens" -> fi |> sectionProvider.Load |> MalariaProphylaxisRegimens.parse |> Graph.fromMalariaProphylaxisRegimens |> Done
            | "intramuscularAdrenalineEmergency" -> fi |> sectionProvider.Load |> IntramuscularAdrenalineEmergency.parse |> Graph.fromIntramuscularAdrenalineEmergency |> Done
            | _ -> sprintf "%s %s" t f |> NotDone

  let writettl graph path t =
    let sb = new System.Text.StringBuilder()
    use tw = toString sb
    graph |> Graph.writeTtl tw |> ignore
    let fn = Path.GetFileName path
    let nfn = Path.ChangeExtension(fn,"ttl")
    Done(sb.ToString(),nfn,t)

  let gena f = async {
       let t = Directory.GetParent(f).Name
       let! fi = fileasync f
       let m = tottl f t fi

       return match m with
              | Done graph -> writettl graph f t
              | NotDone s -> NotDone s
    }


  let generate f =
    //get the type from the filename, somehow
    let t = Directory.GetParent(f).Name
    use fi = file f
    let m = tottl f t fi

    match m with
        | Done graph -> writettl graph f t
        | NotDone s -> NotDone s

  let apply o f =
    try
      match (generate f) with
       | Done(text,fn,t) ->
           let dir = (o ++ t)
           if (not(Directory.Exists(dir))) then
             Directory.CreateDirectory(dir) |> ignore
           let fn = (dir ++ fn)
           toFileSynch fn text
           Done(fn)
       | NotDone s -> NotDone s
     with
       | ex -> NotDone (sprintf "%s failed with %s %s" f ex.Message ex.StackTrace)

  let applya o f = async {
      try
        match (generate f) with
         | Done(text,fn,t) ->
           let dir = (o ++ t)
           if (not(Directory.Exists(dir))) then
             Directory.CreateDirectory(dir) |> ignore
           let fn = (dir ++ fn)
           do! toFile fn text
           return Done(fn)
         | NotDone s -> return NotDone s
      with
         | ex -> return NotDone (sprintf "%s failed with %s %s" f ex.Message ex.StackTrace)
    }



  [<EntryPoint>]
  let main args =
    let parser = ArgumentParser.Create<Arguments>()
    let results = parser.ParseCommandLine(args,errorHandler=ProcessExiter())
    let xmlDirectory = results.GetResult <@ XmlDirectory @>
    let outputDirectory = results.GetResult <@ OutputDirectory @>
    printfn "%s" xmlDirectory

    let fs = Directory.EnumerateFiles(xmlDirectory,"*.*",SearchOption.AllDirectories)

    let f = function
            | Done _ -> ()
            | NotDone s -> s |>  printfn "%s unprocessed"

    let stopWatch = System.Diagnostics.Stopwatch.StartNew()

    //fs |> Seq.map (apply outputDirectory)
    //   |> Seq.iter f


    //AsyncSeq.ofSeq fs
    //    |> AsyncSeq.map (applya outputDirectory)
    //    |> AsyncSeq.iter (fun s -> Async.RunSynchronously s |> f)
    //    |> Async.RunSynchronously

    fs |> PSeq.map (apply outputDirectory)
       |> PSeq.iter f

    //fs |> PSeq.map (applya outputDirectory)
    //   |> PSeq.iter (fun s -> Async.RunSynchronously s |> f)

    stopWatch.Stop()
    printfn "%f" stopWatch.Elapsed.TotalMilliseconds
    0
