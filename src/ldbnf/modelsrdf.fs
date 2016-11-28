namespace Bnf
open FSharp.RDF
open FSharp.Data.Runtime
open prelude
open resource
open Assertion
open Rdf
open Shared
open rdf
open RdfUris


module MedicalDeviceRdf =
  open MedicalDevice
  open Bnf.Order

  type Graph with
    static member frommedicaldevice(MedicalDevice(id,t,pdi,ids)) =
      //horrible nested optional lists
      let f (PrescribingAndDispensingInformation(t,sd)) =
        blank !!"nicebnf:hasPrescribingAndDispensingInformation" (( sd |> dita ) @ (t <!> xtitle |> unwrap))
      let s = optionlist {
        yield a Uri.MedicalDeviceEntity
        yield! t |> xtitle
        yield pdi <!> f
        yield! ids |> List.map (Uri.frommdt >> (objectProperty !!"nicebnf:hasMedicalDeviceType"))
        }
      let dr = resource (Uri.frommd id)
      [dr s]
      |> addOrder (Uri.frommd id)
      |> Assert.graph (empty())

module BorderlineSubstanceTaxonomyRdf =
  open BorderlineSubstanceTaxonomy
  open Bnf.Order

  type Graph with
    static member from(x:BorderlineSubstanceTaxonomy) =
      let s = optionlist {
        yield a Uri.BorderlineSubstanceTaxonomyEntity
        yield! x.title |> xtitle
        yield! x.general <!> dita |> unwrap
        yield! x.substances |> List.map (Uri.frombsc >> (objectProperty !!"nicebnf:hasBorderlineSubstance"))
        yield! x.categories |> List.map (Uri.frombst >> (objectProperty !!"nicebnf:hasBorderlineSubstanceTaxonomy"))
        yield x.acbs <!> (string >> xsd.xmlliteral >> (dataProperty !!"nicebnf:hasAcbsIndication"))
        }

      let dr = resource (Uri.frombst x.id)
      [dr s]
      |> addOrder (Uri.frombst x.id)
      |> Assert.graph (empty())

module PublicationRdf =
  open Bnf.Publication
  open Bnf.Order
  type Graph with
    static member fromPublication (Publication(d,wmid,BorderlineSubstanceTaxonomyId(bsid))) =
      let dto = (System.DateTimeOffset d)^^xsd.datetime

      let wm wmid = one !!"nicebnf:hasWoundManagement" (Uri.fromwm wmid)
                      [a !!(Uri.nicebnfClass + "WoundManagementRoot")]

      let bs (bsid:Id) = one !!"nicebnf:hasBorderlineSubstanceTaxonomyRoot" (Uri.frombsc bsid)
                          [a !!(Uri.nicebnfClass + "BorderlineSubstanceTaxonomyRoot")]

      let s = [a !!(Uri.nicebnf + "Publication")
               dto |> (dataProperty !!"nicebnf:hasPublicationDate")
               wmid |> wm
               bsid |> bs]

      let dr = resource !!(Uri.bnfsite + "publication")
      [dr s]
      |> addOrder !!(Uri.bnfsite + "publication")
      |> Assert.graph (empty())

module GenericRdf =
  open Bnf.Generic

  type Graph with

    static member fromta (TargetAudience s) =
      dataProperty !!"nicebnf:hasTargetAudience" (s^^xsd.string)

    static member fromcontent (Content(s,ta)) =
      blank !!"nicebnf:hasContent"
        (optionlist {
          yield ta >>= (Graph.fromta >> Some)
          yield! s |> dita})

    static member from (x:ContentLink) =
      objectProperty !!"nicebnf:hasLink" (Uri.totopic (x.href))

    static member fromGeneric n (x:Generic) =
      let uri n id = !!(sprintf "%s%s/%s" Uri.bnfsite n (string id))

      let s = optionlist {
        yield a !!(Uri.nicebnf + n)
        yield! x.title |> xtitle
        yield! x.links |> Seq.map Graph.from |> Seq.toList
        yield! x.content |> List.map Graph.fromcontent}

      let dr r = resource (uri n x.id) r

      [dr s]
      |> Assert.graph (empty())

module IndexRdf =
  open Bnf.Index
  open Bnf.Order

  type Graph with
    static member fromindex n (Index(id,ids)) =

      let uri n id = !!(sprintf "%s%s/%s" Uri.bnfsite (splitCamelCase n) (string id))

      let s = optionlist {
        yield a !!(sprintf "%s%ss" Uri.nicebnf n)
        yield! ids |> List.map ((uri n) >> objectProperty !!("nicebnf:has" + n))
        }

      let dr r = resource (uri (sprintf "%ss" n) id) r

      [dr s]
      |> addOrder (uri (sprintf "%ss" n) id)
      |> Assert.graph (empty())


module InteractionRdf =
  open Bnf.Interaction
  open Bnf.Order
  type Graph with
    static member from (InteractionList(id,t,il,ids,n)) =
      let note (Note(p,t)) = blank !!"nicebnf:hasNote"
                              ((t |> (toString >> xsd.string >> dataProperty !!"nicebnf:hasNoteType")) :: (p |> dita))
      let s = optionlist{
                yield a Uri.InteractionListEntity
                yield t.XElement.Value |> label
                yield t.XElement |> (string >> title)
                yield n >>= (note >> Some)}

      let iwuri = Uri.fromiw id

      let importance i =
        match i.importance with
          | High -> dataProperty !!"nicebnf:hasImportance" ("High"^^xsd.string)
          | NotSet -> dataProperty !!"nicebnf:hasImportance" ("NotSet"^^xsd.string)

      let interactionDetail i = one !!"nicebnf:hasInteraction" (iwuri i)
                                 (optionlist {
                                   yield a Uri.InteractionEntity
                                   yield objectProperty !!"nicebnf:interactsWith" (Uri.fromiwl i)
                                   yield importance i
                                   yield! i.message |> dita
                                   yield i.message.XElement.Value |> (string >> label)
                                   yield dataProperty !!"nicebnf:hasImportance" ((string i.importance)^^xsd.string)
                                  })

      let link i = one !!"nicebnf:hasInteractionList" (Uri.fromil i) [objectProperty !!"nicebnf:isInteractionListOf" (Uri.fromil id)]

      let dr r = resource (Uri.fromil id) r
      [dr s
       dr (il |> List.map interactionDetail)
       dr (ids |> List.map link)]
       |> addOrder (Uri.fromil id)
       |> Assert.graph (empty())

module BorderlineSubstanceRdf =
  open Bnf.BorderlineSubstance
  open Bnf.Order

  let inline dpo n x = x <!> (string >> xsd.string >> (dataProperty !!("nicebnf:has" + n)))

  type Graph with
    static member from (x:BorderlineSubstance) =
      let s =  optionlist {
                yield a Uri.BorderlineSubstanceEntity
                yield! x.title |> xtitle
                yield x.category |> (string >> Uri.frombst >> objectProperty !!"nicebnf:hasCategory")
                yield x.intro <!> (string >> xsd.string >> dataProperty !!"nicebnf:hasIntroductoryNote")}

      let ds = x.details |> List.map Graph.fromdetails

      let dr r = resource (Uri.from x) r
      [dr s
       dr ds]
       |> addOrder (Uri.from x)
       |> Assert.graph (empty())


    static member frompackinfo (PackInfo(ps,uom,acbs)) =
      optionlist {
        yield ps |> dpo "PackSize"
        yield uom |> dpo "UnitOfMeasure"
        yield acbs |> dpo "Acbs"}

    static member fromnhsindicativeinfo (NhsIndicativeInfo(nhsi,pt,nhsip)) =
      optionlist {
        yield nhsi |> dpo "NhsIndicative"
        yield pt |> dpo "PriceText"
        yield nhsip |> dpo "NhsIndicativePrice"}

    static member frompricetarrif (PackSizePriceTariff(pi,nhs)) =
      let s = [pi >>= (Graph.frompackinfo >> Some)
               nhs >>= (Graph.fromnhsindicativeinfo >> Some)] |> List.choose id
      blank !!"nicebnf:hasPack" (s |> List.collect id)

    static member fromprep (BorderlineSubstancePrep(t,pts)) =
      let s = match t with
              | Some (PreparationTitle(p,m)) ->
                optionlist {
                  yield! p |> xtitle
                  yield m |> dpo "Manufacturer"}
              | None -> []
      let ts = pts |> List.map Graph.frompricetarrif
      blank !!"nicebnf:hasBorderlineSubstancePrep" (s @ ts)

    static member fromdetail (x:Detail) =
      let inline dp n s = dataProperty !!("nicebnf:has" + n) ((string s)^^xsd.string)
      let inline dpx n s = dataProperty !!("nicebnf:has" + n) ((string s)^^xsd.xmlliteral)
      let acbs (p:bsProvider.P) =
        let link = p.Xref <!> (fun xr ->
                               xr.XElement.Remove()
                               objectProperty !!("nicebnf:hasACBSNote") !!("bnfsite:" + xr.Href.Replace(".xml","")))
        blank !!"nicebnf:hasAcbsWarning"
          (optionlist {
            yield! p |> dita
            yield link})
      match x with
        | Formulation s -> s |> dp "Formulation"
        | EnergyKj e -> e |> dp "EnergyKj"
        | EnergyKcal e -> e |> dp "EnergyKcal"
        | ProteinGrams p -> p |> dp "ProteinGrams"
        | ProteinConstituents p -> p |> dp "ProteinConstituents"
        | CarbohydrateGrams c -> c |> dp "CarbohydrateGrams"
        | CarbohydrateConstituents c -> c |> removebrackets |> dp "CarbohydrateConstituents"
        | FatGrams f -> f |> dp "FatGrams"
        | FatConstituents f -> f |> dp "FatConstituents"
        | FibreGrams f -> f |> dp "FibreGrams"
        | SpecialCharacteristics s -> s |> dp "SpecialCharacteristics"
        | Acbs a -> a |> acbs
        | Presentation p -> p |> dpx "Presentation"
        | PresentationNote p -> p |> dpx "PresentationNote"
        | RxAdvice p -> p |> dp "RxAdvice"

    static member fromdetails (Details(ds,bsps)) =
      let dps = ds |> List.map Graph.fromdetail
      let preps = bsps |> List.map Graph.fromprep
      blank !!"nicebnf:hasDetails" (dps @ preps)


module DrugClassificationRdf =
  open DrugClassification

  type Graph with
    static member from (DrugClassifications cs) =
      cs |> List.map Graph.from |> Assert.graph (empty())

    static member from (x:Classification) =
      resource !!(Uri.nicebnfClass + "Classification#" + x.key) [x.value |> label]

module TreatmentSummaryRdf =
  open Bnf.TreatmentSummary
  open Bnf.Order
  type Graph with
    static member from (x:TreatmentSummary) =
      let isa  (TreatmentSummary (_,x)) =
        match x with
          | Generic _ ->  [a Uri.TreatmentSummaryEntity]
          | About _ | Guidance _ -> [a !!(Uri.nicebnf + (toString x))]
          | _ -> [a !!(Uri.nicebnf + (toString x))
                  a Uri.TreatmentSummaryEntity]

      let s = optionlist {
               yield! isa x}

      let p = Graph.fromts (Uri.from x) x
      let dr r = resource (Uri.from x) r
      [dr s
       dr p] |> addOrder (Uri.from x) |> Assert.graph (empty())

    static member fromdoi (Shared.Doi s)=
      dataProperty !!"nicebnf:hasDoi" (s^^xsd.string)
    static member frombs url (BodySystem x) =
        one !!"nicebnf:hasBodySystem" (Uri.frombs x) (optionlist {
          yield a Uri.BodySystemEntity
          yield dataProperty !!"rdfs:label" (x^^xsd.string)
          yield objectProperty !!"nicebnf:isBodySystemOf" url
          })

    static member fromta (TargetAudience s) =
      dataProperty !!"nicebnf:hasTargetAudience" (s^^xsd.string)
    static member fromcontent c =
      match c with
        | Content(s,ta) ->
            blank !!"nicebnf:hasContent"
             (optionlist {
              yield ta >>= (Graph.fromta >> Some)
              yield! s |> dita})
        | LabelContent(l) ->
            blank !!"nicebnf:hasLabel"
             (optionlist {
              yield l.number <!> (xsd.string >> dataProperty !!"nicebnf:hasNumber")
              yield l.recommendation <!> (string >> xsd.xmlliteral >> dataProperty !!"nicebnf:hasRecommendation")
              yield l.description <!> (string >> xsd.xmlliteral >> dataProperty !!"nicebnf:hasDescription")
              })

    static member fromlink url (x:ContentLink) =
      one !!"nicebnf:hasLink" (Uri.totopic (x.href))
        [objectProperty !!"nicebnf:isLinkedFrom" url] //create a back link at the same time

    static member fromsummary url (x:Summary) =
      let xr (xref:tsProvider.Xref) =
        let id = Id(xref.Href)
        match xref.Rel with
        | Some rel -> objectProperty !!("nicebnf:has" + (firstupper rel)) (Uri.fromhref rel id) |> Some
        | None -> None

      optionlist {
        yield! x.title |> xtitle
        yield x.doi <!> Graph.fromdoi
        yield x.bodySystem <!> (Graph.frombs url)
        yield! x.links |> Seq.map (Graph.fromlink url) |> Seq.toList
        yield! x.content |> List.map Graph.fromcontent
        yield! x.sublinks |> List.choose xr
        }

    static member fromts url (TreatmentSummary (_,x)) =
      match x with
        | ComparativeInformation s -> Graph.fromsummary url s
        | ManagementOfConditions s -> Graph.fromsummary url s
        | MedicalEmergenciesBodySystems s -> Graph.fromsummary url s
        | TreatmentOfBodySystems s -> Graph.fromsummary url s
        | About s -> Graph.fromsummary url s
        | Guidance s -> Graph.fromsummary url s
        | Generic s -> Graph.fromsummary url s

module MedicinalFormRdf =
  open Bnf.Drug
  open Bnf.MedicinalForm
  open FSharpx.Control
  open System.IO
  open DrugParser
  open Shared
  open System
  open Bnf.Order
  type Graph with
    static member from (x:MedicinalForm) =
      let s = optionlist{
                yield a Uri.MedicinalFormEntity
                yield! x.title |> xtitle
                yield x.excipients >>= Graph.fromexc
                yield x.electrolytes >>= Graph.fromele}

      let cals = match x.cautionaryAdvisoryLabels with
                 | Some x -> Graph.fromcals x
                 | None -> []

      let cmpis = x.cmpis |> List.map Graph.fromclinicalmpi
      let count = ref 0
      let cmpiOrdering = x.cmpis |> List.map (fun x -> Graph.ClinicalMedicinalProductInformationOrder(x, count))
      let mpsCount = ref 0 
      let mps = x.medicinalProducts |> List.map (fun x -> Graph.fromCount(x, mpsCount))
      let dr r = resource (Uri.from x) r
      [dr s
       dr mps
       dr cals
       dr cmpis]
       |> addOrder (Uri.from x) |> Assert.graph (empty())

    static member fromclinicalmpi x = objectProperty !!"nicebnf:hasClinicalMedicinalProductInformation" (Uri.fromcmpi x)

    static member ClinicalMedicinalProductInformationOrder (x, count) =
        count := !count + 1
        let getProducts (x:drugProvider.Topic) =
          x.Xrefs |> Array.map (fun xref -> xref.Href)

        let file (fn:string) = File.OpenText fn
        let xmlDirectory = Environment.GetCommandLineArgs() |> Array.filter (fun x-> x.Contains("xml")) |> Array.filter (fun x-> x <> "--xmldirectory")
        let fi = file(xmlDirectory.[0]+"/clinical-medicinal-product-information/clinicalMedicinalProductInformation.xml")

        let cmpiList (fi:StreamReader) =
          fi |> drugProvider.Load |> getProducts

        let cmpis = cmpiList fi
        let q =  x.GetType().GetProperty("Item").GetValue(x, null) |> string
        let order q = 
            let c = cmpis |> Array.tryFindIndex(fun i -> i.Contains(q+".xml"))
            match c with
            | Some c -> c
            | None -> count.Value

        blank !!"nicebnf:hasClinicalMedicinalProductInformationOrder"
                 (optionlist {
                  yield dataProperty !!"nicebnf:hasOrder" ((order q).ToString()^^xsd.string)
                  yield dataProperty !!"nicebnf:hasClinicalMedicinalProductInformation" ((Uri.fromcmpi x).ToString()^^xsd.string)
                  })
    static member fromcal (CautionaryAdvisoryLabel(ln,p)) =
      blank !!"nicebnf:hasCautionaryAdvisoryLabel"
               (optionlist {
                 yield! p |> dita
                 yield ln >>= (string >> xsd.string >> (dataProperty !!"nicebnf:hasLabelNumber") >> Some)})

    static member fromcals (CautionaryAdvisoryLabels(_,cals)) =
      cals |> Array.map Graph.fromcal |> Array.toList

    static member dp n = xsd.string >> (dataProperty !!("nicebnf:has" + n))

    static member fromman (Manufacturer x) = Graph.dp "Manufacturer" x |> Some
    static member frombt (BlackTriangle x) = Graph.dp "BlackTriangle" x |> Some
    static member frommpt (MedicinalProductTitle(m,bt,t)) =
      optionlist {
        yield m >>= Graph.fromman
        yield bt >>= Graph.frombt
        yield! t |> xtitle}

    static member fromexc (Excipients e) =
      e |> (string >> xsd.xmlliteral >> (dataProperty !!"nicebnf:hasExcipients") >> Some)

    static member fromele (Electrolytes e) =
      e |> (string >> xsd.xmlliteral >> (dataProperty !!"nicebnf:hasElectrolytes") >> Some)

    static member fromsai(StrengthOfActiveIngredient p) = Graph.dp "StrengthOfActiveIngredient" (string p) |> Some
    static member fromcd(ControlledDrug p) = Graph.dp "ControlledDrug" (string p) |> Some

    static member fromnhsi (NhsIndicative x) = Graph.dp "NhsIndicative" x |> Some
    static member frompt (PriceText x) = Graph.dp "PriceText" x |> Some
    static member fromnhsip (NhsIndicativePrice x) = Graph.dp "NhsIndicativePrice" (string x) |> Some
    static member fromhos (Hospital x) = Graph.dp "Hospital" x |> Some
    static member fromnhsii (NhsIndicativeInfo(nhsi,pt,nhsip,hos)) =
      optionlist {
        yield nhsi >>= Graph.fromnhsi
        yield pt >>= Graph.frompt
        yield nhsip >>= Graph.fromnhsip
        yield hos >>= Graph.fromhos}

    static member fromps (PackSize d) = Graph.dp "PackSize" (string d) |> Some
    static member fromuom u = Graph.dp "UnitOfMeasure" (string u) |> Some
    static member fromlc lc = Graph.dp "LegalCategory" (string lc) |> Some
    static member fromacbs (Acbs x) = Graph.dp "Acbs" x |> Some
    static member frompackinfo (PackInfo(ps,uom,lc,acbs)) =
      optionlist {
       yield ps >>= Graph.fromps
       yield uom >>= Graph.fromuom
       yield lc >>= Graph.fromlc
       yield acbs >>= Graph.fromacbs}

    static member fromdt (DrugTarrif s) = Graph.dp "DrugTarrif" s |> Some
    static member fromdtp (DrugTariffPrice dtp) = Graph.dp "DrugTariffPrice" (string dtp) |> Some
    static member fromdti (DrugTariffInfo(dt,pt,dtp)) =
      optionlist {
       yield dt >>= Graph.fromdt
       yield pt >>= Graph.frompt
       yield dtp >>= Graph.fromdtp}

    static member frompack(Pack(pi,nii,dti,li)) =
      let u = Pack(pi,nii,dti,li) |> Uri.fromcontent <| li
      one !!"nicebnf:hasPack" u
        (optionlist {
          yield! pi <!> Graph.frompackinfo |> unwrap
          yield! nii <!> Graph.fromnhsii |> unwrap
          yield! dti <!> Graph.fromdti |> unwrap})

    static member from (x:MedicinalProduct) =
      one !!"nicebnf:hasMedicinalProduct" (Uri.from x)
        (optionlist {
          yield a Uri.MedicinalProductEntity
          yield x.ampid |> string |> Graph.dp "Ampid"
          yield! x.title |> Graph.frommpt
          yield! x.strengthOfActiveIngredient |> List.choose Graph.fromsai
          yield! x.controlledDrugs |> List.choose Graph.fromcd
          yield! x.packs |> List.map Graph.frompack
          })

    static member fromCount (x:MedicinalProduct, count) =
      count := !count + 1
      one !!"nicebnf:hasMedicinalProduct" (Uri.from x)
        (optionlist {
          yield a Uri.MedicinalProductEntity
          yield x.ampid |> string |> Graph.dp "Ampid"
          yield! x.title |> Graph.frommpt
          yield! x.strengthOfActiveIngredient |> List.choose Graph.fromsai
          yield! x.controlledDrugs |> List.choose Graph.fromcd
          yield! x.packs |> List.map Graph.frompack
          })

module WoundManagementRdf =
  open Bnf.WoundManagement
  open Bnf.Order
  let dp n = xsd.string >> dataProperty !!("nicebnf:has" + n)

  type Graph with
    static member setupGraph = Graph.ReallyEmpty ["nicebnf",!!Uri.nicebnf
                                                  "rdfs",!!"http://www.w3.org/2000/01/rdf-schema#"
                                                  "bnfsite",!!Uri.bnfsite]

    static member from (x:WoundManagement) =
      let s = optionlist {
               yield a Uri.WoundManagementEntity
               yield x.general <!> (string >> xsd.xmlliteral >> dataProperty !!"nicebnf:hasGeneral")
               yield! x.title |> xtitle
               yield! x.dressingChoices |> List.map Graph.fromWoundType
               yield! x.productGroups |> List.map Graph.fromProductGroup
               yield! x.links |> List.map Graph.fromwml
              }

      let dr r = resource (Uri.from x) r

      [dr s]
      |> addOrder (Uri.from x) |> Assert.graph Graph.setupGraph

    static member fromDescription (Description sd) = sd |> dita

    static member fromwml (x:WoundManagementLink) =
      objectProperty !!"nicebnf:hasWoundManagement" (Uri.from x)

    static member fromExudate (WoundExudate(s,wmls)) =
      blank !!"nicebnf:hasWoundExudate"
        (dataProperty !!"nicebnf:hasRate" (s^^xsd.string) :: (wmls |> List.map Graph.fromwml))

    static member fromWoundType (WoundType(TypeOfWound(t),d,wes)) =
      blank !!"nicebnf:hasWoundType"
        (optionlist {
            yield t |> dp "TypeOfWound"
            yield! d <!> Graph.fromDescription |> unwrap
            yield! wes |> List.map Graph.fromExudate
          })

    static member fromProduct (x:Product) =
      one !!"nicebnf:hasProduct" (Uri.from x)
       [x.manufacturer |> dp "Manufacturer"
        x.name |> dp "Name"
        x.price |> (string >> (dp "Price"))]

    static member fromProductGroup (ProductGroup(t,d,pl)) =
      blank !!"nicebnf:hasProductGroup"
        (optionlist {
            yield! t |>  xtitle
            yield! d <!> Graph.fromDescription |> unwrap
            yield! pl |> List.map Graph.fromProduct
            })

module ClinicalMedicalDeviceInformationGroupRdf =
  open ClinicalMedicalDeviceInformationGroup
  open Bnf.Drug
  open Bnf.MedicinalForm
  open Bnf.MedicalDeviceType
  open DrugRdf
  open MedicinalFormRdf
  open Bnf.Order
  type Graph with
    static member from (x:ClinicalMedicalDeviceInformationGroup) = 
      let uri = Uri.fromcmdig_id x

      let s =  optionlist {
                yield a Uri.ClinicalMedicalDeviceInformationGroupEntity
                yield! x.title |> xtitle
                yield x.description <!> (Graph.fromdd uri)
                yield x.complicance <!> (Graph.fromcs uri)}

      let sec = Graph.fromsec uri
      let ss = x.sections |> List.collect sec
      let mps = x.products |> List.map Graph.from

      let dr r = resource (Uri.fromcmdig x) r
      [dr ss
       dr mps
       dr s]
       |> Assert.graph Graph.setupGraph

    static member fromdd uri (DeviceDescription(id,sd)) =
      one !!"nicebnf:hasDeviceDescription" (uri id) (sd |> dita)

    static member fromcs uri (ComplicanceStandards(id,sd)) =
      one !!"nicebnf:hasComplicanceStandards" (uri id) (sd |> dita)

module MedicalDeviceTypeRdf =
  open Bnf.Drug
  open Bnf.MedicinalForm
  open Bnf.MedicalDeviceType
  open DrugRdf
  open MedicinalFormRdf
  open Bnf.Order

  type Graph with
    static member from (x:MedicalDeviceType) =
      let s = optionlist {
               yield a Uri.MedicalDeviceTypeEntity
               yield! x.title |> xtitle
               yield! x.groups |> List.map Graph.fromcmdig
               yield! x.products |> List.map Graph.fromproducts
               }

      let dr r = resource (Uri.from x) r
      [dr s]
      |> addOrder (Uri.from x)
      |> Assert.graph Graph.setupGraph

    static member fromproducts (x:MedicinalProduct) = Graph.from x

    static member fromcmdig (x:Href) =
      objectProperty !!"nicebnf:hasClinicalMedicalDeviceInformationGroup" (Uri.fromcmdig (string x))


module SectionsRdf =
  open Sections

  open Bnf.Drug
  open DrugRdf
  open MedicinalFormRdf
  open Bnf.Order

  let inline dp n = xsd.string >> (dataProperty !!("nicebnf:has" + (n |> titleCase)))

  let inline xdp n = string >> xsd.xmlliteral >> (dataProperty !!("nicebnf:has" + (n |> titleCase)))

  let ti  = function
             | XmlTitle p -> p |> xtitle

  type Graph with
    static member fromFluidAndElectrolytes (x:FluidAndElectrolytes) =
      let concentrations (ElectrolyteConcentrations(t,npv,iis)) =
        let normalplasmavalues (x:NormalPlasmaValues) =
          let intravenous (x:IntravenousInfusion) =
            blank (Uri.has x) (optionlist {
              yield! x.title |> ti
              yield x.sodium <!> (dp "sodium")
              yield x.potassium <!> (dp "potassium")
              yield x.bicarbonate <!> (dp "bicarbonate")
              yield x.chloride <!> (dp "chloride")
              yield x.calcium <!> (dp "calcium")
              yield x.forMetabolicAcidosis <!> (string >> dp "forMetabolicAcidosis")
              })

          blank (Uri.has x) (optionlist {
            yield! x.title |> ti
            yield x.sodium |> (dp "sodium")
            yield x.potassium |> (dp "potassium")
            yield x.bicarbonate |> (dp "bicarbonate")
            yield x.chloride |> (dp "chloride")
            yield x.calcium |> (dp "calcium")
            yield! iis |> List.map intravenous
            })

        blank (Uri.has<ElectrolyteConcentrations>()) (optionlist {
            yield! t |> ti
            yield npv |> normalplasmavalues
          })

      let content (ElectrolyteContent(title,fluids)) =
        let fluid (x:TypeOfFluid) =
          blank (Uri.has x) (optionlist {
            yield! x.title |> ti
            yield x.hydrogen <!> (dp "hydrogen")
            yield x.sodium <!> (dp "sodium")
            yield x.potassium <!> (dp "potassium")
            yield x.bicarbonate <!> (dp "bicarbonate")
            yield x.chloride <!> (dp "chloride")
            })

        blank (Uri.has<ElectrolyteContent>()) (optionlist{
          yield! title |> ti
          yield! fluids |> List.map fluid
          })

      let s = optionlist {
               yield a (Uri.TypeEntity x)
               yield! x.title <!> ti |> unwrap
               yield x.concentrations |> concentrations
               yield x.content |> content
               }
      let dr = resource (Uri.fromtype<FluidAndElectrolytes> (string x.id))

      [dr s]
       |> addOrder (Uri.fromtype<FluidAndElectrolytes> (string x.id))
       |> Assert.graph Graph.setupGraph


  type Graph with
    static member fromParenteralFeeding (ParenteralFeeding(id,en,pl)) =
      let notes (EnergyNotes(sd)) = sd |> dita
      let pack (Pack(sd)) =
        blank (Uri.has<Pack>()) (optionlist {
          yield! sd |> dita
          })

      let preparation (x:Preparation) =
        blank (Uri.has x) (optionlist{
          yield! x.title |> ti
          yield x.manufacturer |> (dp "manufacturer")
          yield x.nitrogen <!> (dp "nitrogen")
          yield x.energy <!> (dp "energy")
          yield x.potassium <!> (dp "potassium")
          yield x.magnesium <!> (dp "magnesium")
          yield x.sodium <!> (dp "sodium")
          yield x.acetate <!> (dp "acetate")
          yield x.chloride <!> (dp "chloride")
          yield x.otherComponentsPerLitre <!> (xdp "otherComponentsPerLitre")
          yield x.adultOnly <!> (string >> dp "adultOnly")
          yield! x.packs |> List.map pack
          })

      let s = optionlist {
        yield! en |> notes
        yield! pl |> List.map preparation
        }

      let dr = resource (Uri.fromtype<ParenteralFeeding> (string id))
      [dr s]
      |> addOrder (Uri.fromtype<ParenteralFeeding> (string id))
      |> Assert.graph Graph.setupGraph

  type Graph with
    static member fromHrtRisks (HrtRisks(id,note,risks)) =
      let n (Note(sd)) = sd |> dita

      let risk (Risk(title,note,groups)) =
        let group (Group(ar,incedenceslist)) =
          let incedences (Incedences(_,typ,incedencelist)) =
            let incedence name (Incedence(duration,count)) =
              blank !!( "nicebnf:has" + (firstupper name)) (optionlist{
                yield duration |> (string >> dp "duration")
                yield count |> (dp "count")
                })

            incedencelist |> List.map (incedence ( typ |> toString ))

            //blank (Uri.has<Incedences>()) (optionlist{
            //  yield! incedencelist |> List.map (incedence ( typ |> toString ))
            //  })

          blank (Uri.has<Group>()) (optionlist{
            yield ar |> (string >> dp "AgeRange")
            yield! incedenceslist |> List.collect incedences
            })

        blank (Uri.has<Risk>()) (optionlist{
          yield! title |> ti
          yield! note <!> n |> unwrap
          yield! groups |> List.map group
          })

      let s = optionlist {
          yield! note |> n
          yield! risks |> List.map risk
        }

      let dr = resource (Uri.fromtype<HrtRisks> (string id))
      [dr s]
      |> addOrder (Uri.fromtype<HrtRisks> (string id))
      |> Assert.graph Graph.setupGraph

  type Graph with
    static member fromBloodMonitoringStrips (BloodMonitoringStrips(id,title,strips)) =
      let strip (x:BloodMonitoringStrip) =
        let compatibleStrip (x:CompatibleStrip) =
          blank (Uri.has x) (optionlist{
            yield! x.name |> ti
            yield x.packSize |> (dp "packSize")
            yield x.price |> (string >> dp "price")
            })

        blank (Uri.has x) (optionlist{
          yield! x.meter |> dita
          yield x.typeOfMonitoring |> (dp "typeOfMonitoring")
          yield x.sensitivityRange |> (string >> xsd.xmlliteral >> (dataProperty !!"nicebnf:hasSensitivityRange"))
          yield x.manufacturer |> (dp "manufacturer")
          yield x.compatibleStrip |> compatibleStrip
          yield x.note <!> (dp "note")
          })

      let s = optionlist {
        yield! title <!> ti |> unwrap
        yield! strips |> List.map strip
        }

      let dr = resource (Uri.fromtype<BloodMonitoringStrips> (string id))

      [dr s]
      |> addOrder (Uri.fromtype<BloodMonitoringStrips> (string id))
      |> Assert.graph Graph.setupGraph


  type Graph with
    static member fromAntiTuberculosisTreatments (AntiTuberculosisTreatments(id,title,therapies)) =
      let therapy (x:Therapy) =
        let patientgroup (PatientGroup(agegroup,dosage)) =
          //swap to have the dosage with a group
          let grp (x:AgeGroup) =
            let g (p:sectionProvider.P) a =
              one !!"nicebnf:hasPatientGroup" (Uri.fromgrp p.String.Value)
                          (optionlist {
                            yield! p |> xtitle
                            yield objectProperty !!"rdfs:subClassOf" (Uri.fromgrp(a))
                            })
            match x with
              | Adult p -> g p "adult"
              | Child p -> g p "child"

          let dir (Dosage(p)) = p |> dita

          blank (Uri.has<Dosage>()) (optionlist{
            yield agegroup |> grp
            yield! dosage.Value |> dir
            })

        blank (Uri.has x) (optionlist{
          yield x.supervision |> (toString >> dp "supervision")
          yield x.therapyType |> (toString >> dp "therapyType")
          yield! x.title |> ti
          yield x.takenInMonths |> (string >> dp "takenInMonths")
          yield! x.groups |> List.map patientgroup
          })

      let s = optionlist {
        yield! title <!> ti |> unwrap
        yield! therapies |> List.map therapy
        }

      let dr = resource (Uri.fromtype<AntiTuberculosisTreatments> (string id))
      [dr s]
      |> addOrder (Uri.fromtype<AntiTuberculosisTreatments> (string id))
      |> Assert.graph Graph.setupGraph

  type Graph with
    static member fromHelicobacterPyloriRegimens (HelicobacterPyloriRegimens(id,title,regimens)) =
      let regimen (Regimen(title,drugs,course,patientgroup)) =
        let crse (Course(p)) = p |> (string >> xsd.xmlliteral >> dataProperty !!"nicebnf:hasCourse")
        
        let getPatientGroup (PatientGroup(agegroup,_)) =
          let grp (x:AgeGroup) =
            let g (p:sectionProvider.P) a =
              one !!"nicebnf:hasPatientGroup" (Uri.fromgrp p.String.Value)
                          (optionlist {
                            yield! p |> xtitle
                            yield objectProperty !!"rdfs:subClassOf" (Uri.fromgrp(a))
                            })
            match x with
              | Adult p -> g p "adult"
              | Child p -> g p "child"
          
          agegroup |> grp

        let drug d =
          let build s q = (optionlist{
                       yield s |> label
                       yield q |> (dp "quantity")
                       })
          match d with
                   | AcidSuppressant(s,Quantity(q))
                     -> blank !!"nicebnf:hasAcidSuppressant" (build s q)
                   | Antibacterial(s,Quantity(q))
                     -> blank !!"nicebnf:hasAntibacterial" (build s q)
        let savePatientGroup = match patientgroup with
                              | Some z -> [z]
                              | None -> []

        blank (Uri.has<Regimen>()) (optionlist{
          yield! title |> ti
          yield match course with | Some z -> (z |> crse)|> Some | None -> None
          yield! drugs |> List.map drug
          yield! savePatientGroup |> List.map getPatientGroup
          })

      let s = optionlist {
        yield! title <!> ti |> unwrap
        yield! regimens |> List.map regimen
        }

      let dr = resource (Uri.fromtype<HelicobacterPyloriRegimens> (string id))
      [dr s]
      |> addOrder (Uri.fromtype<HelicobacterPyloriRegimens> (string id))
      |> Assert.graph Graph.setupGraph

  type Graph with
    static member fromMalariaProphylaxisRegimens (MalariaProphylaxisRegimens(id,title,regimens)) =
      let regimen (MalariaProphylaxisRegimen(country,risks)) =
        let risk (x:MalariaRisk) =
          blank (Uri.has x) (optionlist {
            yield x.content |> (dp "content")
            yield x.regimen |> (dp "regimen")
            })
        blank (Uri.has<MalariaProphylaxisRegimen>()) (optionlist{
          yield country |> (string >> dp "country")
          yield! risks |> List.map risk
          })
      let s = optionlist{
        yield! title <!> ti |> unwrap
        yield! regimens |> List.map regimen
        }

      let dr = resource (Uri.fromtype<MalariaProphylaxisRegimens> (string id))
      [dr s]
      |> Assert.graph Graph.setupGraph

  type Graph with
    static member fromIntramuscularAdrenalineEmergency (x:IntramuscularAdrenalineEmergency) =
      let statement (x:DoseStatement) =
        blank (Uri.has x) (optionlist{
          yield x.age |> (dp "age")
          yield x.dose |> (dp "dose")
          yield x.volume |> (string >> xsd.xmlliteral >> (dataProperty !!"nicebnf:hasVolume"))
          yield x.note <!> (string >> dp "note")
          })

      let s = optionlist{
        yield! x.title <!> ti |> unwrap
        yield! x.statements |> List.map statement
        yield x.note <!> (dp "note")
        }

      let dr = resource (Uri.fromtype<IntramuscularAdrenalineEmergency>(string(x.id)))
      [dr s]
      |> Assert.graph Graph.setupGraph
