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

  type Graph with
    static member frommedicaldevice(MedicalDevice(id,t,pdi,ids)) =
      let s = optionlist {
        yield a Uri.MedicalDeviceEntity
        yield t |> (string >> xsd.string >> (dataProperty !!"rdfs:label"))
        yield pdi >>= (string >> xsd.xmlliteral >> (dataProperty !!"nicebnf:hasDitaContent") >> Some)
        yield! ids |> List.map (Uri.frommdt >> (objectProperty !!"nicebnf:hasMedicalDeviceType"))
        }
      let dr = resource (Uri.frommd id)
      [dr s] |> Assert.graph (empty())

module BorderlineSubstanceTaxonomyRdf =
  open BorderlineSubstanceTaxonomy

  type Graph with
    static member from(x:BorderlineSubstanceTaxonomy) =
      let s = optionlist {
        yield a Uri.BorderlineSubstanceTaxonomyEntity
        yield x.title |> (string >> xsd.string >> (dataProperty !!"rdfs:label"))
        yield x.general >>= (string >> xsd.xmlliteral >> (dataProperty !!"nicebnf:hasDitaContent") >> Some)
        yield! x.substances |> List.map (Uri.frombsc >> (objectProperty !!"nicebnf:hasBorderlineSubstance"))
        yield! x.categories |> List.map (Uri.frombst >> (objectProperty !!"nicebnf:hasBorderlineSubstanceTaxonomy"))
        }

      let dr = resource (Uri.frombst x.id)
      [dr s]
      |> Assert.graph (empty())

module PublicationRdf = 
  open Bnf.Publication

  type Graph with
    static member fromPublication (Publication(d)) =
      let dto = (System.DateTimeOffset d)^^xsd.datetime

      let s = [a !!(Uri.nicebnf + "Publication")
               dto |> (dataProperty !!"nicebnf:hasPublicationDate")]

      let dr = resource !!(Uri.bnfsite + "publication")
      [dr s]
      |> Assert.graph (empty())

module GenericRdf = 
  open Bnf.Generic

  type Graph with
    static member fromti (Title s) =
      dataProperty !!"rdfs:label" (s^^xsd.string)

    static member fromta (TargetAudience s) =
      dataProperty !!"nicebnf:hasTargetAudience" (s^^xsd.string)

    static member fromcontent (Content(s,ta)) =
      blank !!"nicebnf:hasContent"
        (optionlist {
          yield ta >>= (Graph.fromta >> Some)
          yield dataProperty !!"nicebnf:hasDitaContent" ((string s)^^xsd.xmlliteral)})

    static member from (x:ContentLink) =
      objectProperty !!"nicebnf:hasLink" (Uri.totopic (x.rel,x.id))

    static member fromGeneric n (x:Generic) =
      let uri n id = !!(sprintf "%s%s/%s" Uri.bnfsite n (string id))

      let s = optionlist {
        yield Graph.fromti x.title
        yield! x.links |> Seq.map Graph.from |> Seq.toList
        yield! x.content |> List.map Graph.fromcontent}

      let dr r = resource (uri n id) r

      [dr s]
      |> Assert.graph (empty())

module InteractionRdf =
  open Bnf.Interaction

  type Graph with
    static member from (InteractionList(id,t,il,ids)) =
      let s = [ a Uri.InteractionListEntity
                t |> (string >> xsd.string >> (dataProperty !!"rdfs:label"))]

      let iwuri = Uri.fromiw id

      let importance i =
        match i.importance with
          | High -> dataProperty !!"nicebnf:hasImportance" ("High"^^xsd.string)
          | NotSet -> dataProperty !!"nicebnf:hasImportance" ("NotSet"^^xsd.string)

      let interactionDetail i = one !!"nicebnf:hasInteraction" (iwuri i)
                                 [a Uri.InteractionEntity
                                  objectProperty !!"nicebnf:interactsWith" (Uri.fromiwl i)
                                  importance i
                                  dataProperty !!"nicebnf:hasDitaContent" ((string i.message)^^xsd.xmlliteral)
                                  dataProperty !!"rdfs:label" ((string i.message.XElement.Value)^^xsd.string)
                                  dataProperty !!"nicebnf:hasImportance" ((string i.importance)^^xsd.string)]

      let link = Uri.fromil >> objectProperty !!"nicebnf:hasInteractionList"

      let dr r = resource (Uri.fromil id) r
      [dr s
       dr (il |> List.map interactionDetail)
       dr (ids |> List.map link)]
       |> Assert.graph (empty())

module BorderlineSubstanceRdf =
  open Bnf.BorderlineSubstance

  let inline dpo n x = x >>= (string >> xsd.string >> (dataProperty !!("nicebnf:has" + n)) >> Some)

  type Graph with
    static member from (x:BorderlineSubstance) =
      let l t = match t with | Title t -> t.XElement.Value.ToString()

      let s =  optionlist {
                yield a Uri.BorderlineSubstanceEntity
                yield x.title |> (l >> xsd.string >> (dataProperty !!"rdfs:label"))
                yield x.title |> (string >> xsd.xmlliteral >> (dataProperty !!"nicebnf:hasTitle"))
                yield x.category |> (string >> Uri.frombsc >> (objectProperty !!"nicebnf:hasCategory"))
                yield x.intro >>= (string >> xsd.string >> (dataProperty !!"nicebnf:hasIntroductoryNote") >> Some)}

      let ds = x.details |> List.map Graph.fromdetails

      let dr r = resource (Uri.from x) r
      [dr s
       dr ds]
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
                  yield p |> (string >> xsd.xmlliteral >> (dataProperty !!"nicebnf:hasTitle"))
                  yield p.XElement.Value |> (xsd.string >> (dataProperty !!"rdfs:label"))
                  yield m |> dpo "Manufacturer"}
              | None -> []
      let ts = pts |> List.map Graph.frompricetarrif
      blank !!"nicebnf:hasBorderlineSubstancePrep" (s @ ts)

    static member fromdetail (x:Detail) =
      let inline dp n s = dataProperty !!("nicebnf:has" + n) ((string s)^^xsd.string)
      let inline dpx n s = dataProperty !!("nicebnf:has" + n) ((string s)^^xsd.xmlliteral)
      match x with
        | Formulation s -> s |> dp "Formulation"
        | EnergyKj e -> e |> dp "EnergyKj"
        | EnergyKcal e -> e |> dp "EnergyKcal"
        | ProteinGrams p -> p |> dp "ProteinGrams"
        | ProteinConstituents p -> p |> dp "ProteinConstituents"
        | CarbohydrateGrams c -> c |> dp "CarbohydrateGrams"
        | CarbohydrateConstituents c -> c |> dp "CarbohydrateConstituents"
        | FatGrams f -> f |> dp "FatGrams"
        | FatConstituents f -> f |> dp "FatConstituents"
        | FibreGrams f -> f |> dp "FibreGrams"
        | SpecialCharacteristics s -> s |> dp "SpecialCharacteristics"
        | Acbs a -> a |> dpx "Acbs"
        | Presentation p -> p |> dp "Presentation"

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
      resource !!(Uri.nicebnfClass + "Classification#" + x.key)
         [dataProperty !!"rdfs:label" (x.value^^xsd.string)]


module TreatmentSummaryRdf =
  open Bnf.TreatmentSummary

  type Graph with
    static member from (x:TreatmentSummary) =
      let secondary  (TreatmentSummary (_,x)) =
        match x with
          | Generic _ -> None
          | _ -> a !!(Uri.nicebnf + (toString x)) |> Some

      let s = optionlist {
               yield a Uri.TreatmentSummaryEntity |> Some
               yield secondary x}

      let p = Graph.fromts x
      let dr r = resource (Uri.from x) r
      [dr s
       dr p] |> Assert.graph (empty())

    static member fromti (Title s) =
      dataProperty !!"rdfs:label" (s^^xsd.string)
    static member fromdoi (Shared.Doi s) =
      dataProperty !!"nicebnf:hasDoi" (s^^xsd.string)
    static member frombs (BodySystem s) =
      dataProperty !!"nicebnf:hasBodySystem" (s^^xsd.string)
    static member fromta (TargetAudience s) =
      dataProperty !!"nicebnf:hasTargetAudience" (s^^xsd.string)
    static member fromcontent (Content(s,ta)) =
      blank !!"nicebnf:hasContent"
       (optionlist {
         yield ta >>= (Graph.fromta >> Some)
         yield dataProperty !!"nicebnf:hasDitaContent" ((string s)^^xsd.xmlliteral)})

    static member from (x:ContentLink) =
      objectProperty !!"nicebnf:hasLink" (Uri.totopic (x.rel,x.id))

    static member from (x:Summary) =
      optionlist {
        yield Graph.fromti x.title
        yield x.doi >>= (Graph.fromdoi >> Some)
        yield x.bodySystem >>= (Graph.frombs >> Some)
        yield! x.links |> Seq.map Graph.from |> Seq.toList
        yield! x.content |> List.map Graph.fromcontent}

    static member fromts (TreatmentSummary (_,x)) =
      match x with
        | ComparativeInformation s -> Graph.from s
        | ManagementOfConditions s -> Graph.from s
        | MedicalEmergenciesBodySystems s -> Graph.from s
        | TreatmentOfBodySystems s -> Graph.from s
        | About s -> Graph.from s
        | Guidance s -> Graph.from s
        | Generic s -> Graph.from s

module MedicinalFormRdf =
  open Bnf.Drug
  open Bnf.MedicinalForm

  type Graph with
    static member from (x:MedicinalForm) =
      let s = optionlist{
                yield a Uri.MedicinalFormEntity
                yield x.title >>= (string >> xsd.string >> (dataProperty !!"rdfs:label") >> Some)
                yield x.excipients >>= Graph.fromexc
                yield x.electrolytes >>= Graph.fromele}

      let cals = match x.cautionaryAdvisoryLabels with
                 | Some x -> Graph.fromcals x
                 | None -> []

      let cmpis = x.cmpis |> List.map Graph.fromclinicalmpi

      let mps = x.medicinalProducts |> List.map Graph.from
      let dr r = resource (Uri.from x) r
      [dr s
       dr mps
       dr cals
       dr cmpis]
       |> Assert.graph (empty())

    static member fromclinicalmpi x = objectProperty !!"nicebnf:hasClinicalMedicinalProductInformation" (Uri.fromcmpi x)

    static member fromcal (CautionaryAdvisoryLabel(ln,p)) =
      blank !!"nicebnf:hasCautionaryAdvisoryLabel"
               (optionlist {
                 yield p |> (string >> xsd.xmlliteral >> (dataProperty !!"nicebnf:hasDitaContent"))
                 yield ln >>= (string >> xsd.string >> (dataProperty !!"nicebnf:hasLabelNumber") >> Some)})

    static member fromcals (CautionaryAdvisoryLabels(_,cals)) =
      cals |> Array.map Graph.fromcal |> Array.toList

    static member dp n = xsd.string >> (dataProperty !!("nicebnf:has" + n))

    static member fromman (Manufacturer x) = Graph.dp "Manufacturer" x |> Some
    static member frombt (BlackTriangle x) = Graph.dp "BlackTriangle" x |> Some
    static member frommpt (MedicinalProductTitle(m,bt,t)) =
      let s = optionlist {
               yield m >>= Graph.fromman
               yield bt >>= Graph.frombt
               yield t |> (string >> xsd.xmlliteral >> (dataProperty !!"nicebnf:hasDitaContent"))}
      blank !!"nicebnf:hasMedicinalProductTitle" s |> Some

    static member fromexc (Excipients e) =
      dataProperty !!"nicebnf:hasExcipients" ((string e)^^xsd.xmlliteral) |> Some

    static member fromele (Electrolytes e) =
      dataProperty !!"nicebnf:hasElectrolytes" ((string e)^^xsd.xmlliteral) |> Some

    static member fromsai(StrengthOfActiveIngredient p) = Graph.dp "StrengthOfActiveIngredient" (string p) |> Some
    static member fromcd(ControlledDrug p) = Graph.dp "ControlledDrug" (string p) |> Some

    static member fromnhsi (NhsIndicative x) = Graph.dp "NhsIndicative" x |> Some
    static member frompt (PriceText x) = Graph.dp "PriceText" x |> Some
    static member fromnhsip (NhsIndicativePrice x) = Graph.dp "NhsIndicativePrice" (string x) |> Some
    static member fromnhsii (NhsIndicativeInfo(nhsi,pt,nhsip)) =
      optionlist {
        yield nhsi >>= Graph.fromnhsi
        yield pt >>= Graph.frompt
        yield nhsip >>= Graph.fromnhsip}

    static member fromps (PackSize d) = Graph.dp "PackSize" (string d) |> Some
    static member fromuom u = Graph.dp "UnitOfMeasure" (string u) |> Some
    static member fromlc lc = Graph.dp "LegalCategory" (string lc) |> Some
    static member frompackinfo (PackInfo(ps,uom,lc)) =
      optionlist {
       yield ps >>= Graph.fromps
       yield uom >>= Graph.fromuom
       yield lc >>= Graph.fromlc}

    static member fromdt (DrugTarrif s) = Graph.dp "DrugTarrif" s |> Some
    static member fromdtp (DrugTariffPrice dtp) = Graph.dp "DrugTariffPrice" (string dtp) |> Some
    static member fromdti (DrugTariffInfo(dt,pt,dtp)) =
      optionlist {
       yield dt >>= Graph.fromdt
       yield pt >>= Graph.frompt
       yield dtp >>= Graph.fromdtp}

    static member frompack(Pack(pi,nii,dti)) =
      blank !!"nicebnf:hasPack"
        (optionlist {
          return! pi >>= (Graph.frompackinfo >> Some)
          return! nii >>= (Graph.fromnhsii >> Some)
          return! dti >>= (Graph.fromdti >> Some)
          yield a !!"nicebnf:Pack"})

    static member from (x:MedicinalProduct) =
      one !!"nicebnf:hasMedicinalProduct" (Uri.from x)
        (optionlist {
          yield a Uri.MedicinalProductEntity
          yield x.ampid |> string |> Graph.dp "Ampid"
          yield x.title |> Graph.frommpt
          yield! x.strengthOfActiveIngredient |> List.choose Graph.fromsai
          yield! x.controlledDrugs |> List.choose Graph.fromcd
          yield! x.packs |> List.map Graph.frompack
        })
namespace Bnf
open FSharp.RDF
open FSharp.Data.Runtime

module WoundManagementRdf =
  open prelude
  open resource
  open Bnf.WoundManagement
  open Assertion
  open rdf
  open Shared
  open Rdf
  open RdfUris

  let dp n = xsd.string >> dataProperty !!("nicebnf:has" + n)

  type Graph with
    static member setupGraph = Graph.ReallyEmpty ["nicebnf",!!Uri.nicebnf
                                                  "rdfs",!!"http://www.w3.org/2000/01/rdf-schema#"
                                                  "bnfsite",!!Uri.bnfsite]
    static member fromTitle (Title t) = dataProperty !!"rdfs:label" (t^^xsd.string)

    static member from (x:WoundManagement) =
      let s = optionlist {
               yield a Uri.WoundManagementEntity
               yield x.general >>= (string >> (dp "General") >> Some)
               yield x.title |> Graph.fromTitle
               yield! x.dressingChoices |> List.map Graph.fromWoundType
               yield! x.productGroups |> List.map Graph.fromProductGroup
               yield! x.links |> List.map Graph.fromwml
              }

      let dr r = resource (Uri.from x) r

      [dr s]
      |> Assert.graph Graph.setupGraph

    static member fromDescription (Description sd) =
      dataProperty !!"nicebnf:hasDitaContent" (sd |> (string >> xsd.xmlliteral))

    static member fromwml (x:WoundManagementLink) =
      objectProperty !!"nicebnf:hasWoundManagment" (Uri.totopic(x.rel,x.id))

    static member fromExudate (WoundExudate(s,wmls)) =
      blank !!"nicebnf:WoundExudate"
        (dataProperty !!"nicebnf:hasRate" (s^^xsd.string) :: (wmls |> List.map Graph.fromwml))

    static member fromWoundType (WoundType(TypeOfWound(t),d,wes)) =
      blank !!"nicebnf:hasWoundType"
        (optionlist {
            yield t |> dp "TypeOfWound"
            yield d >>= (Graph.fromDescription >> Some)
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
            yield t |>  Graph.fromTitle
            yield d >>= (Graph.fromDescription >> Some)
            yield! pl |> List.map Graph.fromProduct
            })


namespace Bnf
open FSharp.RDF
open FSharp.Data.Runtime

module MedicalDeviceTypeRdf =
  open prelude
  open resource
  open Bnf.Drug
  open Bnf.MedicinalForm
  open Bnf.MedicalDeviceType
  open Assertion
  open rdf
  open Shared
  open Rdf
  open RdfUris
  open DrugRdf
  open MedicinalFormRdf

  type Graph with
    static member from (x:MedicalDeviceType) =
      let s = [a Uri.MedicalDeviceTypeEntity
               dataProperty !!"rdfs:label" ((string x.title)^^xsd.string)]

      let uri = Uri.fromcmdig x

      let gs = x.groups |> List.map (Graph.fromcmdig uri)

      let dr r = resource (Uri.from x) r
      [dr s
       dr gs]
       |> Assert.graph Graph.setupGraph

    static member fromcmdig uri (x:ClinicalMedicalDeviceInformationGroup) =
      let s =  optionlist {
                yield a Uri.ClinicalMedicalDeviceInformationGroupEntity
                yield dataProperty !!"rdfs:label" ((string x.title)^^xsd.string)
                yield x.description >>= (Graph.fromdd uri >> Some)
                yield x.complicance >>= (Graph.fromcs uri >> Some)}

      let sec = Graph.fromsec uri

      let ss = x.sections |> List.collect sec
      let mps = x.products |> List.map Graph.from

      one !!"bnfsite:hasClinicalMedicalDeviceInformationGroup" (uri x.id) (s @ ss @ mps)

    static member fromdd uri (DeviceDescription(id,sd)) =
      one !!"bnfsite:hasDeviceDescription" (uri id)
        [dataProperty !!"nicebnf:hasDitaContent" ((string sd)^^xsd.xmlliteral)]

    static member fromcs uri (ComplicanceStandards(id,sd)) =
      one !!"bnfsite:hasComplicanceStandards" (uri id)
        [dataProperty !!"nicebnf:hasDitaContent" ((string sd)^^xsd.xmlliteral)]
