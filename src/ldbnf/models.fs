namespace Bnf
open FSharp.Data
open prelude
open Shared

module MedicinalForm =
  type Title =
    | Title of string
    override __.ToString() = match __ with | Title x -> x

  type LabelNumber =
    | LabelNumber of decimal
    override __.ToString() = match __ with | LabelNumber x -> string x

  type CautionaryAndAdvisoryLabelsTitle =
    | CautionaryAndAdvisoryLabelsTitle of string
    override __.ToString() = match __ with | CautionaryAndAdvisoryLabelsTitle x -> x

  type CautionaryAdvisoryLabel = | CautionaryAdvisoryLabel of Option<LabelNumber> * drugProvider.P

  type CautionaryAdvisoryLabels = | CautionaryAdvisoryLabels of Option<CautionaryAndAdvisoryLabelsTitle> * CautionaryAdvisoryLabel []

  type Excipients = | Excipients of drugProvider.Section

  type Electrolytes = | Electrolytes of drugProvider.Section

  type Manufacturer = | Manufacturer of string

  type BlackTriangle = | BlackTriangle of string

  type MedicinalProductTitle = | MedicinalProductTitle of Option<Manufacturer> * Option<BlackTriangle> * drugProvider.Title

  type Ampid =
    | Ampid of int64
    override __.ToString() = match __ with | Ampid x -> string x

  type StrengthOfActiveIngredient = | StrengthOfActiveIngredient of drugProvider.P

  type UnitOfMeasure =
    | UnitOfMeasure of string
    override __.ToString() = match __ with | UnitOfMeasure x -> x

  type PackSize = | PackSize of decimal

  type LegalCategory =
    | POM
    | P
    | GSL
    override __.ToString() = toString __

  type PackInfo = | PackInfo of Option<PackSize> * Option<UnitOfMeasure> * Option<LegalCategory>

  type PriceText = | PriceText of string

  type NhsIndicative = | NhsIndicative of string

  type NhsIndicativePrice = | NhsIndicativePrice of decimal

  type NhsIndicativeInfo = | NhsIndicativeInfo of Option<NhsIndicative> * Option<PriceText> * Option<NhsIndicativePrice>


  type DrugTarrif = | DrugTarrif of string

  type DrugTariffPrice = | DrugTariffPrice of decimal

  type DrugTariffInfo = | DrugTariffInfo of Option<DrugTarrif> * Option<PriceText> * Option<DrugTariffPrice>

  type ControlledDrug = | ControlledDrug of drugProvider.P

  type Pack = | Pack of Option<PackInfo> * Option<NhsIndicativeInfo> * Option<DrugTariffInfo>

  type ClinicalMedicinalProductInformation = | ClinicalMedicinalProductInformation of Id

  type MedicinalProduct = {
    title:MedicinalProductTitle;
    ampid:Ampid;
    strengthOfActiveIngredient: StrengthOfActiveIngredient list;
    controlledDrugs: ControlledDrug list;
    packs: Pack list}

  type MedicinalForm = {
    id : Id;
    title : Title option;
    excipients : Excipients option;
    electrolytes : Electrolytes option;
    cautionaryAdvisoryLabels : CautionaryAdvisoryLabels option;
    medicinalProducts : MedicinalProduct list;
    cmpis : ClinicalMedicinalProductInformation list;
  }

module MedicinalFormParser =
  open MedicinalForm

  let inline withoc n x =
    let oc = (^a : (member Outputclass : string) x)
    if (oc = n) then Some (x)
    else None

  let inline withoco n x =
    let oc = (^a : (member Outputclass : Option<string>) x)
    match oc with
      | Some s ->
        if (s = n) then Some (x) else None
      | _ -> None

  type CautionaryAdvisoryLabel with
    static member from (x:drugProvider.P) =
      let ln = x.Phs.[0].Number >>= (LabelNumber >> Some)
      CautionaryAdvisoryLabel(ln,x)

  type CautionaryAdvisoryLabels with
    static member from (x:drugProvider.Section) =
      let ls = x.Ps |> Array.map CautionaryAdvisoryLabel.from
      let t = match x.Title with
                | Some t -> t.Value >>= (CautionaryAndAdvisoryLabelsTitle >> Some)
                | None -> failwith "CautionaryAdvisoryLabels must have a Title"
      CautionaryAdvisoryLabels(t,ls)

  let fromphn c (x:drugProvider.Ph) =
    x.Number >>= (c >> Some)

  let fromphs c (x:drugProvider.Ph) =
    x.String >>= (c >> Some)

  type UnitOfMeasure with
    static member from (x:drugProvider.Ph) =
      match x.String with
        | Some(s) -> UnitOfMeasure s |> Some
        | None -> None

  type LegalCategory with
    static member from (x:drugProvider.Ph) =
      match x.String with
        | Some(s) -> match s with
                     | "POM" -> Some(POM)
                     | "P" -> Some(P)
                     | "GSL" -> Some(GSL)
                     | _ ->
                       printfn "Unknown LegalCatgory %s" s
                       None
        | None -> None

  type PackInfo with
    static member from (x:drugProvider.P) =
      let ps = x.Phs |> Array.tryPick (withoc "packSize") >>= (fromphn PackSize)
      let uom = x.Phs |> Array.tryPick (withoc "unitOfMeasure") >>= UnitOfMeasure.from
      let lc = x.Phs |> Array.tryPick (withoc "legalCategory") >>= LegalCategory.from
      PackInfo(ps,uom,lc)

  type NhsIndicativeInfo with
    static member from (x:drugProvider.P) =
      let nhsi = x.Phs |> Array.tryPick (withoc "nhsIndicative") >>= (fromphs NhsIndicative)
      let pt = x.Phs |> Array.tryPick (withoc "priceText") >>= (fromphs PriceText)
      let nhsip = x.Phs |> Array.tryPick (withoc "nhsIndicativePrice") >>= (fromphn NhsIndicativePrice)
      NhsIndicativeInfo(nhsi,pt,nhsip)

  type DrugTariffInfo with
    static member from (x:drugProvider.P) =
      let dt = x.Phs |> Array.tryPick (withoc "drugTariff") >>= (fromphs DrugTarrif)
      let pt = x.Phs |> Array.tryPick (withoc "priceText") >>= (fromphs PriceText)
      let dtp = x.Phs |> Array.tryPick (withoc "drugTariffPrice") >>= (fromphn DrugTariffPrice)
      DrugTariffInfo(dt,pt,dtp)

  type MedicinalProductTitle with
    static member from (x:drugProvider.Title) =
      let mph = x.Phs |> Array.tryPick (withoc "manufacturer")
      let btph = x.Phs |> Array.tryPick (withoc "blackTriangle")
      if (mph.IsSome) then mph.Value.XElement.Remove() //edit the xml
      if (btph.IsSome) then btph.Value.XElement.Remove()

      let m =  mph >>= (fromphs Manufacturer)
      let bt = btph >>= (fromphs BlackTriangle)

      MedicinalProductTitle(m,bt,x)

  type Ampid with
    static member from (x:drugProvider.Data) =
      Ampid(x.Number.Value)

  type Pack with
    static member from (x:drugProvider.Ul) =
      x.Lis |> Array.map Pack.from
    static member from (x:drugProvider.Li) =
      let pi = x.Ps |> Array.tryPick (withoco "packInfo") >>= (PackInfo.from >> Some)
      let nio = x.Ps |> Array.tryPick (withoco "nhsIndicativeInfo") >>= (NhsIndicativeInfo.from >> Some)
      let dti = x.Ps |> Array.tryPick (withoco "drugTariffInfo") >>= (DrugTariffInfo.from >> Some)
      Pack(pi,nio,dti)

  type MedicinalProduct with
    static member from (x:drugProvider.Section) =
      let t = match x.Title with
               | Some t -> MedicinalProductTitle.from t
               | None -> failwith "MedicinalProduct must have a Title"
      let str = x.Ps |> Array.choose (withoco "strengthOfActiveIngredient")
                     |> Array.map StrengthOfActiveIngredient
                     |> Array.toList
      let con = x.Ps |> Array.choose (withoco "controlledDrugs")
                     |> Array.map ControlledDrug
                     |> Array.toList
      let ps = x.Uls |> Array.map Pack.from |> Array.collect id |> Array.toList
      let a = match x.Data with
              | Some d -> Ampid.from d
              | None -> failwith "MedicinalProduct must have an Ampid"
      {title = t; ampid = a; strengthOfActiveIngredient = str; packs = ps; controlledDrugs = con;}

  type ClinicalMedicinalProductInformation with
    static member list (x:drugProvider.Section) =
      x.Xrefs |> Array.map (fun xref -> xref.Href |> Id |> ClinicalMedicinalProductInformation)

  type MedicinalForm with
    static member parse (x:drugProvider.Topic) =
      let sections (x:drugProvider.Topic) =
        match x.Body with
        | Some (b) -> b.Sections
        | _ -> failwith "body is required"

      let t = match x.Title.Value with
              | Some(v) -> Some(Title v)
              | None -> None
      let cals = x |> sections
               |> Array.choose (withoco "cautionaryAndAdvisoryLabels")
               |> Array.map (CautionaryAdvisoryLabels.from >> Some)
               |> Array.tryPick id
      let mps = x |> sections
               |> Array.choose (withoco "medicinalProduct")
               |> Array.map MedicinalProduct.from
               |> Array.toList
      let ex = x |> sections
               |> Array.choose (withoco "excipients")
               |> Array.map (Excipients >> Some)
               |> Array.tryPick id
      let el = x |> sections
               |> Array.choose (withoco "electrolytes")
               |> Array.map (Electrolytes >> Some)
               |> Array.tryPick id
      let cmpis = x |> sections
               |> Array.choose (withoco "clinicalMedicinalProductInformationGroup")
               |> Array.collect ClinicalMedicinalProductInformation.list
               |> Array.toList
      {id = Id(x.Id); title = t; excipients=ex; electrolytes=el; cautionaryAdvisoryLabels = cals; medicinalProducts = mps; cmpis = cmpis;}

module MedicalDeviceType =
  open Drug
  open MedicinalForm

  type DeviceDescription = | DeviceDescription of Id * drugProvider.Sectiondiv

  type ComplicanceStandards = | ComplicanceStandards of Id * drugProvider.Sectiondiv

  type ClinicalMedicalDeviceInformationGroup ={
     id:Id;
     title:Title;
     sections:MonographSection list;
     products:MedicinalProduct list;
     description:DeviceDescription option;
     complicance:ComplicanceStandards option}

  type MedicalDeviceType = {
     id:Id;
     title:Title;
     groups:ClinicalMedicalDeviceInformationGroup list;
    }

module MedicalDeviceTypeParser =
  open Drug
  open DrugParser
  open MedicinalForm
  open MedicinalFormParser
  open MedicalDeviceType

  let sections (x:drugProvider.Topic) =
    match x.Body with
    | Some (b) -> b.Sections
    | _ -> failwith "body is required"

  let firstsd c (x:drugProvider.Topic) =
    let sd = x |> sections |> Array.collect (fun s -> s.Sectiondivs)
    c(Id(x.Id),sd.[0])

  type DeviceDescription with
    static member from  = firstsd DeviceDescription >> Some

  type ComplicanceStandards with
    static member from = firstsd ComplicanceStandards >> Some

  type ClinicalMedicalDeviceInformationGroup with
    static member list (x:drugProvider.Topic) =
      x.Topics |> Array.map ClinicalMedicalDeviceInformationGroup.from
    static member from (x:drugProvider.Topic) =
      let mss = x.Topics |> Array.map MonographSection.section |> Array.choose id |> Array.toList

      let mps = x.Topics
                 |> Array.choose (withoc "clinicalMedicinalProducts")
                 |> Array.collect sections
                 |> Array.choose (withoco "medicinalProduct")
                 |> Array.map MedicinalProduct.from
                 |> Array.toList

      let des = x.Topics |> Array.tryPick (withoc "deviceDescription") >>= DeviceDescription.from
      let com = x.Topics |> Array.tryPick (withoc "complianceStandards") >>= ComplicanceStandards.from

      {id=Id(x.Id); title=Title(x.Title.Value.Value); sections=mss; products=mps; description = des; complicance = com;}

  type MedicalDeviceType with
    static member parse (x:drugProvider.Topic) =
      let gs = x.Topics |> Array.collect ClinicalMedicalDeviceInformationGroup.list |> Array.toList
      {id=Id(x.Id); title=Title(x.Title.Value.Value); groups=gs}

open System.Xml.Linq
open System.Xml.XPath

module TreatmentSummary =
  type tsProvider = XmlProvider<"./samples/supertreatmentsummary.xml", Global=true>

  type Title = | Title of string
  type TargetAudience = | TargetAudience of string
  type Content = | Content of tsProvider.Section * TargetAudience option
  type BodySystem = | BodySystem of string

  type Summary = {
    title:Title;
    doi:Doi option;
    bodySystem:BodySystem option;
    content:Content list;
    links:ContentLink seq
  }

  type Treatment =
    | ComparativeInformation of Summary
    | ManagementOfConditions of Summary
    | MedicalEmergenciesBodySystems of Summary
    | TreatmentOfBodySystems of Summary
    | About of Summary
    | Guidance of Summary
    | Generic of Summary

  type TreatmentSummary = | TreatmentSummary of Id * Treatment

module TreatmentSummaryParser =
  open TreatmentSummary

  let withname = (|HasName|_|)

  type Doi with
    static member from (x:tsProvider.Data) = Doi(x.Value) |> Some

  type BodySystem with
    static member from (x:tsProvider.Data) = BodySystem(x.Value) |> Some

  type Content with
    static member from (x:tsProvider.Section) =
      let ta = match x.Outputclass with
               | Some x -> TargetAudience x |> Some
               | None -> None
      Content(x,ta)

  type Summary with
    static member from (x:tsProvider.Topic) =
      let ls = x.XElement |> ContentLink.from
      let t = Title(x.Title)
      let d = x.Body.Datas |> Array.choose (withname "doi") |> Array.tryPick Doi.from
      let bs = x.Body.Datas |> Array.choose (withname "bodySystem") |> Array.tryPick BodySystem.from
      let c = x.Body.Sections |> Array.map Content.from |> Array.toList
      Id(x.Id),{title = t; doi = d; bodySystem = bs; content = c; links = ls}

  type TreatmentSummary with
    static member from c (i,s) =
      TreatmentSummary(i, c s)

  type TreatmentSummary with
    static member parse (x:tsProvider.Topic) =
      let build c t = Summary.from t |> TreatmentSummary.from c
      match x with
        | HasOutputClass "comparativeInformation" t -> t |> build ComparativeInformation
        | HasOutputClass "managementOfConditions" t -> t |> build ManagementOfConditions
        | HasOutputClass "medicalEmergenciesBodySystems" t -> t |> build MedicalEmergenciesBodySystems
        | HasOutputClass "treatmentOfBodySystems" t -> t |> build TreatmentOfBodySystems
        | HasOutputClass "about" t -> t |> build About
        | HasOutputClass "guidance" t -> t |> build Guidance
        | t -> t |> build Generic


module DrugClassification =
  type dcProvider = XmlProvider<"./samples/drugClassifications.xml", Global=true>

  type Classification = {key:string; value:string}

  type DrugClassifications = | DrugClassifications of Classification list

module DrugClassificationParser =
  open DrugClassification

  type Classification with
    static member from (x:dcProvider.Section) =
      let k = x.Ps.[0].Value
      let v = x.Ps.[1].Value
      {key = k; value = v}

  type DrugClassifications with
    static member parse (x:dcProvider.Topic) =
      let cs = x.Body.Sections |> Array.map Classification.from |> Array.toList
      DrugClassifications(cs)

module BorderlineSubstance =

  type bsProvider = XmlProvider<"./samples/borderlinesubstances.xml", Global=true, SampleIsList=true>

  type Title =
    | Title of bsProvider.Title
    override __.ToString() = match __ with | Title x -> string x

  type Link = {Uri:string;Label:string;}

  type Category =
    | Category of string
    override __.ToString() = match __ with | Category x -> x

  type IntroductionNote =
    | IntroductionNote of string
    override __.ToString() = match __ with | IntroductionNote x -> x


  [<Measure>] type Kj
  [<Measure>] type Kcal
  [<Measure>] type g

  type Detail =
    | Formulation of string
    | EnergyKj of int<Kj>
    | EnergyKcal of int<Kcal>
    | ProteinGrams of int<g>
    | ProteinConstituents of string
    | CarbohydrateGrams of int<g>
    | CarbohydrateConstituents of string
    | FatGrams of int<g>
    | FatConstituents of string
    | FibreGrams of int<g>
    | SpecialCharacteristics of string
    | Acbs of bsProvider.P
    | Presentation of string

  type Manufacturer =
    | Manufacturer of string
    override __.ToString() = match __ with | Manufacturer x -> string x

  type PreparationTitle = | PreparationTitle of bsProvider.P * Manufacturer option


  type PackSize =
    | PackSize of decimal
    override __.ToString() = match __ with | PackSize x -> string x

  type UnitOfMeasure =
    | UnitOfMeasure of string
    override __.ToString() = match __ with | UnitOfMeasure x -> string x

  type PackAcbs =
    | PackAcbs of string
    override __.ToString() = match __ with | PackAcbs x -> x

  type PackInfo = | PackInfo of PackSize option * UnitOfMeasure option * PackAcbs option


  type NhsIndicative =
    | NhsIndicative of string
    override __.ToString() = match __ with | NhsIndicative x -> x

  type PriceText =
    | PriceText of string
    override __.ToString() = match __ with | PriceText x -> x

  type NhsIndicativePrice =
    | NhsIndicativePrice of decimal
    override __.ToString() = match __ with | NhsIndicativePrice x -> string x

  type NhsIndicativeInfo = | NhsIndicativeInfo of NhsIndicative option * PriceText option * NhsIndicativePrice option


  type PackSizePriceTariff = | PackSizePriceTariff of PackInfo option * NhsIndicativeInfo option


  type BorderlineSubstancePrep = | BorderlineSubstancePrep of PreparationTitle option * PackSizePriceTariff list

  type Details = | Details of Detail list * BorderlineSubstancePrep list

  type BorderlineSubstance = {
    id:Id;
    title:Title;
    category:Category;
    intro:IntroductionNote option;
    details:Details list;
  }


module BorderlineSubstanceParser =
  open BorderlineSubstance

  let inline withoc n x =
    let oc = (^a : (member Outputclass : string) x)
    if (oc = n) then Some (x)
    else None

  let inline withoco n x =
    let oc = (^a : (member Outputclass : Option<string>) x)
    match oc with
      | Some s ->
        if (s = n) then Some (x) else None
      | _ -> None

  let inline (|HasOutputClass|_|) (n:string) x =
    let oc = (^a : (member Outputclass : string) x)
    if oc = n then Some(x)
    else None

  let inline (|HasOutputClasso|_|) (n:string) x =
    let oc = (^a : (member Outputclass : Option<string>) x)
    match oc with
      | Some s -> if s = n then Some(x)
                  else None
      | None -> None

  type IntroductionNote with
    static member from (x:bsProvider.P) =
      match x with
        | HasOutputClasso "introductionNote" _ ->
          x.String >>= (IntroductionNote >> Some)
        | _ -> None

  let unit<[<Measure>]'u> = int >> LanguagePrimitives.Int32WithMeasure<'u>

  type Manufacturer with
    static member from (x:bsProvider.Ph) =
      match x with
        | HasOutputClass "manufacturer" ph -> ph.String >>= (Manufacturer >> Some)
        | _ -> None

  type PreparationTitle with
    static member from (x:bsProvider.P) =
      let m (p:bsProvider.P) = p.Phs |> Array.tryPick Manufacturer.from
      match x with
        | HasOutputClasso "title" p -> PreparationTitle(p, m p) |> Some
        | _ -> None

  let fromphn c (x:bsProvider.Ph) =
    x.Number >>= (c >> Some)

  let fromphs c (x:bsProvider.Ph) =
    x.String >>= (c >> Some)

  type UnitOfMeasure with
    static member from (x:bsProvider.Ph) =
      match x.String with
        | Some(s) -> UnitOfMeasure s |> Some
        | None -> None

 

  type PackInfo with
    static member from (x:bsProvider.P) =
      let ps = x.Phs |> Array.tryPick (withoc "packSize") >>= (fromphn PackSize)
      let uom = x.Phs |> Array.tryPick (withoc "unitOfMeasure") >>= UnitOfMeasure.from
      let acbs = x.Phs |> Array.tryPick (withoc "acbs") >>= (fromphs PackAcbs)
      PackInfo(ps,uom,acbs)

  type NhsIndicativeInfo with
    static member from (x:bsProvider.P) =
      let nhsi = x.Phs |> Array.tryPick (withoc "nhsIndicative") >>= (fromphs NhsIndicative)
      let pt = x.Phs |> Array.tryPick (withoc "priceText") >>= (fromphs PriceText)
      let nhsip = x.Phs |> Array.tryPick (withoc "nhsIndicativePrice") >>= (fromphn NhsIndicativePrice)
      NhsIndicativeInfo(nhsi,pt,nhsip)

  type PackSizePriceTariff with
    static member from (x:bsProvider.Li) =
      let pi = x.Ps |> Array.tryPick (withoco "packInfo") >>= (PackInfo.from >> Some)
      let nhs = x.Ps |> Array.tryPick (withoco "nhsIndicativeInfo") >>= (NhsIndicativeInfo.from >> Some)
      PackSizePriceTariff(pi,nhs)

  type BorderlineSubstancePrep with
    static member from (x:bsProvider.Sectiondiv) =
      let title = x.P >>= PreparationTitle.from
      let pt = match x.Ul with
                | Some ul -> ul.Lis |> Array.map PackSizePriceTariff.from |> Array.toList
                | None -> []
      BorderlineSubstancePrep(title,pt)

  type Detail with
    static member from (x:bsProvider.P) =
      match x with
        | HasOutputClasso "formulation" p -> p.String >>= (Formulation >> Some)
        | HasOutputClasso "energyKj" p -> p.Number >>= (unit<Kj> >> EnergyKj >> Some)
        | HasOutputClasso "energyKcal" p -> p.Number >>= (unit<Kcal> >> EnergyKcal >> Some)
        | HasOutputClasso "proteinGrams" p -> p.Number >>= (unit<g> >> ProteinGrams >> Some)
        | HasOutputClasso "proteinConstituents" p -> p.String >>= (ProteinConstituents >> Some)
        | HasOutputClasso "carbohydrateGrams" p -> p.Number >>= (unit<g> >> CarbohydrateGrams >> Some)
        | HasOutputClasso "carbohydrateConstituents" p -> p.String >>= (CarbohydrateConstituents >> Some)
        | HasOutputClasso "fatGrams" p -> p.Number >>= (unit<g> >> FatGrams >> Some)
        | HasOutputClasso "fatConstituents" p -> p.String >>= (FatConstituents >> Some)
        | HasOutputClasso "fibreGrams" p -> p.Number >>= (unit<g> >> FibreGrams >> Some)
        | HasOutputClasso "specialCharacteristics" p -> p.String >>= (SpecialCharacteristics >> Some)
        | HasOutputClasso "acbs" p -> p |> (Acbs >> Some)
        | HasOutputClasso "presentation" p -> p.String >>= (Presentation >> Some)
        | _ -> None
    static member from (x:bsProvider.Section) =
      let ds = match x with
               | HasOutputClass "details" s ->
                 s.Ps |> Array.choose Detail.from |> Array.toList
                 | _ -> []
      let bsps = match x.Sectiondiv with
                  | Some sd ->  sd.Sectiondivs |> Array.map BorderlineSubstancePrep.from |> Array.toList
                  | None -> []
      Details(ds,bsps)


  type BorderlineSubstance with
    static member parse (x:bsProvider.Topic) =
      let t = x.Title |> Title
      let c = x.Body.Data.Value |> Category
      let note = x.Body.Ps |> Array.tryPick IntroductionNote.from
      let ds = x.Body.Sections |> Array.map Detail.from |> Array.toList

      {id = Id(x.Id)
       title = t;
       category = c;
       intro = note;
       details = ds;
       }

module Interaction =
  type inProvider = XmlProvider<"./samples/superinteraction.xml", Global=true>

  type Link = {url: string; label: string;}

  type Importance =
    | High
    | NotSet
    override __.ToString() = toString __

  type InteractsWith =
    {id:Id;
     title:inProvider.Title;
     importance:Importance;
     message:inProvider.P;
     interactswith:Link;}

  type InteractionList =
    | InteractionList of Id * inProvider.Title * InteractsWith list * Id list


module InteracitonParser =
  open Interaction

  type InteractsWith with
    static member from (x:inProvider.Topic) =
      let t = x.Title
      let p,l = match x.Body.P with
                | Some p ->
                  let ds = p.Phs |> Array.filter (fun p -> p.Outputclass = "drug")
                  let l = match ds.[1].Xref with
                          | Some x -> {url=x.Href;label=x.Value}
                          | None -> failwith "cant find the link"
                  p,l
                | None -> failwith "cant find paragraph"
      let i = match x.Importance with
              | Some "high" -> High
              | _ -> NotSet
      {id=Id(x.Id); title=t; importance = i;message = p; interactswith = l}

  type InteractionList with
    static member parse (x:inProvider.Topic) =
      let is = x.Topics |> Array.map InteractsWith.from |> Array.toList
      let ids = x.Xrefs |> Array.map (fun x -> x.Href |> Id) |> Array.toList
      InteractionList(Id(x.Id),x.Title,is, ids)


module WoundManagement =

  type wmProvider = XmlProvider<"./samples/superwoundmanagment.xml", Global=true, SampleIsList=true>

  type Title =
    | Title of string
    override __.ToString () = match __ with | Title x -> x

  type General = | General of wmProvider.Section

  type TypeOfWound = | TypeOfWound of string

  type Description = | Description of wmProvider.Sectiondiv

  type WoundManagementLink = {id:Id;label:string;rel:string;}

  type WoundExudate = | WoundExudate of string * WoundManagementLink list

  type WoundType = | WoundType of TypeOfWound * Description option * WoundExudate list

  type Product = {
    ampid: int64;
    name: string;
    price: decimal;
    manufacturer: string;
  }

  type ProductGroup = | ProductGroup of Title * Description option * Product list

  type WoundManagement = {
    id: Id;
    title: Title;
    general: General option;
    dressingChoices: WoundType list;
    links: WoundManagementLink list;
    //products: Product list; //need to check if they exist in isolation
    productGroups: ProductGroup list
  }

module WoundManagementParser =
  open WoundManagement

  let desc = Array.tryPick (hasOutputclasso "description" >> Option.map Description)

  type Title with
    static member from (x:wmProvider.P) = x.Value >>= (Title >> Some)

  type WoundManagementLink with
    static member from (x:wmProvider.Xref) =
      {WoundManagementLink.id=Id(x.Href);label=x.Value;rel=x.Rel |? ""}

  type WoundExudate with
    static member list (x:wmProvider.Sectiondiv[]) =
      x |> Array.choose (hasOutputclasso "woundExudate" >> Option.map WoundExudate.from) |> Array.toList
    static member from (x:wmProvider.Sectiondiv) =
      let r = x.Ps.[0].Value.Value
      let ls = x.Xrefs |> Array.map WoundManagementLink.from |> Array.toList
      WoundExudate(r,ls)

  type TypeOfWound with
    static member from (x:wmProvider.P) = x.Value >>= (TypeOfWound >> Some)

  type WoundType with
    static member list (x:wmProvider.Section) =
      x.Sectiondivs |> Array.choose (hasOutputclasso "woundType" >> Option.map WoundType.from)
    static member from (x:wmProvider.Sectiondiv) =
      let tow = x.Ps |> Array.pick (hasOutputclasso "typeOfWound" >> Option.bind TypeOfWound.from)
      let d = x.Sectiondivs |> desc
      let wes = x.Sectiondivs |> WoundExudate.list
      WoundType(tow,d,wes)

  type Product with
    static member from (x:wmProvider.P) =
      let n = x.Phs |> Array.tryPick (hasOutputclass "name")
      let p = x.Phs |> Array.tryPick (hasOutputclass "price")
      let m = x.Phs |> Array.tryPick (hasOutputclass "manufacturer")
      match (x.Data,n,p,m) with
        | (Some d, Some name, Some price, Some man) ->
           {ampid=d.Value;name=name.String.Value;price=price.Number.Value;manufacturer=man.String.Value}
        | _ -> failwith "missing part of the product"

    static member from (x:wmProvider.Sectiondiv) =
      let p = x.Ps |> Array.tryPick (hasOutputclasso "product")
      match p with
        | Some p -> p |> Product.from |> Some
        | None -> None

    static member list (x:wmProvider.Sectiondiv[]) =
      x |> Array.choose Product.from |> Array.toList

  type ProductGroup with
    static member list (x:wmProvider.Section) =
      x.Sectiondivs |> Array.choose (hasOutputclasso "productGroup" >> Option.map ProductGroup.from)
    static member from (x:wmProvider.Sectiondiv) =
      let t = x.Ps |> Array.pick (hasOutputclasso "title" >> Option.bind Title.from)
      let d = x.Sectiondivs |> desc
      let ps = x.Sectiondivs |> Product.list
      ProductGroup(t,d,ps)

  type WoundManagement with
    static member parse (x:wmProvider.Topic) =
      let t = Title x.Title
      let gen = x.Body.Sections |> Array.tryPick (hasOutputclass "general" >> Option.map General)
      let dcs = x.Body.Sections
                |> Array.choose (hasOutputclass "dressingChoices")
                |> Array.collect WoundType.list
                |> Array.toList
      let ls = x.Xrefs |> Array.map WoundManagementLink.from |> Array.toList
      let pgs = x.Body.Sections
                |> Array.choose (hasOutputclass "productGroups")
                |> Array.collect ProductGroup.list
                |> Array.toList
      {id=Id(x.Id);title=t;general=gen;dressingChoices=dcs;links=ls;productGroups=pgs}



module Generic =
  type genericProvider = XmlProvider<"./samples/supercontent.xml", Global=true, SampleIsList=true>

  type Title =
    | Title of string
    override __.ToString () = match __ with | Title x -> x
  type TargetAudience = | TargetAudience of string
  type Content = | Content of genericProvider.Section * TargetAudience option

  type Generic = {
    id:Id;
    title:Title;
    content:Content list;
    links:ContentLink seq
  }

module GenericParser =
  open Generic

  type Content with
    static member from (x:genericProvider.Section) =
      let ta = match x.Outputclass with
               | Some x -> TargetAudience x |> Some
               | None -> None
      Content(x,ta)

  type Generic with
    static member parse (x:genericProvider.Topic) =
      let c = match x.Body with
              | Some b -> b.Sections |> Array.map Content.from |> Array.toList
              | None -> []
      let ls = x.XElement |> ContentLink.from
      {id=Id(x.Id); title = Title(x.Title); content = c; links = ls}



module BorderlineSubstanceTaxonomy =
  type Title = | Title of string

  type BorderlineSubstanceTaxonomy = {
    id:Id;
    title: Title;
    general: drugProvider.Section option
    substances: Id list;
    categories: Id list;
    }


module BorderlineSubstanceTaxonomyParser =
  open BorderlineSubstanceTaxonomy

  type BorderlineSubstanceTaxonomy with
    static member parse (x:drugProvider.Topic) =
      let title = Title(x.Title.Value |? "")
      let general = x |> sections "general" |> Array.tryPick Some
      let ids = match x.Body with
                   | Some b -> b.Sections
                                |> Array.collect (fun s -> s.Ps)
                                |> Array.collect (fun p -> p.Xrefs)
                                |> Array.map (fun x -> x.Href |> Id)
                                |> Array.toList
                   | None -> []
      let cats = x.Xrefs |> Array.map (fun x -> x.Href |> Id) |> Array.toList
      {id=Id(x.Id);title=title;general=general;substances=ids;categories=cats}

module MedicalDevice =
  type Title = | Title of string

  type PrescribingAndDispensingInformation = | PrescribingAndDispensingInformation of drugProvider.Sectiondiv

  type MedicalDevice =
    | MedicalDevice of Id * Title * PrescribingAndDispensingInformation option * Id list


module MedicalDeviceParser =
  open MedicalDevice

  type MedicalDevice with
    static member parse (x:drugProvider.Topic) =
      let title = Title(x.Title.Value |? "")
      let padi = x |> topics "prescribingAndDispensingInformation"
                   |> Array.collect allsections
                   |> Array.tryPick (PrescribingAndDispensingInformation >> Some)

      let id (x:drugProvider.Xref) = x.Href |> Id
      let links (x:drugProvider.Topic) = x.Xrefs |> Array.map id
      let ids = x |> topics "medicalDeviceTypes"
                  |> Array.collect links
                  |> Array.toList
      MedicalDevice(Id(x.Id),title,padi,ids)
