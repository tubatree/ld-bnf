namespace Bnf
open FSharp.Data
open prelude

module Shared =
  type Id =
    | Id of string
    override __.ToString () = match __ with | Id x -> x.Replace(".xml","").Replace("#","")

  type Doi =
    | Doi of string
    override __.ToString () = match __ with | Doi x -> x

  open System.Xml.Linq
  open System.Xml.XPath

  type ContentLink = {id:Id;label:string;rel:string;format:string;} with
    static member from (x:XElement) =
      let build (x:XElement) =
        let href = x.Attribute(XName.Get "href").Value
        let format = x.Attribute(XName.Get "format")
        let rel = x.Attribute(XName.Get "rel")
        if (rel <> null && format <> null && format.Value = "dita") then
         {id = Id(href); label = x.Value; rel = rel.Value; format = format.Value} |> Some
        else
          None
      x.XPathSelectElements("//xref") |> Seq.choose build |> Seq.toList

  //sensible compromise to reference the types provided to avoid replication
  type drugProvider = XmlProvider<"./samples/SuperDrug.xml", Global=true, SampleIsList=true>

  let sections (n:string) (x:drugProvider.Topic) =
    match x.Body with
      | Some(b) -> b.Sections |> Array.choose (hasOutputclasso n)
      | None -> [||]

  let topics (n:string) (x:drugProvider.Topic) =
    x.Topics |> Array.choose (hasOutputclass n)

  let allsections (x:drugProvider.Topic) =
    match x.Body with
        | Some b -> b.Sections |> Array.collect (fun s -> s.Sectiondivs)
        | None -> [||]

  let sectiondivs cl (s:drugProvider.Section[]) =
    s |> Array.choose (hasOutputclasso cl) |> Array.collect (fun sec -> sec.Sectiondivs)

  let somesections cl  (x:drugProvider.Topic) =
    match x.Body with
    | Some b -> b.Sections |> (sectiondivs cl)
    | None -> [||]


module Drug =
    open Shared

    type Paragraph = | Paragraph of string * drugProvider.P option
    type Paragraphs = | Paragraphs of Paragraph seq
    type Title = | Title of Paragraph

    type Link = {Title:string; Url:string}

    type InteractionLink = | InteractionLink of Link

    type ConstituentDrug =
      | ConstituentDrug of Link
      override __.ToString () = match __ with | ConstituentDrug x -> x.Title

    type InheritsFromClass = | InheritsFromClass of string

    type ClassificationType = | Primary | Secondary

    type Classification = | Classification of Id * InheritsFromClass list * ClassificationType

    type DrugName = | DrugName of drugProvider.Title

    type DrugClassName = | DrugClassName of drugProvider.Title

    type CMPIName = | CMPIName of drugProvider.Title

    type Vtmid = | Vtmid of int64

    type MedicinalForm = | MedicinalForm of Link

    type TheraputicIndication = | TheraputicIndication of string * drugProvider.P

    type PatientGroup = {Group:string; Dosage:string; dosageXml:drugProvider.P;}

    type Route =
      | Route of string
      override __.ToString () = match __ with | Route x -> x

    type Indication =
      | Indication of string
      override __.ToString () = match __ with | Indication x -> x

    type Specificity = | Specificity of Paragraph * Route list * Indication list

    type RouteOfAdministration = | RouteOfAdministration of Specificity option * PatientGroup seq 

    type IndicationsAndDose = | IndicationsAndDose  of TheraputicIndication seq * RouteOfAdministration seq

    type IndicationsAndDoseSection =
      | Pharmacokinetics of drugProvider.Section
      | DoseEquivalence of drugProvider.Section
      | DoseAdjustments of drugProvider.Section
      | ExtremesOfBodyWeight of drugProvider.Section
      | Potency of drugProvider.Section

    type GeneralInformation = | GeneralInformation of Option<Title> * Option<Specificity> * drugProvider.Sectiondiv

    type DoseAdjustment = | DoseAdjustment of Option<Title> * Option<Specificity> * drugProvider.Sectiondiv

    type AdditionalMonitoringInPregnancy =
      | AdditionalMonitoringInPregnancy of Option<Title> * Option<Specificity> * drugProvider.Sectiondiv

    type AdditionalMonitoringInBreastFeeding =
      | AdditionalMonitoringInBreastFeeding of Option<Title> * Option<Specificity> * drugProvider.Sectiondiv

    type AdditionalMonitoringInRenalImpairment = | AdditionalMonitoringInRenalImpairment of Title option * Specificity option * drugProvider.Sectiondiv

    type AdditionalMonitoringInHepaticImpairment = | AdditionalMonitoringInHepaticImpairment of Title option * Specificity option * drugProvider.Sectiondiv

    type LicensingVariationStatement = | LicensingVariationStatement of drugProvider.P

    type AdditionalFormsStatement = | AdditionalFormsStatement of drugProvider.P

    //thinking maybe alias this type?
    type PatientAndCarerAdvice =
      | PatientResources of Title option * Specificity option * drugProvider.Sectiondiv
      | AdviceAroundMissedDoses of Title option * Specificity option * drugProvider.Sectiondiv
      | GeneralPatientAdvice of Title option * Specificity option * drugProvider.Sectiondiv
      | AdviceAroundDrivingAndOtherTasks of Title option * Specificity option * drugProvider.Sectiondiv
      | PatientAdviceInPregnancy of Title option * Specificity option * drugProvider.Sectiondiv
      | PatientAdviceInConceptionAndContraception of Title option * Specificity option * drugProvider.Sectiondiv

    type TheraputicUse =
      | TheraputicUse of string * Option<TheraputicUse> 
    type PrimaryTheraputicUse = | PrimaryTheraputicUse of Option<TheraputicUse>

    type SecondaryTheraputicUses = | SecondaryTheraputicUses of Option<TheraputicUse>

    type DomainOfEffect = | DomainOfEffect of
                            Option<string> * Option<PrimaryTheraputicUse> * Option<SecondaryTheraputicUses>

    type PrimaryDomainOfEffect = | PrimaryDomainOfEffect of DomainOfEffect

    type SecondaryDomainsOfEffect = | SecondaryDomainsOfEffect of DomainOfEffect seq

    type AllergyAndCrossSensitivityContraindications =
      | AllergyAndCrossSensitivityContraindications of Title option * Specificity option * drugProvider.Sectiondiv

    type AllergyAndCrossSensitivityCrossSensitivity =
      | AllergyAndCrossSensitivityCrossSensitivity of Title option * Specificity option * drugProvider.Sectiondiv

    type ExceptionToLegalCategory = | ExceptionToLegalCategory of Option<Title> * Option<Specificity> * drugProvider.Sectiondiv

    type DentalPractitionersFormularyInformation = | DentalPractitionersFormularyInformation of Option<Title> * Option<Specificity> * drugProvider.Sectiondiv

    type AdviceForDentalPractitioners = | AdviceForDentalPractitioners of Title option * Specificity option * drugProvider.Sectiondiv

    type EffectOnLaboratoryTest = | EffectOnLaboratoryTest of Title option * Specificity option * drugProvider.Sectiondiv

    type PreTreatmentScreening = | PreTreatmentScreening of Title option * Specificity option * drugProvider.Sectiondiv

    type LessSuitableForPrescribing = | LessSuitableForPrescribing of Option<Title> * Option<Specificity> * drugProvider.Sectiondiv
 
    type HandlingAndStorage = | HandlingAndStorage of Option<Title> * Option<Specificity> * drugProvider.Sectiondiv

    type TreatmentCessation = | TreatmentCessation of Title option * Specificity option * drugProvider.Sectiondiv

    type DrugAction = | DrugAction of Title option * Specificity option * drugProvider.Sectiondiv

    type SideEffectAdvice =
      | SideEffectAdvice of Option<Title> * Option<Specificity> * drugProvider.Sectiondiv

    type SideEffectsOverdosageInformation =
      | SideEffectsOverdosageInformation of Option<Title> * Option<Specificity> * drugProvider.Sectiondiv

    type SideEffect = | SideEffect of drugProvider.Ph

    type Frequency = {
      frequencyid:string;
      label:string;
    }

    type SideEffectsGroup =
      | GeneralSideEffects of Frequency * drugProvider.P * SideEffect list
      | SideEffectsWithRoutes of Frequency * Specificity * drugProvider.P * SideEffect list
      | SideEffectsWithIndications of Frequency * Specificity * drugProvider.P * SideEffect list

    type Contraindication = | Contraindication of drugProvider.Ph

    type ContraindicationsGroup =
      | GeneralContraindications of drugProvider.P * Contraindication list
      | ContraindicationWithRoutes of Specificity * drugProvider.P * Contraindication list
      | ContraindicationWithIndications of Specificity * drugProvider.P * Contraindication list

    type ImportantAdvice = | ImportantAdvice of Title option * Specificity option * drugProvider.Sectiondiv

    type ContraindicationsRenalImpairment = | ContraindicationsRenalImpairment of Title option * Specificity option * drugProvider.Sectiondiv

    type Caution = Caution of drugProvider.Ph

    type CautionsGroup =
      | GeneralCautions of drugProvider.P * Caution list
      | CautionsWithRoutes of Specificity * drugProvider.P * Caution list
      | CautionsWithIndications of Specificity * drugProvider.P * Caution list

    type PrescribingAndDispensingInformation =
      | PrescribingAndDispensingInformation of Option<Title> * Option<Specificity> * drugProvider.Sectiondiv

    type UnlicencedUse =
      | UnlicencedUse of Option<Title> * Option<Specificity> * drugProvider.Sectiondiv

    type MonitoringRequirement =
      | PatientMonitoringProgrammes of Option<Title> * Option<Specificity> * drugProvider.Sectiondiv
      | TheraputicDrugMonitoring of Option<Title> * Option<Specificity> * drugProvider.Sectiondiv
      | MonitoringOfPatientParameters of Option<Title> * Option<Specificity> * drugProvider.Sectiondiv

    type ConceptionAndContraception =
      | ConceptionAndContraception of Option<Title> * Option<Specificity> * drugProvider.Sectiondiv

    type ImportantSafetyInformation =
      | ImportantSafetyInformation of Option<Title> * Option<Specificity> * drugProvider.Sectiondiv

    type DirectionForAdministration =
      | DirectionForAdministration of Option<Title> * Option<Specificity> * drugProvider.Sectiondiv

    type FundingIdentifier =
      | FundingIdentifier of Link
      override __.ToString () = match __ with | FundingIdentifier x -> x.Title

    type FundingDecision =
      | NonNHS of Specificity option * drugProvider.Sectiondiv
      | NiceTechnologyAppraisals of FundingIdentifier option * Title option * Specificity option * drugProvider.Sectiondiv option
      | SmcDecisions of Specificity option * drugProvider.Sectiondiv

    type InteractionStatement = | InteractionStatement of Title option * Specificity option * drugProvider.Sectiondiv

    type MonographSection =
        | IndicationsAndDoseGroup of Id * IndicationsAndDose seq * IndicationsAndDoseSection seq
        | Pregnancy of Id * GeneralInformation seq * DoseAdjustment seq * AdditionalMonitoringInPregnancy seq
        | BreastFeeding of Id * GeneralInformation seq * AdditionalMonitoringInBreastFeeding seq * DoseAdjustment seq
        | HepaticImpairment of Id * GeneralInformation seq * DoseAdjustment seq * AdditionalMonitoringInHepaticImpairment seq
        | RenalImpairment of Id * GeneralInformation seq * AdditionalMonitoringInRenalImpairment seq * DoseAdjustment seq
        | PatientAndCarerAdvices of Id * PatientAndCarerAdvice seq
        | MedicinalForms of Id * LicensingVariationStatement option * AdditionalFormsStatement option * MedicinalForm seq
        | AllergyAndCrossSensitivity of Id * AllergyAndCrossSensitivityContraindications seq * AllergyAndCrossSensitivityCrossSensitivity seq
        | ExceptionsToLegalCategory of Id * ExceptionToLegalCategory seq
        | ProfessionSpecificInformation of Id * DentalPractitionersFormularyInformation seq * AdviceForDentalPractitioners seq
        | EffectOnLaboratoryTests of Id * EffectOnLaboratoryTest seq
        | PreTreatmentScreenings of Id * PreTreatmentScreening seq
        | LessSuitableForPrescribings of Id * LessSuitableForPrescribing seq
        | HandlingAndStorages of Id * HandlingAndStorage seq
        | TreatmentCessations of Id * TreatmentCessation seq
        | DrugActions of Id * DrugAction seq
        | SideEffects of Id * SideEffectsGroup seq * SideEffectAdvice seq * SideEffectsOverdosageInformation seq
        | Contraindications of Id * ContraindicationsGroup seq * ImportantAdvice seq * ContraindicationsRenalImpairment seq
        | Cautions of Id * CautionsGroup list * ImportantAdvice seq
        | PrescribingAndDispensingInformations of Id * PrescribingAndDispensingInformation seq
        | UnlicencedUses of Id * UnlicencedUse seq
        | MonitoringRequirements of Id * MonitoringRequirement seq
        | ConceptionAndContraceptions of Id * ConceptionAndContraception seq
        | ImportantSafetyInformations of Id * ImportantSafetyInformation seq
        | DirectionsForAdministration of Id * DirectionForAdministration seq
        | NationalFunding of Id * FundingDecision seq
        | InteractionStatements of Id * InteractionStatement seq

    type Synonyms = | Synonyms of string

    type Drug = {id : Id;
                 name : DrugName;
                 interactionLinks : InteractionLink seq;
                 constituentDrugs : ConstituentDrug seq;
                 classifications : Classification seq;
                 vtmid : Option<Vtmid>;
                 synonyms : Synonyms option;
                 sections : MonographSection seq;
                 primaryDomainOfEffect : Option<PrimaryDomainOfEffect>;
                 secondaryDomainsOfEffect : Option<SecondaryDomainsOfEffect>;}

    type DrugClass = {id : Id;
                      dcname : DrugClassName;
                      sections : MonographSection seq;}

    type CMPI = {id: Id;
                 cmpiname : CMPIName;
                 sections: MonographSection seq;}

module DrugParser =
    open Drug
    open Shared

    type Paragraph with
      static member from (x:drugProvider.P) =
        Paragraph(x.Value |? "",Some x)

    type Paragraphs with
        static member from (x:Option<drugProvider.Sectiondiv>) =
          match x with
            | Some(x) -> Paragraphs.fromsd x
            | None -> Paragraphs Array.empty<Paragraph>
        static member fromsd (x:drugProvider.Sectiondiv) =
          x.Ps |> Seq.map Paragraph.from |> Paragraphs
        static member froms (x:drugProvider.Section) =
          x.Ps |> Seq.map Paragraph.from |> Paragraphs

    type Route with
      static member from (Paragraph(x,_)) = Route x
      static member from (Paragraphs xs) = Seq.map Route.from xs
      static member from (x:drugProvider.Ph) = Route(x.String.Value)

    type Link with
      static member from (r:drugProvider.Xref) =
        {Url = r.Href; Title = r.Value |? ""}
      static member from (x:drugProvider.P) =
        x.Xrefs |> Array.map Link.from |> Array.tryPick Some
      static member from (x:drugProvider.Sectiondiv) =
        x.Ps |> Array.choose Link.from

    type Indication with
      static member from (x:drugProvider.Ph) = Indication(x.String.Value)

    type Specificity with
      static member from (x:drugProvider.P) =
        let rs = x.Phs |> Array.choose (hasOutputclass "route") |> Array.map Route.from |> Array.toList
        let is = x.Phs |> Array.choose (hasOutputclass "indication") |> Array.map Indication.from |> Array.toList
        Specificity(Paragraph.from x,rs,is)
      static member from (x:string) =
        Specificity(Paragraph(x,None),[],[])

    let extractSpecificity (x:drugProvider.Sectiondiv) =
      x.Ps |> Array.choose (hasOutputclasso "specificity") |> Array.map Specificity.from |> Array.tryPick Some

    let addSpecificity x =
      extractSpecificity x, x

    type InteractionLink with
        static member from (x:drugProvider.Xref) = InteractionLink {Url = x.Href.Replace(".xml", "") ; Title = x.Value |? ""}

    type ConstituentDrug with
        static member from (x:drugProvider.Xref)= ConstituentDrug {Url = x.Href.Replace(".xml", ""); Title = x.Value |? ""}

    type MedicinalForm with
        static member from (x:drugProvider.Xref) = MedicinalForm {Url = x.Href.Replace(".xml", ""); Title = x.Value |? ""}

    type PatientGroup with
      static member from (x:drugProvider.Li) =
        match x.Ps with
          |[| g; p |] -> {Group = g.Value |? ""; Dosage = p.Value |? ""; dosageXml = p} |> Some
          | _ -> None

    type TheraputicIndication with
      static member from (x:Paragraph) =
        match x with
          | Paragraph (s,p) ->
            match p with
              | Some p -> TheraputicIndication(s,p)
              | None -> failwith "Missing paragraph"
      static member from (Paragraphs xs) = Seq.map TheraputicIndication.from xs

    type IndicationsAndDose with
      static member from (x:drugProvider.Section) =
         let theraputicIndications = x.Sectiondivs.[0] |> ( Paragraphs.fromsd >> TheraputicIndication.from )
         let specificities = x.Ps |> Array.map (Specificity.from >> Some)
         let groups = x.Uls |> Array.map (fun u -> u.Lis |> Seq.choose PatientGroup.from)
         //if there are no routes then return something else
         let routesOfAdministration =
            match specificities with
            | [||] -> [|RouteOfAdministration(None,groups.[0]) |]
            | _ -> Array.zip specificities groups |> Array.map RouteOfAdministration
         IndicationsAndDose.IndicationsAndDose(theraputicIndications,routesOfAdministration)

    type IndicationsAndDoseSection with
      static member from (x:drugProvider.Section) =
        match x with
          | HasOutputClasso "pharmacokinetics" _ -> Pharmacokinetics x |> Some
          | HasOutputClasso "doseEquivalence" _ -> DoseEquivalence x |> Some
          | HasOutputClasso "doseAdjustments" _ -> DoseAdjustments x |> Some
          | HasOutputClasso "extremesOfBodyWeight" _ -> ExtremesOfBodyWeight x |> Some
          | HasOutputClasso "potency" _ -> Potency x |> Some
          | _ -> None

    type MonographSection with
      static member indicationsAndDoseGroup (x:drugProvider.Topic) =
        match x.Body with
          | Some(b) ->
             let grps = b.Sections |> Array.choose (hasOutputclasso "indicationAndDoseGroup") |> Array.map IndicationsAndDose.from
             let idgss = b.Sections |> Array.choose IndicationsAndDoseSection.from
             Some(IndicationsAndDoseGroup(Id(x.Id),grps,idgss))
          | None -> None

    let statements (b:drugProvider.Body option) =
      match b with
        | Some b ->
            match b.Ps with
                | [||] -> None,None
                | [|lvs|] -> LicensingVariationStatement lvs |> Some, None
                | [|lvs;afs|] -> LicensingVariationStatement lvs |> Some, AdditionalFormsStatement afs |> Some
                | _ -> failwith "licensingVariationStatement too many paragaraphs"
        | None -> None,None

    let medicinalForms (x:drugProvider.Topic) =
      let lvs,avs = statements x.Body
      let links = x.Xrefs |> Array.map MedicinalForm.from
      MedicinalForms(Id(x.Id),lvs,avs,links)

    let inline optn t f x = Some (t (f x))

    let inline opt t f x =
        let y = f x
        match y with
          | Some(y) -> Some (t (y))
          | None -> None

    type Title with
      static member from (x:drugProvider.P) =
        Title(Paragraph.from x) 

    let extractTitle (x:drugProvider.Sectiondiv) =
      x.Ps |> Array.choose (hasOutputclasso "title") |> Array.map Title.from |> Array.tryPick Some

    let addTitle (sp,s) =
      extractTitle s,sp,s

    type GeneralInformation with
      static member from (x:drugProvider.Sectiondiv) = x |> (addSpecificity >> addTitle >> GeneralInformation)
      static member from (x:drugProvider.Section) =
        x.Sectiondivs |> Array.map GeneralInformation.from

    type DoseAdjustment with
      static member from (x:drugProvider.Sectiondiv) = x |> (addSpecificity >> addTitle >> DoseAdjustment)
      static member from (x:drugProvider.Section) =
          x.Sectiondivs |> Array.map DoseAdjustment.from

    let subsections cl c s =
      s |> Array.choose (hasOutputclasso cl) |> Array.collect c
 
    let renalImpairment (x:drugProvider.Topic) =
      match x.Body with
        | Some(b) -> 
           let gi = b.Sections |> subsections "generalInformation" GeneralInformation.from
           let am = x |> (somesections "additionalMonitoringInRenalImpairment")
                      |> Array.map (addSpecificity >> addTitle >> AdditionalMonitoringInRenalImpairment)
                      |> Array.toSeq
           let da = b.Sections |> subsections "doseAdjustments" DoseAdjustment.from
           Some(RenalImpairment(Id(x.Id),gi,am,da))
        | None -> None

    let patientAndCarerAdvice (x:drugProvider.Topic) =
      let c (s:drugProvider.Section) =
        let pca f (s:drugProvider.Section) = s.Sectiondivs |> Array.map (addSpecificity >> addTitle >> f)
        match s with
          | HasOutputClasso "patientResources" s -> s |> pca PatientResources
          | HasOutputClasso "adviceAroundMissedDoses" s -> s |> pca AdviceAroundMissedDoses
          | HasOutputClasso "generalPatientAdvice" s -> s |> pca GeneralPatientAdvice
          | HasOutputClasso "adviceAroundDrivingAndOtherTasks" s -> s |> pca AdviceAroundDrivingAndOtherTasks
          | HasOutputClasso "patientAdviceInPregnancy" s -> s|> pca PatientAdviceInPregnancy
          | HasOutputClasso "patientAdviceInConceptionAndContraception" s -> s |> pca PatientAdviceInConceptionAndContraception
          | _ -> failwith ("patientAndCarerAdvice missed " + s.Outputclass.Value)
      match x.Body with
        | Some(b) ->
            let a = b.Sections |> Array.collect c
            Some(PatientAndCarerAdvices(Id(x.Id),a))
        | None -> None

    let withname = (|HasName|_|)
    let withclass = (|HasOutputClasso|_|)

    type InheritsFromClass with
      static member from (x:drugProvider.Data) =
        match x.String with
          | Some(s) -> InheritsFromClass s |> Some
          | None -> None


    type Classification with
      static member from (x:drugProvider.Data) =
        let l = x.Datas |> Array.tryPick (Some >=> withname "drugClassification" >=> (fun d -> d.String >>= (Id >> Some)))
        let i = x.Datas |> Array.choose (Some >=> withname "inheritsFromClass" >=> InheritsFromClass.from) |> Array.toList
        match l with
          | Some(l) -> Some( Classification(l,i, Primary))
          | None -> None

      static member fromlist (x:drugProvider.Data) =
        let ty = match x.Type with
                 | Some "primary" -> Primary
                 | _ -> Secondary

        //recursively flatten the classificaiton data's to make them easier to fold
        let rec flatten (x:drugProvider.Data) =
          let children = x.Datas |> Array.choose (hasName "classification") |> Array.toList
          match children with
            | [] -> [x]
            | [d] -> x :: (flatten d)
            | _ -> []

        let cs = x.Datas |> Array.toList |> List.collect flatten

        //gather up the inherits from but only take the last classification id
        let folder (Classification(st_id,st_in,st_type)) (x:drugProvider.Data) =
          let cl = Classification.from x
          match cl with
            | Some(Classification(id,inherits,typ)) -> Classification(id,st_in @ inherits,typ)
            | None -> Classification(st_id,st_in,st_type)

        cs |> List.fold folder (Classification(Id(""),[],ty))


    type TheraputicUse with
      static member from (x:drugProvider.Data) =
        TheraputicUse(x.String.Value,TheraputicUse.from x.Datas)
      static member from (x:drugProvider.Data []) =
        x |> Array.tryPick (Some >=> withname "therapeuticUse" >>| TheraputicUse.from)

    type SecondaryTheraputicUses with
      static member from (x:drugProvider.Data) =
        SecondaryTheraputicUses(TheraputicUse.from x.Datas)

    type PrimaryTheraputicUse with
      static member from (x:drugProvider.Data) =
        PrimaryTheraputicUse(TheraputicUse.from x.Datas)

    type DomainOfEffect with
      static member from (x:drugProvider.Data) =
        let p = x.Datas |> Array.tryPick (Some >=> withname "primaryTherapeuticUse" >>| PrimaryTheraputicUse.from)
        let s = x.Datas |> Array.tryPick (Some >=> withname "secondaryTherapeuticUses" >>| SecondaryTheraputicUses.from)
        DomainOfEffect(x.String,p,s)

    type PrimaryDomainOfEffect with
      static member from (x:drugProvider.Body) =
        let d = x.Datas |> Array.tryPick (Some >=> withname "primaryDomainOfEffect" >>| PrimaryDomainOfEffect.from)
        match d with
          | Some(d) -> Some(PrimaryDomainOfEffect(d))
          | None -> None
      static member from (x:drugProvider.Data) =
        x.Datas |> Array.pick (Some >=> withname "domainOfEffect" >>| DomainOfEffect.from)

    type SecondaryDomainsOfEffect with
      static member from (x:drugProvider.Body) =
        let ds = x.Datas
                 |> Array.choose (Some >=> withname "secondaryDomainsOfEffect" >>| SecondaryDomainsOfEffect.from)
                 |> Array.collect id
        match ds with
          | [||] -> None
          | _ -> Some(SecondaryDomainsOfEffect(ds))
      static member from (x:drugProvider.Data) =
        x.Datas |> Array.choose (Some >=> withname "domainOfEffect" >>| DomainOfEffect.from)

    type Vtmid with
      static member from (x:drugProvider.Data) =
        match x.String with
          | Some(n) -> Some(Vtmid(int64 n))
          | None -> None

    type MonographSection with
      static member buidgi c (x:drugProvider.Topic) =
        match x.Body with
          | Some(b) ->
              let gi =b.Sections |> subsections "generalInformation" GeneralInformation.from |> Array.toSeq
              Some(c(Id(x.Id),gi))
          | None -> None
      static member pregnancyfrom (x:drugProvider.Topic) =
        let gis = x |> (somesections "generalInformation") |> Array.map GeneralInformation.from
        let das = x |> (somesections "doseAdjustments") |> Array.map DoseAdjustment.from
        let amps = x|> (somesections "additionalMonitoringInPregnancy") |> Array.map (addSpecificity >> addTitle >> AdditionalMonitoringInPregnancy)
        Pregnancy(Id(x.Id),gis,das,amps)
      static member breastFeedingFrom (x:drugProvider.Topic) =
        let gis = x |> (somesections "generalInformation") |> Array.map GeneralInformation.from
        let ambfs = x |> (somesections "additionalMonitoringInBreastFeeding") |> Array.map (addSpecificity >> addTitle >> AdditionalMonitoringInBreastFeeding)
        let das = x |> (somesections "doseAdjustments") |> Array.map (addSpecificity >> addTitle >> DoseAdjustment)
        BreastFeeding(Id(x.Id),gis,ambfs,das)
      static member hepaticImparmentFrom (x:drugProvider.Topic) =
        match x.Body with
          | Some(b) ->
                    let hi = b.Sections |> subsections "doseAdjustments" DoseAdjustment.from |> Array.toSeq
                    let am = x |> (somesections "additionalMonitoringInHepaticImpairment") |> Array.map (addSpecificity >> addTitle >> AdditionalMonitoringInHepaticImpairment) |> Array.toSeq
                    let c (i,gi) = HepaticImpairment(i, gi, hi, am) //build a partial constructor
                    MonographSection.buidgi c x
          | None -> None

    type AllergyAndCrossSensitivityContraindications with
      static member from (x:drugProvider.Sectiondiv) = x |> (addSpecificity >> addTitle >> AllergyAndCrossSensitivityContraindications)
      static member from (x:drugProvider.Section) =
                x.Sectiondivs |> Array.map AllergyAndCrossSensitivityContraindications.from

    type AllergyAndCrossSensitivityCrossSensitivity with
      static member from (x:drugProvider.Sectiondiv) = x |> (addSpecificity >> addTitle >> AllergyAndCrossSensitivityCrossSensitivity)
      static member from (x:drugProvider.Section) =
                x.Sectiondivs |> Array.map AllergyAndCrossSensitivityCrossSensitivity.from

    type MonographSection with
      static member allergyAndCrossSensitivity (x:drugProvider.Topic) =
        let ac = x |> (somesections "allergyAndCrossSensitivityContraindications") |> Array.map (addSpecificity >> addTitle >> AllergyAndCrossSensitivityContraindications)
        let acss = x |> (somesections "allergyAndCrossSensitivityCrossSensitivity") |> Array.map (addSpecificity >> addTitle >> AllergyAndCrossSensitivityCrossSensitivity)
        AllergyAndCrossSensitivity(Id(x.Id),ac,acss)

    type ExceptionToLegalCategory with
      static member from (x:drugProvider.Sectiondiv) =
                x |> (addSpecificity >> addTitle >> ExceptionToLegalCategory)

    type MonographSection with
      static member exceptionsToLegalCategory (x:drugProvider.Topic) =
        let es = match x.Body with
                       | Some(b) -> b.Sections |> Array.collect (fun s -> s.Sectiondivs) |> Array.map ExceptionToLegalCategory.from
                       | None -> Array.empty<ExceptionToLegalCategory>
        ExceptionsToLegalCategory(Id(x.Id),es)

    type DentalPractitionersFormularyInformation with
      static member from (x:drugProvider.Sectiondiv) =
        x |> (addSpecificity >> addTitle >> DentalPractitionersFormularyInformation)
      static member from (x:drugProvider.Section) =
        x.Sectiondivs |> Array.map DentalPractitionersFormularyInformation.from

    type MonographSection with
      static member professionSpecificInformation (x:drugProvider.Topic) =
        let psi = x |> somesections "dentalPractitionersFormulary" |> Array.map (addSpecificity >> addTitle >> DentalPractitionersFormularyInformation)
        let adp = x |> somesections "adviceForDentalPractitioners" |> Array.map (addSpecificity >> addTitle >> AdviceForDentalPractitioners)
        ProfessionSpecificInformation(Id(x.Id),psi,adp)

    type SideEffectsGroup with
      static member sideEffects (ps:drugProvider.P[]) =
        let f = function
          | _,[] -> None
          | p,s -> Some(p,s)
        ps |> Array.map (fun p -> p,p.Phs |> Array.map SideEffect |> Array.toList) |> Array.pick f

      static member title (x:drugProvider.Sectiondiv) =
        x.Ps |> Array.pick (fun p -> match p with
                                                | HasOutputClasso "title" p -> Some(p.Value.Value)
                                                | _ -> None)
      static member frequency (x:drugProvider.Sectiondiv) =
        let l = x |> SideEffectsGroup.title
        match x.Outputclass,l with
          | Some(c),l -> {frequencyid=c;label=l}
          | _ -> failwith "missing parts of the frequency"

      static member fromge (x:drugProvider.Sectiondiv) =
        let f = SideEffectsGroup.frequency x
        let p,s = x.Ps |> SideEffectsGroup.sideEffects
        GeneralSideEffects(f,p,s)
      static member fromsp (x:drugProvider.Sectiondiv) =
        let se' fc f (s:drugProvider.Sectiondiv) =
          let i = s |> (SideEffectsGroup.title >> fc)
          let p,s = s.Ps |> SideEffectsGroup.sideEffects
          (f,i,p,s)

        let se (s:drugProvider.Sectiondiv) =
          let f = x |> SideEffectsGroup.frequency
          match s with
            | HasOutputClasso "sideEffectsWithIndications" _ ->
              SideEffectsWithIndications(s |> se' Specificity.from f)
            | HasOutputClasso "sideEffectsWithRoutes" _ ->
              SideEffectsWithRoutes(s |> se' Specificity.from f)
            | _ -> failwith "unmatched side effect"

        x.Sectiondivs |> Array.map se

    type SideEffectAdvice with
      static member from (x:drugProvider.Sectiondiv) =
        x |> (addSpecificity >> addTitle >> SideEffectAdvice)

    type SideEffectsOverdosageInformation with
      static member from (x:drugProvider.Sectiondiv) =
        x |> (addSpecificity >> addTitle >> SideEffectsOverdosageInformation)

    type Contraindication with
      static member from (x:drugProvider.Ph) = Contraindication x
    type ContraindicationsGroup with
      static member from (x:drugProvider.Section) =
        let gen p = GeneralContraindications(p, p.Phs |> Array.map Contraindication.from |> Array.toList)
        let ac (x:drugProvider.Sectiondiv) =
          match x with
            | HasOutputClasso "cautionsOrContraindicationsWithRoutes" s ->
                ContraindicationWithRoutes(Specificity.from(s.Ps.[0].Value.Value), s.Ps.[1], s.Ps.[1].Phs |> Array.map Contraindication.from |> Array.toList)
            | HasOutputClasso "cautionsOrContraindicationsWithIndications" s ->
              ContraindicationWithIndications(Specificity.from(s.Ps.[0].Value.Value), s.Ps.[1], s.Ps.[1].Phs |> Array.map Contraindication.from |> Array.toList)
            | _ -> failwith "unmatched ouput class" 
        [|x.Ps |> Array.map gen
          x.Sectiondivs |> Array.choose (hasOutputclasso "additionalContraindications") |> Array.collect (fun sd -> sd.Sectiondivs |> Array.map ac) |] |> Array.collect id


    type Caution with
      static member from (x:drugProvider.Ph) = Caution x
    type CautionsGroup with
      static member from (x:drugProvider.Section) =
        let gen p = GeneralCautions(p, p.Phs |> Array.map Caution.from |> Array.toList)
        let ac (x:drugProvider.Sectiondiv) =
          match x with
            | HasOutputClasso "cautionsOrContraindicationsWithRoutes" s ->
                CautionsWithRoutes(Specificity.from(s.Ps.[0].Value.Value), s.Ps.[1], s.Ps.[1].Phs |> Array.map Caution.from |> Array.toList)
            | HasOutputClasso "cautionsOrContraindicationsWithIndications" s ->
                CautionsWithIndications(Specificity.from(s.Ps.[0].Value.Value), s.Ps.[1], s.Ps.[1].Phs |> Array.map Caution.from |> Array.toList)
            | _ -> failwith "unmatched output class"

        [|x.Ps |> Array.map gen
          x.Sectiondivs |> Array.choose (hasOutputclasso "additionalCautions") |> Array.collect (fun sd -> sd.Sectiondivs |> Array.map ac) |] |> Array.collect id

    let firstsection n (x:drugProvider.Topic) =
      match x.Body with
        | Some b -> b.Sections |> Array.tryPick n
        | None -> None

    type MonitoringRequirement with
      static member from (x:drugProvider.Section) =
        let build c = x.Sectiondivs |> Array.map (addSpecificity >> addTitle >> c)
        match x with
          | HasOutputClasso "patientMonitoringProgrammes" _ -> build PatientMonitoringProgrammes
          | HasOutputClasso "therapeuticDrugMonitoring" _ -> build TheraputicDrugMonitoring
          | HasOutputClasso "monitoringOfPatientParameters" _ -> build MonitoringOfPatientParameters
          | _ -> failwith "unmatched output class"

    //build function to create the funding identifier
    type FundingIdentifier with
      static member from l (x:drugProvider.P) =
        let fi v = FundingIdentifier {Title=v;Url=l}
        x.Value >>= (fi >> Some)

    type FundingDecision with
      static member from (x:drugProvider.Sectiondiv) =
        let buildTa (s1:drugProvider.Sectiondiv) =
          let fid = function
            | HasOutputClasso "fundingIdentifier" p -> p |> FundingIdentifier.from s1.Xref.Value.Value.Value
            | _ -> None
          let fi = s1.Ps |> Array.tryPick fid
          match s1.Sectiondivs with
            | [|head|] ->
              let (t,sp,s) = head |> (addSpecificity >> addTitle)
              NiceTechnologyAppraisals (fi,t,sp,Some s)
            | _ -> NiceTechnologyAppraisals (fi,None,None,None)

        let buildSmc (s1:drugProvider.Sectiondiv) = s1.Sectiondivs.[0] |> (addSpecificity >> SmcDecisions)

        match x with
          | HasOutputClasso "niceTechnologyAppraisals" _ ->
             x.Sectiondivs |> Array.map buildTa
          | HasOutputClasso "smcDecisions" _ ->
             x.Sectiondivs |> Array.map buildSmc
          | _ -> sprintf "unmatched type of funding decision %s" x.Outputclass.Value |> failwith
      static member fromfd (x:drugProvider.Section) =
        match x with
          | HasOutputClasso "nonNHS" _ -> x.Sectiondivs |> Array.map (addSpecificity >> NonNHS)
          | _ -> x.Sectiondivs |> Array.collect FundingDecision.from 

    type MonographSection with
      static member effectOnLaboratoryTests (x:drugProvider.Topic) =
        EffectOnLaboratoryTests(Id(x.Id),allsections x |> Array.map (addSpecificity >> addTitle >> EffectOnLaboratoryTest))
      static member preTreatmentScreenings (x:drugProvider.Topic) =
        PreTreatmentScreenings(Id(x.Id), allsections x |> Array.map (addSpecificity >> addTitle >> PreTreatmentScreening))
      static member lessSuitableForPrescribings (x:drugProvider.Topic) =
        LessSuitableForPrescribings(Id(x.Id), allsections x |> Array.map (addSpecificity >> addTitle >> LessSuitableForPrescribing))
      static member handlingAndStorages (x:drugProvider.Topic) =
        HandlingAndStorages(Id(x.Id), allsections x |> Array.map (addSpecificity >> addTitle >> HandlingAndStorage))
      static member treatmentCessations (x:drugProvider.Topic) =
        TreatmentCessations(Id(x.Id), allsections x |> Array.map (addSpecificity >> addTitle >> TreatmentCessation))
      static member drugActions (x:drugProvider.Topic) =
        DrugActions(Id(x.Id), allsections x |> Array.map (addSpecificity >> addTitle >> DrugAction))
      static member sideEffects (x:drugProvider.Topic) =
        let gse = x |> (somesections "generalSideEffects")
                    |> Array.choose (hasOutputclasso "frequencies")
                    |> Array.collect (fun f -> f.Sectiondivs |> Array.map SideEffectsGroup.fromge)
        let sse = x |> (somesections "specificSideEffects")
                    |> Array.choose (hasOutputclasso "frequencies")
                    |> Array.collect (fun f -> f.Sectiondivs |> Array.collect SideEffectsGroup.fromsp)
        let adv = x |> (somesections "sideEffectsAdvice")
                    |> Array.map SideEffectAdvice.from
        let ods = x |> (somesections "sideEffectsOverdosageInformation")
                    |> Array.map SideEffectsOverdosageInformation.from
        SideEffects(Id(x.Id), Array.concat [gse;sse] ,adv, ods)
      static member contraindications (x:drugProvider.Topic) =
        let s = firstsection (withclass "contraindications") x
        let cgs = match s with
                  | Some (s) -> ContraindicationsGroup.from s |> Array.toList
                  | None -> List.empty<ContraindicationsGroup>
        let ias = x |> (somesections "importantAdvice") |> Array.map (addSpecificity >> addTitle >> ImportantAdvice)
        let ciri = x |> (somesections "contraindicationsRenalImpairment") |> Array.map (addSpecificity >> addTitle >> ContraindicationsRenalImpairment)
        Contraindications(Id(x.Id), cgs, ias, ciri)

      static member cautions (x:drugProvider.Topic) =
        let s = firstsection (withclass "cautions") x
        let cgs = match s with
                   | Some (s) -> CautionsGroup.from s |> Array.toList
                   | None -> List.empty<CautionsGroup>
        let ias = x |> (somesections "importantAdvice") |> Array.map (addSpecificity >> addTitle >> ImportantAdvice)
        Cautions(Id(x.Id), cgs, ias)
      static member prescribingAndDispensingInformation (x:drugProvider.Topic) =
        PrescribingAndDispensingInformations(Id(x.Id), allsections x |> Array.map (addSpecificity >> addTitle >> PrescribingAndDispensingInformation))
      static member unlicencedUse (x:drugProvider.Topic) =
        UnlicencedUses(Id(x.Id), allsections x |> Array.map (addSpecificity >> addTitle >> UnlicencedUse))
      static member monitoringRequirements (x:drugProvider.Topic) =
        match x.Body with
          | Some b -> MonitoringRequirements(Id(x.Id),b.Sections |> Array.collect MonitoringRequirement.from)
          | None -> MonitoringRequirements(Id(x.Id), Array.empty<MonitoringRequirement>)
      static member conceptionAndContraception (x:drugProvider.Topic) =
        ConceptionAndContraceptions(Id(x.Id), allsections x |> Array.map (addSpecificity >> addTitle >> ConceptionAndContraception))
      static member importantSafetyInformation (x:drugProvider.Topic) =
        ImportantSafetyInformations(Id(x.Id), allsections x |> Array.map (addSpecificity >> addTitle >> ImportantSafetyInformation))

      static member directionsForAdministration (x:drugProvider.Topic) =
        DirectionsForAdministration(Id(x.Id), allsections x |> Array.map (addSpecificity >> addTitle >> DirectionForAdministration))
      static member nationalFunding (x:drugProvider.Topic) =
        let fds =  match x.Body with
                         | Some b -> b.Sections |> Array.collect FundingDecision.fromfd
                         | None -> [||]
        NationalFunding(Id(x.Id),fds)
      static member interactionStatements (x:drugProvider.Topic) =
        let fs = x |> firstsection (withclass "general")
        let is = match fs with
                 | Some s -> s.Sectiondivs |> Array.map (addSpecificity >> addTitle >> InteractionStatement)
                 | None -> [||]
        InteractionStatements(Id(x.Id),is)

    type MonographSection with
      static member section x =
        match x with
        | HasOutputClass "indicationsAndDose" _ -> MonographSection.indicationsAndDoseGroup x
        | HasOutputClass "pregnancy" _ -> MonographSection.pregnancyfrom x |> Some
        | HasOutputClass "breastFeeding" _ -> MonographSection.breastFeedingFrom x |> Some
        | HasOutputClass "hepaticImpairment" _ -> MonographSection.hepaticImparmentFrom x
        | HasOutputClass "renalImpairment" _ -> renalImpairment x
        | HasOutputClass "patientAndCarerAdvice" _ -> patientAndCarerAdvice x
        | HasOutputClass "medicinalForms" _ -> Some(medicinalForms x)
        | HasOutputClass "exceptionsToLegalCategory" _ -> Some(MonographSection.exceptionsToLegalCategory x)
        | HasOutputClass "professionSpecificInformation" _ -> Some(MonographSection.professionSpecificInformation x)
        | HasOutputClass "effectOnLaboratoryTests" _ -> Some(MonographSection.effectOnLaboratoryTests x)
        | HasOutputClass "preTreatmentScreening" _ -> Some(MonographSection.preTreatmentScreenings x)
        | HasOutputClass "lessSuitableForPrescribing" _ -> Some(MonographSection.lessSuitableForPrescribings x)
        | HasOutputClass "handlingAndStorage" _ -> Some(MonographSection.handlingAndStorages x)
        | HasOutputClass "treatmentCessation" _ -> Some(MonographSection.treatmentCessations x)
        | HasOutputClass "allergyAndCrossSensitivity" _ -> Some(MonographSection.allergyAndCrossSensitivity x)
        | HasOutputClass "drugAction" _ -> Some(MonographSection.drugActions x)
        | HasOutputClass "sideEffects" _ -> Some(MonographSection.sideEffects x)
        | HasOutputClass "contraindications" _ -> Some(MonographSection.contraindications x)
        | HasOutputClass "cautions" _ -> Some(MonographSection.cautions x)
        | HasOutputClass "prescribingAndDispensingInformation" _ -> Some(MonographSection.prescribingAndDispensingInformation x)
        | HasOutputClass "unlicensedUse" _ -> Some(MonographSection.unlicencedUse x)
        | HasOutputClass "monitoringRequirements" _ -> Some(MonographSection.monitoringRequirements x)
        | HasOutputClass "conceptionAndContraception" _ -> Some(MonographSection.conceptionAndContraception x)
        | HasOutputClass "importantSafetyInformation" _ -> Some(MonographSection.importantSafetyInformation x)
        | HasOutputClass "directionsForAdministration" _ -> Some(MonographSection.directionsForAdministration x)
        | HasOutputClass "nationalFunding" _ -> Some(MonographSection.nationalFunding x)
        | HasOutputClass "interactions" _ -> Some(MonographSection.interactionStatements x)
        | _ -> None

    type CMPI with
      static member parse (x:drugProvider.Topic) =
        let name = CMPIName(x.Title)
        let sections =
          x.Topics |> Array.map MonographSection.section |> Array.choose id
        {id = Id(x.Id); cmpiname = name; sections = sections}

    type DrugClass with
      static member parse (x:drugProvider.Topic) =
        let name = DrugClassName(x.Title)
        let sections =
          x.Topics |> Array.map MonographSection.section |> Array.choose id
        {id = Id(x.Id); dcname = name; sections = sections}

    type Drug with
      static member parse (x:drugProvider.Topic) =
        let name = DrugName(x.Title)

        let interactionLinks = match x.Body with
                               | Some(b) -> b.Ps |> Array.choose (hasOutputclasso "interactionsLinks")
                                                 |> Array.collect (fun p -> p.Xrefs |> Array.map InteractionLink.from)
                               | None -> [||]

        let constituentDrugs = match x.Body with
                               | Some(b) -> b.Ps |> Array.choose (hasOutputclasso "constituentDrugs")
                                                 |> Array.collect (fun p -> p.Xrefs |> Array.map ConstituentDrug.from)
                               | None -> [||]
        let classifications = match x.Body with
                              | Some(b) -> b.Datas
                                           |> Array.choose (hasName "classifications")
                                           |> Array.map (fun cs -> Classification.fromlist cs)
                              | None -> [||]

        let vtmid = x.Body >>= (fun b ->  b.Datas |> Array.tryPick (Some >=> withname "vtmid" >=> Vtmid.from))

        let syn (x:drugProvider.P) =
          match x with
           | HasOutputClasso "synonyms" p -> p.Value >>= (Synonyms >> Some)
           | _ -> None

        let synonyms = x.Body >>= (fun b -> b.Ps |> Array.tryPick syn)

        let sections =
          x.Topics |> Array.map MonographSection.section |> Array.choose id

        let primaryDomainOfEffect = x.Body >>= PrimaryDomainOfEffect.from
        let secondaryDomainsOfEffect = x.Body >>= SecondaryDomainsOfEffect.from

        {id = Id(x.Id); name = name; interactionLinks = interactionLinks; constituentDrugs = constituentDrugs; classifications = classifications; vtmid = vtmid; sections = sections; primaryDomainOfEffect = primaryDomainOfEffect; secondaryDomainsOfEffect = secondaryDomainsOfEffect; synonyms = synonyms}

module Publication =
  open Shared
  open prelude

  type WoundManagmentId = | WoundManagmentId of Id

  let name (n:string) (x:drugProvider.Data) = 
    if x.Name = n then Some x
    else None

  type Publication =
    | Publication of System.DateTime * WoundManagmentId
    static member parse (x:drugProvider.Topic) =
      let value (d:drugProvider.Data) = d.Number >>= (int32 >> Some)
      let number n ds  = ds |> Array.tryPick (fun x -> (name n x) >>= value)
      let d = match x.Body with
               | Some b -> match b.Datas with
                           | [|d|] ->
                            let day = d.Datas |> number "publicationDay"
                            let month = d.Datas |> number "publicationMonth"
                            let year = d.Datas |> number "publicationYear"
                            match (day,month,year) with
                             | Some d, Some m, Some y ->
                               System.DateTime(y,m,d)
                             | _ -> failwith "Missing d/m/y"
                           | _ -> failwith "No Datas"
               | None -> failwith "Missing Body"
      let wmid (xref:drugProvider.Xref) = xref.Href |> Id |> WoundManagmentId |> Some
      let id = x.Xrefs |> Array.filter (fun xref -> xref.Rel = Some "woundManagement") |> Array.pick wmid
      Publication(d,id)
