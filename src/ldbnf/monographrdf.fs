namespace Bnf
open FSharp.RDF
open FSharp.Data.Runtime

module DrugRdf =
  open prelude
  open resource
  open Bnf.Drug
  open Bnf.MedicinalForm
  open Assertion
  open rdf
  open Shared
  open Rdf
  open RdfUris
  open guid
  open Bnf.Order
  open System.IO
  open System
  open System.Xml.Linq
  open System.Xml.XPath
  open FSharp.Collections.ParallelSeq

  //shoudl replace these with tostring
  let getlabeld (DrugName n) = n.Value |? ""
  let getvald (DrugName n) = string n
  let getlabeldc (DrugClassName n) = n.Value |? ""
  let getvaldc (DrugClassName n) = string n
  let getlabelcmpi (CMPIName n) = n.Value |? ""
  let getvalcmpi (CMPIName n) = string n
  let tosys (Sys s) = s

  let subject x l = dataProperty !!"nicebnf:hasSubject" ((toString x)^^xsd.string) :: l
  let subtype x l = (a !!("nicebnf:" + (toString x))) :: l

  let ph (ph:drugProvider.Ph) =
     optionlist {
       yield ph.XElement.Value |> label
       yield! ph |> dita
       }

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
  type Graph with
    static member setupGraph = Graph.ReallyEmpty ["nicebnf",!!Uri.nicebnf
                                                  "rdfs",!!"http://www.w3.org/2000/01/rdf-schema#"
                                                  "bnfsite",!!Uri.bnfsite]

    static member from (x:CMPI) =
      let s = [ a Uri.CMPIEntity
                getlabelcmpi x.cmpiname |> label
                getvalcmpi x.cmpiname |> title]
      let dr r = resource (Uri.from x) r
      let sec = Graph.fromsec (Uri.fromseccmpi x)
      [dr s
       dr (x.sections |> Seq.map sec |> Seq.collect id |> Seq.toList)]
       |> addOrder (Uri.from x) |> Assert.graph Graph.setupGraph

    static member from (x:DrugClass) =
      let s = [a Uri.DrugClassEntity
               getlabeldc x.dcname |> label
               getvaldc x.dcname |> title]

      let dr r = resource (Uri.from x) r
      let sec = Graph.fromsec (Uri.fromsecdc x)

      [dr s
       dr (x.sections |> Seq.map sec |> Seq.collect id |> Seq.toList)]
       |> addOrder (Uri.from x) |> Assert.graph Graph.setupGraph

    static member from (x:Drug) =
      let getvtmid (Vtmid i) = Some(string i)

      let s = optionlist {
                 yield a Uri.DrugEntity
                 yield getlabeld x.name |> label
                 yield getvald x.name |> title
                 yield x.vtmid >>= getvtmid >>= (xsd.string >> dataProperty !!"nicebnf:hasVtmid" >> Some)
                 yield x.primaryDomainOfEffect >>= (Graph.frompdoe >> Some)
                 yield x.synonyms >>= (Graph.fromsyn >> Some)
                }
      let count = ref 0
      let mfl = function
                 | MedicinalForms (_,_,_,x) -> let mf = x |> Seq.map (fun x -> Graph.frommfl (x, count)) |> Seq.toList
                                               match mf with
                                               | [] -> [blank !!"nicebnf:hasMedicinalForm" []]
                                               | _ -> mf
                 | _ -> []

      let mfls = (x.sections |> Seq.collect mfl |> Seq.toList)

      let dr r = resource (Uri.from x) r
      //pass in uri construction for sections
      let sec = Graph.fromsec (Uri.fromsec x)
      let sdoe = match x.secondaryDomainsOfEffect with
                 | Some d -> Graph.fromsdoes d
                 | None -> []

      [dr s
       dr sdoe
       dr (x.classifications |> Seq.map (fun c-> c |> List.collect(Graph.fromcl (Uri.from x))) |> Seq.toList |> List.collect id)
       dr (x.constituentDrugs |> Seq.map Graph.fromcd |> Seq.toList)
       dr (x.interactionLinks |> Seq.map Graph.fromil |> Seq.toList)
       dr (x.sections |> Seq.map sec |> Seq.collect id |> Seq.toList)
       dr mfls]
        |> addOrder (Uri.from x) |> Assert.graph Graph.setupGraph

    static member fromsyn (Synonyms s) = dataProperty !!"nicebnf:hasSynonyms" (s^^xsd.string)

    static member fromdc (InheritsFromClass (c)) =
      one !!"nicebnf:inheritsFromClass" (Uri.fromdc c) [a Uri.DrugClassEntity]

    //the label for this is in another part of the feed so will be created elsewhere
    static member fromcl drugurl (Classification(id,ifcs,typ)) =
      let parseClassfications (id) =
        let classificationType = match typ with
                                 | Primary -> "primary"
                                 | Secondary -> "secondary"
                                 | Tertiary -> "tertiary"
        let result = classifications.XPathSelectElements(".//data[@name='classifications']//data[@name='drugClassification' and text()='"+id+"']")  |> Seq.toList
        result
      let searchForClassification = if (parseClassfications(id.ToString()).Length > 1) then "true" else ""
      
      let pname = function
                      | Primary -> !!"nicebnf:hasPrimaryClassification"
                      | Secondary -> !!"nicebnf:hasSecondaryClassification"
                      | Tertiary -> !!"nicebnf:hasTertiaryClassification"

      let hasDrugsInClassification = function
                      | Primary -> dataProperty !!"nicebnf:hasDrugsInPrimaryClassification" (searchForClassification^^xsd.string)
                      | Secondary -> dataProperty !!"nicebnf:hasDrugsInSecondaryClassification" (searchForClassification^^xsd.string)
                      | Tertiary -> dataProperty !!"nicebnf:hasDrugsInTertiaryClassification" (searchForClassification^^xsd.string)


      let ifs = ifcs |> Seq.map Graph.fromdc |> Seq.toList
      let cl = one (pname typ) (Classification(id,ifcs,typ) |> Uri.fromc)
                   [(a Uri.ClassificationEntity)]
      let c = one !!"nicebnf:hasClassification" (Classification(id,ifcs,typ) |> Uri.fromc)
                   [(a Uri.ClassificationEntity)
                    objectProperty !!"nicebnf:isClassificationOf" drugurl]

      (if (searchForClassification = "true") 
       then
        (hasDrugsInClassification typ) :: c :: cl :: ifs
      else
       c :: cl :: ifs)

    static member fromil (i:InteractionLink) =
      one !!"nicebnf:hasInteractionList" (Uri.from i) [a Uri.InteractionListEntity]

    static member fromcd (x:ConstituentDrug) =
      one !!"nicebnf:hasConstituentDrug" (Uri.from x )
       [a Uri.ConstituentDrugEntity
        x |> (string >> label)]

    static member fromtu ((x:TheraputicUse), ?name0:string) =
      let name = defaultArg name0 "nicebnf:hasTherapeuticUse"
      let s = match x with | TheraputicUse(n,u) ->
                              optionlist {
                               yield a Uri.TheraputicUseEntity
                               yield n |> label
                               yield u >>= (Graph.fromtu >> Some)}
      one !!name (Uri.from x) s

    static member fromptu (PrimaryTheraputicUse t) =
      t >>= fun x -> Graph.fromtu (x,"nicebnf:hasPrimaryTherapeuticUse") |> Some

    static member fromstu (SecondaryTheraputicUses t) =
      t >>= fun x -> Graph.fromtu (x,"nicebnf:hasSecondaryTherapeuticUses") |> Some

    static member fromdoe (DomainOfEffect (n,p,s)) =
      optionlist {
        yield a Uri.DomainOfEffectEntity
        yield n >>= (label >> Some)
        yield p >>= Graph.fromptu
        yield s >>= Graph.fromstu}

    static member hasdoe p (d:DomainOfEffect) =
      one !!("nicebnf:has" + p) (Uri.from d) (Graph.fromdoe d)

    static member frompdoe (PrimaryDomainOfEffect d) =
      d |> (Graph.hasdoe "PrimaryDomainOfEffect")

    static member fromsdoes (SecondaryDomainsOfEffect ds) =
      ds |> Seq.map (Graph.hasdoe "SecondaryDomainOfEffect") |> Seq.toList

    static member from (x:Route) =
      Some(one !!"nicebnf:hasRoute" (Uri.from x)
            [x |> (string >> label)
             a Uri.RouteEntity])

    static member from (x:Indication) =
      Some(one !!"nicebnf:hasIndication" (Uri.from x)
            [x |> (string >> label)
             a Uri.IndicationEntity]  )

    static member from (x:FundingIdentifier) =
      Some(one !!"nicebnf:hasFundingIdentifier" (Uri.fromfi x)
              [x |> (string >> label)
               a Uri.FundingIdentifierEntity])

    static member fromti (Bnf.Drug.Title (Paragraph(s,_))) =
      Some(dataProperty !!"nicebnf:hasTitle" (s^^xsd.string))

    static member fromsp (Specificity (Paragraph(s,p),r,i)) =
      let sp = optionlist {
                yield! p <!> xtitle |> unwrap
                yield a Uri.SpecificityEntity
                yield! r |> List.choose Graph.from
                yield! i |> List.choose Graph.from}
      one !!"nicebnf:hasSpecificity" (Uri.froms s) sp

    //ungroup the patient groups adding a route if available
    static member from (RouteOfAdministration(r,pgs)) =
      let patientGrp pg =
        blank !!"nicebnf:hasDosage"
         (optionlist {
          yield one !!"nicebnf:hasPatientGroup" (Uri.fromgrp pg.Group.Value.Value)
                 (optionlist{
                   yield! pg.Group |> xtitle
                   yield objectProperty !!"rdfs:subClassOf" (Uri.fromgrp(pg.parentGroup))
                   yield a Uri.PatientGroupEntity
                   })

          yield pg.Dosage |> label
          yield! pg.dosageXml |> dita
          yield r >>= (Graph.fromsp >> Some)})
      pgs |> Seq.map patientGrp

    static member from (x:TheraputicIndication) =
      match x with
        | TheraputicIndication (s,p) ->
           Some(one !!"nicebnf:hasIndication" (Uri.from x)
             (optionlist {
              yield a Uri.IndicationEntity
              yield s |> label
              yield! p |> dita
             }))

    static member order (count) =
        [dataProperty !!"nicebnf:hasOrderDisabled" (count.ToString()^^xsd.string)]

    static member therapeuticIndicationOrder (uri, count) =
      count := !count + 1
      blank !!"nicebnf:hasTherapeuticIndicationOrder"
             (optionlist {
              yield dataProperty !!"nicebnf:hasIndication" (uri.ToString()^^xsd.string)
              })

    static member fromidg (IndicationsAndDose(tis,roas,count)) =
      (tis |> Seq.map Graph.from |> Seq.choose id |> Seq.toList)
              @ (roas |> Seq.collect Graph.from |> Seq.toList)

    static member fromidgs (x:IndicationsAndDoseSection) =
      let dp n s = (a !!("nicebnf:" + n)) :: (s |> dita)
      match x with
       | Pharmacokinetics s -> s |> dp "Pharmacokinetics"
       | DoseEquivalence s -> s |> dp "DoseEquivalence"
       | DoseAdjustments s -> s |> dp "DoseAdjustments"
       | ExtremesOfBodyWeight s -> s |> dp "ExtremesOfBodyWeight"
       | Potency s -> s |> dp "Potency" 
 
    static member frompca (p:PatientAndCarerAdvice) =
      let pca t = Graph.fromthree >> (subtype t)
      match p with
        | PatientResources (t,sp,s) -> (t,sp,s) |> pca p
        | AdviceAroundMissedDoses (t,sp,s) -> (t,sp,s) |> pca p
        | GeneralPatientAdvice (t,sp,s) -> (t,sp,s) |> pca p
        | AdviceAroundDrivingAndOtherTasks (t,sp,s) -> (t,sp,s) |> pca p
        | PatientAdviceInPregnancy  (t,sp,s) -> (t,sp,s) |> pca p
        | PatientAdviceInConceptionAndContraception (t,sp,s) -> (t,sp,s) |> pca p

    static member fromlvs (LicensingVariationStatement(p)) = p |> dita

    static member fromavs (AdditionalFormsStatement(p)) = p |> dita

    static member frommfl (MedicinalForm(l), count) =
      one !!"nicebnf:hasMedicinalForm" (!!(Uri.bnfsite + (string l.Href))) [l.Title |> label]

    static member getMedicinalFormsOrder (MedicinalForm(l), count) =
      count := !count + 1
      blank !!"nicebnf:hasMedicinalFormsOrder" (optionlist {
               yield dataProperty !!"nicebnf:hasOrderDisabled" (count.Value.ToString()^^xsd.string)
               yield dataProperty !!"nicebnf:hasMedicinalForm" ((!!(Uri.bnfsite + (string l.Href))).ToString()^^xsd.string)
               })

    static member fromcsc (AllergyAndCrossSensitivityContraindications (t,sp,s)) = Graph.fromthree (t,sp,s)

    static member fromcscs (AllergyAndCrossSensitivityCrossSensitivity (t,sp,s)) = Graph.fromthree (t,sp,s)

    static member from (x:drugProvider.Sectiondiv) = x |> dita

    static member frompair (sp,s:drugProvider.Sectiondiv) =
      optionlist {
        yield sp >>= (Graph.fromsp >> Some)
        yield! Graph.from s}

    static member fromthree (t,sp,s) =
      optionlist {
        yield t >>= Graph.fromti
        yield! Graph.frompair (sp,s)}

    static member fromgi (GeneralInformation (t,sp,s)) = Graph.fromthree (t,sp,s)
    static member fromda (DoseAdjustment (t,sp,s)) = Graph.fromthree (t,sp,s)
    static member fromexc (ExceptionToLegalCategory (t,sp,s)) = Graph.fromthree (t,sp,s)
    static member fromden (DentalPractitionersFormularyInformation (t,sp,s)) = Graph.fromthree (t,sp,s)
    static member fromadp (AdviceForDentalPractitioners (t,sp,s)) = Graph.fromthree (t,sp,s)
    static member fromlsfp (LessSuitableForPrescribing (t,sp,s)) = Graph.fromthree (t,sp,s)
    static member fromhas (HandlingAndStorage (t,sp,s)) = Graph.fromthree (t,sp,s)
    static member fromelt (EffectOnLaboratoryTest (t,sp,s)) = Graph.fromthree (t,sp,s)
    static member frompts (PreTreatmentScreening (t,sp,s)) = Graph.fromthree (t,sp,s)
    static member fromtc (TreatmentCessation (t,sp,s)) = Graph.fromthree (t,sp,s)
    static member fromdac (DrugAction (t,sp,s)) = Graph.fromthree (t,sp,s)
    static member fromsea (SideEffectAdvice (t,sp,s)) = Graph.fromthree (t,sp,s)
    static member fromod (SideEffectsOverdosageInformation (t,sp,s)) = Graph.fromthree (t,sp,s)
    static member fromia (ImportantAdvice (t,sp,s)) = Graph.fromthree (t,sp,s)
    static member fromciri (ContraindicationsRenalImpairment (t,sp,s)) = Graph.fromthree (t,sp,s)
    static member frompadi (PrescribingAndDispensingInformation (t,sp,s)) = Graph.fromthree (t,sp,s)
    static member fromulu (UnlicencedUse (t,sp,s)) = Graph.fromthree (t,sp,s)
    static member fromcac (ConceptionAndContraception (t,sp,s)) = Graph.fromthree (t,sp,s)
    static member fromisi (ImportantSafetyInformation(t,sp,s)) = Graph.fromthree (t,sp,s)
    static member fromdfa (DirectionForAdministration (t,sp,s))= Graph.fromthree (t,sp,s)
    static member frominter (InteractionStatement(t,sp,s)) = Graph.fromthree(t,sp,s)
    static member fromamp (AdditionalMonitoringInPregnancy(t,sp,s)) = Graph.fromthree(t,sp,s)
    static member fromambf (AdditionalMonitoringInBreastFeeding(t,sp,s)) = Graph.fromthree(t,sp,s)
    static member fromamhi (AdditionalMonitoringInHepaticImpairment(t,sp,s)) = Graph.fromthree(t,sp,s)
    static member fromamri (AdditionalMonitoringInRenalImpairment (t,sp,s)) = Graph.fromthree(t,sp,s)

    static member fromse (x:SideEffect) =
      one !!"nicebnf:hasSideEffect" (Uri.fromse x)
       (optionlist {
         yield! x |> function | SideEffect s -> ph s
         yield a Uri.SideEffectEntity })

    static member fromcon (x:Contraindication) =
      one !!"nicebnf:hasContraindication" (Uri.fromcon x)
        (optionlist {
          yield! x |> function | Contraindication c -> ph c
          yield a Uri.ContraindicationEntity })

    static member fromcau (x:Caution) =
      one !!"nicebnf:hasCaution" (Uri.fromcau x)
       (optionlist {
         yield! x|> function | Caution c -> ph c
         yield a Uri.CautionEntity})

    static member fromfre (x:SideEffectsGroup) =
        let gf (f,p,ses) =
          optionlist {
           yield a !!"nicebnf:SideEffectGroup"
           yield one !!"nicebnf:hasFrequency" (Uri.fromfre f)
             [a !!"nicebnf:Frequency"
              f.label |> label]
           yield! p |> dita
           yield! ses |> Seq.map Graph.fromse |> Seq.toList}
        match x with
          | GeneralSideEffects (f,p,ses) -> gf(f,p,ses) |> subtype x
          | SideEffectsWithRoutes (f,sp,p,ses) -> (sp |> Graph.fromsp) :: gf(f,p,ses) |> subtype x
          | SideEffectsWithIndications (f,sp,p,ses) -> (sp |> Graph.fromsp) :: gf(f,p,ses) |> subtype x

    static member fromcog (x:ContraindicationsGroup) =
      let gen (p,cs) =  optionlist {
                         yield a !!"nicebnf:ContraindicationsGroup"
                         yield! p |> dita
                         yield! (cs |> Seq.map Graph.fromcon |> Seq.toList)}
      match x with
        | GeneralContraindications (p,cs) -> (gen(p,cs)) |> subtype x
        | ContraindicationWithRoutes (s,p,cs) -> Graph.fromsp s :: gen(p,cs) |> subtype x
        | ContraindicationWithIndications (s,p,cs) -> Graph.fromsp s :: gen(p,cs) |> subtype x

    static member fromcg (x:CautionsGroup) =
      let gen (p,cs) =  optionlist {
                         yield a !!"nicebnf:CautionsGroup"
                         yield! p |> dita
                         yield! cs |> List.map Graph.fromcau}
      match x with
        | GeneralCautions (p,cs) -> (gen(p,cs)) |> subtype x
        | CautionsWithRoutes (s,p,cs) -> Graph.fromsp s :: gen(p,cs) |> subtype x
        | CautionsWithIndications (s,p,cs) -> Graph.fromsp s :: gen(p,cs) |> subtype x

    static member fromfd (x:FundingDecision) =
      match x with
        | NonNHS(sp,s) -> Graph.frompair (sp,s) |> subtype x
        | SmcDecisions(sp,s) -> Graph.frompair(sp,s) |> subtype x
        | NiceTechnologyAppraisals(fi,t,sp,s) ->
          optionlist {
            yield sp <!> Graph.fromsp
            yield! s <!> Graph.from |> unwrap
            yield t >>= Graph.fromti
            yield fi >>= Graph.from } |> subtype x

    static member frommon (x:MonitoringRequirement) =
      match x with
        | PatientMonitoringProgrammes (t,sp,s) -> Graph.fromthree (t,sp,s) |> subtype x
        | TheraputicDrugMonitoring (t,sp,s) -> Graph.fromthree (t,sp,s) |> subtype x
        | MonitoringOfPatientParameters (t,sp,s) -> Graph.fromthree (t,sp,s) |> subtype x


    static member fromsec sid (x:MonographSection) =

      //call of the functions in xs with the value a into a new list
      let rec applyTo a fs =
        match fs with
         | [] -> []
         | x::fs -> x a::applyTo a fs


      //the next few functions build up a list of functions (partial application)
      //then apply the same value to them all to generate uri's
      //execution of code should be read from right to left
      // e.g. sec "Pregnancy" sid i [statements addps Graph.fromgi gs]
      // gs -> fromgi -> addps -> statements -> sec

      //take the list, add hasSubject and type
      let add' fpredicate f x (sub,urif,id) =
        let s = f x //generate the properties
        let name = typename x
        //add a type and a subject
        let s' = [a !!("nicebnf:" + name)
                  one !!"nicebnf:hasSubject" (urif id)
                    [a !!("nicebnf:" + sub)]]
        one !!(fpredicate name) (webguid() |> Id |> urif) (s @ s')

      let add f x (sub,uri,sid) = add' (fun n -> "nicebnf:has" + n) f x (sub,uri,sid)
      //an alternate version that ignores the type name
      let addps f x (sub,urif,sid) = add' (fun _ -> "nicebnf:hasPrescribingInformationSection") f x (sub,urif,sid)

      let inline sec n i sid statementf =
        let flatlist = statementf |> List.collect id //take list of functions to create statements, collect id flattens list
        flatlist |> applyTo (n,i,sid) //apply the functions with a name and a uri


      let inline statements a g x = x |> Seq.map (a g) |> Seq.toList


      let inline statement a g x =
        match x with
        | Some(x) -> [x |> a g]
        | None -> []

      match x with
        | Pregnancy (i,gs,das,amps) -> sec "Pregnancy" sid i [statements addps Graph.fromgi gs
                                                              statements addps Graph.fromda das
                                                              statements addps Graph.fromamp amps]

        | BreastFeeding (i,gs,ambfs,das) -> sec "BreastFeeding" sid i [statements addps Graph.fromgi gs
                                                                       statements addps Graph.fromambf ambfs
                                                                       statements addps Graph.fromda das]
        | HepaticImpairment (i,gs,das,amhis) -> sec "HepaticImpairment" sid i [statements addps Graph.fromgi gs
                                                                               statements addps Graph.fromda das
                                                                               statements addps Graph.fromamhi amhis]
        | RenalImpairment (i,gs,amri,das) -> sec "RenalImpairment" sid i [statements addps Graph.fromgi gs
                                                                          statements addps Graph.fromamri amri
                                                                          statements addps Graph.fromda das]
        | IndicationsAndDoseGroup (i,g,gss) -> sec "IndicationAndDosageInformation" sid i [statements add Graph.fromidg g
                                                                                           statements addps Graph.fromidgs gss]
        | PatientAndCarerAdvices (i, pcas) -> sec "PatientAndCarerAdvice" sid i [statements addps Graph.frompca pcas]
        | MedicinalForms (i,lvs,avs,_) -> sec "MedicinalFormInformation" sid i [statement addps Graph.fromlvs lvs
                                                                                statement addps Graph.fromavs avs]
        | AllergyAndCrossSensitivity (i,csc,cscs) -> sec "AllergyAndCrossSensitivity" sid i [statements addps Graph.fromcsc csc
                                                                                             statements addps Graph.fromcscs cscs]
        | ExceptionsToLegalCategory (i,es) -> sec "ExceptionsToLegalCategory" sid i [statements addps Graph.fromexc es]
        | ProfessionSpecificInformation (i,dps,adps) -> sec "ProfessionSpecificInformation" sid i [statements addps Graph.fromden dps
                                                                                                   statements addps Graph.fromadp adps]
        | EffectOnLaboratoryTests (i,elts) -> sec "EffectsOnLaboratoryTests" sid i [statements addps Graph.fromelt elts]
        | PreTreatmentScreenings (i,ptss) -> sec "PreTreatmentScreeningInformation" sid i [statements addps Graph.frompts ptss]
        | LessSuitableForPrescribings (i,lsfps) -> sec "LessSuitableForPrescribings" sid i  [statements addps Graph.fromlsfp lsfps]
        | HandlingAndStorages (i,hass) -> sec "HandlingAndStorageInformation" sid i [statements addps Graph.fromhas hass]
        | TreatmentCessations (i,tcs) -> sec "TreatmentCessationInformation" sid i [statements addps Graph.fromtc tcs]
        | DrugActions (i,das) -> sec "DrugActions" sid i [statements addps Graph.fromdac das]
        | SideEffects (i,fres,seas,ods) -> sec "SideEffects" sid i [statements add Graph.fromfre fres
                                                                    statements addps Graph.fromsea seas
                                                                    statements addps Graph.fromod ods]
        | Contraindications (i,cogs,ias,ciri) -> sec "ContraIndications" sid i [statements add Graph.fromcog cogs
                                                                                statements addps Graph.fromia ias
                                                                                statements addps Graph.fromciri ciri]
        | Cautions (i,cgs,ias) -> sec "Cautions" sid i [statements add Graph.fromcg cgs
                                                        statements addps Graph.fromia ias]
        | PrescribingAndDispensingInformations (i,padi) -> sec "PrescribingAndDispensingInformations" sid i [statements addps Graph.frompadi padi]
        | UnlicencedUses (i,ulus) -> sec "UnlicencedUsageInformation" sid i [statements addps Graph.fromulu ulus]
        | ConceptionAndContraceptions (i,cacs) -> sec "ConceptionAndContraception" sid i [statements addps Graph.fromcac cacs]
        | ImportantSafetyInformations (i,isis) -> sec "ImportantSafetyInformations" sid i [statements addps Graph.fromisi isis]
        | DirectionsForAdministration (i,dfas) -> sec "DirectionsForAdministration" sid i [statements addps Graph.fromdfa dfas]
        | NationalFunding (i,fds) -> sec "NationalFunding" sid i [statements add Graph.fromfd fds]
        | InteractionStatements (i,is) -> sec "Interactions" sid i [statements addps Graph.frominter is]
        | MonitoringRequirements (i,mons) -> sec "MonitoringRequirements" sid i [statements addps Graph.frommon mons]

