namespace Bnf
open FSharp.RDF
open FSharp.Data.Runtime

module RdfUris =
  open prelude
  open resource
  open Bnf.Drug
  open Bnf.MedicinalForm
  open Bnf.TreatmentSummary
  open Bnf.BorderlineSubstance
  open Bnf.Interaction
  open Bnf.MedicalDeviceType
  open Bnf.WoundManagement
  open Bnf.BorderlineSubstanceTaxonomy
  open Bnf.Publication
  open Bnf.ClinicalMedicalDeviceInformationGroup
  open Assertion
  open rdf
  open Shared
  open Bnf.PublicationInfo

  let optionlist = new OptionlistBuilder<Predicate * Object>()

  let fettle (s:string) = s.Trim().ToLower()

  let label = fettle >> xsd.string >> (dataProperty !!"rdfs:label")
  let title = xsd.xmlliteral >> (dataProperty !!"nicebnf:hasTitle")

  let inline xtitle x =
    let element = ( ^a : (member XElement : System.Xml.Linq.XElement) x)
    [element |> (string >> title)
     element.Value |> label]

  let inline dita x =
    let element = ( ^a : (member XElement : System.Xml.Linq.XElement) x)
    [element |> (string >> xsd.xmlliteral >> (dataProperty !!"nicebnf:hasDitaContent"))
     element |> (stringify >> xsd.string >> (dataProperty !! "nicebnf:hasTextContent"))]


  type Uri with
    static member nicebnf = "http://ld.nice.org.uk/ns/bnf#"
    static member nicebnfClass = "http://ld.nice.org.uk/ns/bnf/"
    static member bnfsite = "http://bnf.nice.org.uk/"

    static member totopic (id:Id) = !!(sprintf "%s%s" Uri.bnfsite (string id))
    static member totopic (href:Href) = !!(sprintf "%s%s" Uri.bnfsite (string href))
    static member fromhref rel (id:Id) = !!(sprintf "%s%s/%s" Uri.bnfsite (rel |> splitCamelCase) (string id))

    static member fromobj o s = !!(sprintf "%s%s/%s" Uri.bnfsite (typename o |> splitCamelCase) s)
    static member fromtype<'a> s = !!(sprintf "%s%s/%s" Uri.bnfsite (typeof<'a>.Name |> splitCamelCase) s)
    static member inline fromcontent o x =
      let element = ( ^a : (member XElement : System.Xml.Linq.XElement) x)
      let ident = element.Value.Replace(" ","-").ToLower()
      Uri.fromobj o ident

    static member has o = !!("nicebnf:has" + typename o)
    static member has<'a>() = !!("nicebnf:has" + typeof<'a>.Name)

    // BNF website entity identifiers
    static member from (x:Drug) = !!(Uri.bnfsite + "drug/" + string x.id )
    static member from (ConstituentDrug (l)) = !!(Uri.bnfsite + "drug/" + (l.Href.ToId() |> string))
    static member fromsec (x:Drug) (Id i) = !!(Uri.bnfsite + "drug/" + string x.id + "#" + i)
    static member from (x:DrugClass) = !!(Uri.bnfsite + "drug-class/" + string x.id )
    static member fromdc (x:string) = !!(Uri.bnfsite  + (string x).Replace(".xml","") )
    static member fromsecdc (x:DrugClass) (Id i) = !!(Uri.bnfsite + "drug-class/" + string x.id + "#" + i)
    static member from (x:CMPI) = !!(Uri.bnfsite + "clinical-medicinal-product-information/" + string x.id )
    static member fromcmpi (ClinicalMedicinalProductInformation(i)) = !!(Uri.bnfsite + "clinical-medicinal-product-information/" + string i)
    static member fromseccmpi (x:CMPI) (Id i) = !!(Uri.bnfsite + "clinical-medicinal-product-information/" + string x.id + "#" + i)
    static member from (x:BorderlineSubstance) = !!(Uri.bnfsite + "borderline-substance/" + string x.id )
    static member frombsc (x:string) = !!(Uri.bnfsite + "borderline-substance/" + (string x).Replace(".xml","") )
    static member frombsc (x:Id) = !!(Uri.bnfsite + "borderline-substance/" + (string x) )
    static member frombst (x:string) = !!(Uri.bnfsite + (string x).Replace(".xml","") )
    static member frombst (x:Id) = !!(Uri.bnfsite + "borderline-substance-taxonomy/" + string x)
    static member from (x:MedicinalForm) = !!(Uri.bnfsite + "medicinal-form/" + string x.id )
    static member from (x:MedicinalProduct) = !!(Uri.bnfsite + "medicinal-product/" + string x.ampid)
    static member from (x:WoundManagement) = !!(Uri.bnfsite + "wound-management/" + string x.id )
    static member from (x:WoundManagementLink) = !!(Uri.bnfsite + "wound-management/" + string x.id )
    static member fromwm (WoundManagmentId(id)) = !!(Uri.bnfsite + "wound-management/" + string id )
    static member from (x:Product) = !!(Uri.bnfsite + "wound-management-product/" + string x.ampid)
    static member from (x:TreatmentSummary) =
      match x with
      | TreatmentSummary (i,About(_)) -> !!(Uri.bnfsite + "about/" + string i)
      | TreatmentSummary (i,AboutIndex(_)) -> !!(Uri.bnfsite + "about/" + string i)
      | TreatmentSummary (i,GuidanceIndex(_)) -> !!(Uri.bnfsite + "guidance/" + string i)
      | TreatmentSummary (i,Guidance(_)) -> !!(Uri.bnfsite + "guidance/" + string i)
      | TreatmentSummary (i,_) -> !!(Uri.bnfsite + "treatment-summary/" + string i)
    static member fromotherindex t x = !!(Uri.bnfsite + t+"/" + string x)
    static member frommd (x:Id) = !!(Uri.bnfsite + "medical-device/" + string x)
    static member from (x:MedicalDeviceType) = !!(Uri.bnfsite + "medical-device-type/" + string x.id)
    static member frommdt (x:Id) = !!(Uri.bnfsite + "medical-device-type/" + string x)

    static member fromcmdig_id (x:ClinicalMedicalDeviceInformationGroup) id = !!(Uri.bnfsite + "clinical-medical-device-information-group/" + string x.id + "#" + string id)
    static member fromcmdig (x:ClinicalMedicalDeviceInformationGroup) = !!(Uri.bnfsite + "clinical-medical-device-information-group/" + string x.id)
    static member fromcmdig (x:string) = !!(Uri.bnfsite + (string x).Replace(".xml",""))

    static member from (InteractionLink (l)) = !!(Uri.bnfsite + "interaction/" + (l.Href.ToId() |> string ))
    static member fromil id = !!(Uri.bnfsite + "interaction/" + string id)
    static member fromiwl (iw:InteractsWith) = !!(Uri.bnfsite + "interaction/" + (iw.interactswith.href.ToId() |> string))
    static member fromiw id (iw:InteractsWith) = !!(Uri.bnfsite + "interaction/" +  string id + "#" + string iw.id)

    // Ontology Taxonomy individuals
    static member from (Route s) = !!(Uri.nicebnfClass + "Route#" + (NameUtils.niceCamelName s))
    static member fromr (s:string) = !!(Uri.nicebnfClass + "Route#" + (NameUtils.niceCamelName s))
    static member from (Indication s) = !!(Uri.nicebnfClass + "Indication#" + (NameUtils.niceCamelName s))
    static member fromi (s:string) = !!(Uri.nicebnfClass + "Indication#" + (NameUtils.niceCamelName s))
    static member from (TheraputicIndication (s,_)) = !!(Uri.nicebnfClass + "Indication#" + (NameUtils.niceCamelName s))
    static member fromc (Classification (Id s,_,_)) = !!(Uri.nicebnfClass + "Classification#" + s)
    static member froms (s:string) = !!(Uri.nicebnfClass + "Specificity#" + (NameUtils.niceCamelName s))
    static member fromfi (FundingIdentifier s) = !!("http://" + (string s.Href))
    static member fromgrp (s:string) = !!(Uri.nicebnfClass + "PatientGroup#" + (NameUtils.niceCamelName s))
    static member from (TheraputicUse (s,_)) = !!(Uri.nicebnfClass + "TheraputicUse#" + (NameUtils.niceCamelName s))
    static member from (DomainOfEffect (s,_,_)) = !!(Uri.nicebnfClass + "DomainOfEffect#" + (NameUtils.niceCamelName (s.Value.Trim())))
    static member fromse (SideEffect s) = !!(Uri.nicebnfClass + "SideEffect#" + (NameUtils.niceCamelName s.String.Value))
    static member fromcau (Caution s) = !!(Uri.nicebnfClass + "Caution#" + (NameUtils.niceCamelName s.String.Value))
    static member fromcon (Contraindication s) = !!(Uri.nicebnfClass + "Contraindication#" + (NameUtils.niceCamelName s.String.Value))
    static member fromfre (f:Frequency) = !!(Uri.nicebnfClass + "Frequency#" + (NameUtils.niceCamelName f.frequencyid))
    static member frombs (s:string) = !!(Uri.nicebnfClass + "BodySystem#" + (NameUtils.niceCamelName s))

    // Ontology Entity Classes
    static member TypeEntity o = !!(Uri.nicebnf + (typename o))
    static member TypeEntity<'a>() = !!(Uri.nicebnf + typeof<'a>.Name)

    static member DrugEntity = !!(Uri.nicebnf + "Drug")
    static member ConstituentDrugEntity = !!(Uri.nicebnf + "ConstituentDrug")
    static member DrugClassEntity = !!(Uri.nicebnf + "DrugClass")
    static member CMPIEntity = !!(Uri.nicebnf + "ClinicalMedicinalProductInformation")
    static member MedicinalFormEntity = !!(Uri.nicebnf + "MedicinalForm")
    static member MedicinalProductEntity = !!(Uri.nicebnf + "MedicinalProduct")
    static member TreatmentSummaryEntity = !!(Uri.nicebnf + "TreatmentSummary")
    static member BorderlineSubstanceEntity = !!(Uri.nicebnf + "BorderlineSubstance")
    static member BorderlineSubstanceTaxonomyEntity = !!(Uri.nicebnf + "BorderlineSubstanceTaxonomy")
    static member MedicalDeviceTypeEntity = !!(Uri.nicebnf + "MedicalDeviceType")
    static member MedicalDeviceEntity = !!(Uri.nicebnf + "MedicalDevice")
    static member ClinicalMedicalDeviceInformationGroupEntity = !!(Uri.nicebnf + "ClinicalMedicalDeviceInformationGroup")
    static member WoundManagementEntity = !!(Uri.nicebnf + "WoundManagement")
    static member InteractionEntity = !!(Uri.nicebnf + "InteractionMessage")
    static member InteractionListEntity = !!(Uri.nicebnf + "InteractionList")
    static member DosageEntity = !!(Uri.nicebnf + "Dosage")

    // Ontology Taxonomy Classes

    static member TypeEntityClass o = !!(Uri.nicebnfClass + (typename o))
    static member TypeEntityClass<'a>() = !!(Uri.nicebnfClass + typeof<'a>.Name)

    static member ClassificationEntity = !!(Uri.nicebnfClass + "Classification")
    static member RouteEntity = !!(Uri.nicebnfClass + "Route")
    static member DomainOfEffectEntity = !!(Uri.nicebnfClass + "DomainOfEffect")
    static member TheraputicUseEntity = !!(Uri.nicebnfClass + "TheraputicUse")
    static member IndicationEntity = !!(Uri.nicebnfClass + "Indication")
    static member FundingIdentifierEntity = !!(Uri.nicebnfClass + "FundingIdentifier")
    static member PatientGroupEntity = !!(Uri.nicebnfClass + "PatientGroup")
    static member SpecificityEntity = !!(Uri.nicebnfClass + "Specificity")
    static member SideEffectEntity = !!(Uri.nicebnfClass + "SideEffect")
    static member ContraindicationEntity = !!(Uri.nicebnfClass + "Contraindication")
    static member CautionEntity = !!(Uri.nicebnfClass + "Caution")
    static member BodySystemEntity = !!(Uri.nicebnfClass + "BodySystem")

module Rdf =
  open prelude
  open resource
  open Bnf.Drug
  open Bnf.MedicinalForm
  open Assertion
  open rdf
  open Shared
  open RdfUris

  type Graph with
    static member ReallyEmpty xp =
      let vds = new VDS.RDF.Graph()
      xp |> List.iter (fun (p, (Uri.Sys ns)) -> vds.NamespaceMap.AddNamespace(p, ns))
      Graph vds

  let empty () = Graph.ReallyEmpty ["nicebnf",!!Uri.nicebnf
                                    "rdfs",!!"http://www.w3.org/2000/01/rdf-schema#"
                                    "bnfsite",!!Uri.bnfsite]
