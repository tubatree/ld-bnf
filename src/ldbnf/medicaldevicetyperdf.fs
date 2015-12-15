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
