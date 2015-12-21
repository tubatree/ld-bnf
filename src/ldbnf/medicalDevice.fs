namespace Bnf
open FSharp.Data
open Shared

module MedicalDevice =
  type Title = | Title of string

  type PrescribingAndDispensingInformation = | PrescribingAndDispensingInformation of drugProvider.Sectiondiv

  type MedicalDevice =
    | MedicalDevice of Title * PrescribingAndDispensingInformation option * Id list


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
      MedicalDevice(title,padi,ids)
