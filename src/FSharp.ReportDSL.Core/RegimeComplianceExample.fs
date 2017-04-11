namespace FSharp.ReportDSL.Examples
open System
open FSharp.ReportDSL
open FSharp.ReportDSL.Helpers
open FSharp.ReportDSL.Core
open FSharp.ReportDSL.Examples.RegimExample

module RegimeComplianceExample =
    open RegimExample
    type Compliance<'t> = { Plan : 't option; Fact: 't option }

    module Compliance = 
        let map f {Plan = plan; Fact = fact } = {Plan = Option.map f plan; Fact = Option.map f fact }

    type OilInfoCompliance = { Ro : double Compliance ; Nu : double Compliance }
    type FlowInfoCompliance  = { Name : string;  Q : double Compliance}
    type NpsInfoCompliance  = { Name : string;  ActivePumps : (PumpInfo []) Compliance; PIn : double Compliance ; POut : double Compliance ; PCol : double Compliance; AvgShaftSpeed :  double Compliance}
    type RegimeInfoCompliance = { RegimeName : string; Period : DateTime * DateTime;  OilInfo : OilInfoCompliance; NpsInfos : NpsInfoCompliance []; FlowInfos : FlowInfoCompliance [];  } 



    let intComplianceVtable label : VTableRowInfo<Compliance<int>> =
         VTableRowInfo.groupMappers label [|
            (@"Факт",  fun s -> CellContent.FromInt s.Fact)
            (@"План",  fun s -> CellContent.FromInt s.Plan)|]  

    let doubleComplianceVtable label : VTableRowInfo<Compliance<double>> =
         VTableRowInfo.groupMappers label [|
            (@"Факт",  fun s -> CellContent.FromDouble s.Fact)
            (@"План",  fun s -> CellContent.FromDouble s.Plan)|]  

    let oilInfoComplianceVtable: VTableInfo<OilInfoCompliance> =  {
         ShowLabelsAsColumn = true
         RowInfos =
            [|
               doubleComplianceVtable "т/м3" |> VTableRowInfo.contramap (fun x -> x.Ro)
               doubleComplianceVtable "сСт" |> VTableRowInfo.contramap (fun x -> x.Nu)

            |]
         Header  =  RangeProxy.constStr "параметры нефти"
    }

    let flowInfoComplianceVtable : VTableInfo<FlowInfoCompliance> =  {
         ShowLabelsAsColumn = true
         RowInfos =
            [|
               doubleComplianceVtable "расход" |> VTableRowInfo.contramap (fun x -> x.Q)
            |]
         Header  =  RangeProxy.cell (fun x -> CellContent.FromString x.Name)
    }

    let npsInfoComplianceVtable hasVfd : VTableInfo<NpsInfoCompliance> =  {
         ShowLabelsAsColumn = true
         RowInfos =
            Array.ofSeq <| seq {
                yield intComplianceVtable "Кол-во аггрегатов" |> VTableRowInfo.contramap (fun x -> x.ActivePumps |> Compliance.map Seq.length)
                if hasVfd then yield  doubleComplianceVtable "частота" |> VTableRowInfo.contramap (fun x -> x.AvgShaftSpeed)
                yield doubleComplianceVtable "Рвх" |> VTableRowInfo.contramap (fun x -> x.PIn)
                yield doubleComplianceVtable "Рвх" |> VTableRowInfo.contramap (fun x -> x.PIn)
                yield doubleComplianceVtable "Ркол" |> VTableRowInfo.contramap (fun x -> x.PCol)
                yield doubleComplianceVtable "Рвых" |> VTableRowInfo.contramap (fun x -> x.POut)
            }   
         Header  =  RangeProxy.cell (fun x -> CellContent.FromString x.Name)
    }

    let containsVfd (npsInfos : NpsInfoCompliance seq) = npsInfos |> Seq.filter(fun x -> x.AvgShaftSpeed.Plan.IsSome || x.AvgShaftSpeed.Fact.IsSome) |> Seq.isEmpty |> not

    let flowInfoView : VTableView<RegimeInfoCompliance> = flowInfoComplianceVtable |> VTableInfo.fromSeq |> VTableView.contramap (fun info -> info.FlowInfos)
    let npsInfoView  : VTableView<RegimeInfoCompliance> = npsInfoComplianceVtable >> VTableInfo.fromSeq |> VTableView.liftDependency |> VTableView.contramap (fun info -> containsVfd info.NpsInfos, info.NpsInfos)
    let oilInfoView  : VTableView<RegimeInfoCompliance> = oilInfoComplianceVtable  |> VTableInfo.fromSingle |> VTableView.contramap (fun info -> info.OilInfo)

    let singleRegimeInfoGrid = VTableView.combine [| oilInfoView; flowInfoView ; npsInfoView  |] |> VTableView.fromSingle
    let regimeComplianceTemplate = 
        RangeProxy.stack Vertical [|
            RangeProxy.cell (fun x -> CellContent.FromString (sprintf " Аппробаия режима %s на `%A` -`%A`" x.RegimeName (fst x.Period) (snd x.Period) ))
            RangeProxy.empty
            RangeProxy.empty
            VTableView.combine [| oilInfoView; flowInfoView  ; npsInfoView |] |> VTableView.fromSingle

        |]


          
    let regimeCompliance : RegimeInfoCompliance =
     {
         RegimeName  = "RRR"
         Period = (DateTime.Now.AddHours(-5.), DateTime.Now)
         OilInfo = {Ro = { Fact = Some 800.; Plan = Some 900.}; Nu = { Fact = Some 13.; Plan = Some 12.}}
         NpsInfos = [| 
            { Name = "Nps1";  ActivePumps = {Fact = None; Plan = None;}; PIn = {Fact = None; Plan = None} ; POut  = {Fact = None; Plan = None} ; PCol  = {Fact = None; Plan = None}; AvgShaftSpeed = {Fact = Some 1.; Plan = Some 2.}}
            { Name = "Nps2";  ActivePumps = {Fact = None; Plan = None;}; PIn = {Fact = None; Plan = None} ; POut  = {Fact = None; Plan = None} ; PCol  = {Fact = None; Plan = None}; AvgShaftSpeed = {Fact = Some 1.; Plan = Some 2.}}
         |]
         FlowInfos = [|
             { Name = "Rp1";  Q =  {Fact = None; Plan = None;}}
             { Name = "Rp2";  Q  = {Fact = None; Plan = None;}}
             { Name = "Rp3";  Q  = {Fact = None; Plan = None;}}
         |]
     }