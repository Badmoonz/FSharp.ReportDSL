
namespace FSharp.ReportDSL.Examples

open FSharp.ReportDSL
open FSharp.ReportDSL.Core

module RegimExample =

    open Helpers

    type OilInfo = { Ro : double option ; Nu : double option }
    type PumpInfo = { Name : string; }
    type FlowInfo  = { Name : string;  PerDay : double option ; PerYear : double option ; PerMonth : double option}
    type NpsInfo  = { Name : string;  ActivePumps : PumpInfo []; PIn : double option ; POut : double option ; PCol : double option; PDef   : double option; }
    type RegimeInfo = {RegimeName : string ; OilInfo : OilInfo; NpsInfos : NpsInfo []; FlowInfos : FlowInfo []  } 




    let flowInfoProxy : RangeProxy<FlowInfo> = 
        RangeProxy.stackMappers Vertical [|
            fun s ->  CellContent.FromString s.Name
            fun s ->  CellContent.FromDouble s.PerDay
            fun s ->  CellContent.FromDouble s.PerMonth
            fun s ->  CellContent.FromDouble s.PerYear |] 



    let npsInfoProxy : RangeProxy<NpsInfo> =              
        RangeProxy.stackMappers Vertical [|
            fun s ->  CellContent.FromString s.Name
            fun s ->  CellContent.FromDouble s.PIn
            fun s ->  CellContent.FromDouble s.PCol
            fun s ->  CellContent.FromDouble s.POut
            fun s ->  CellContent.FromDouble s.PIn
            fun s ->  CellContent.FromDouble s.PCol
            fun s ->  CellContent.FromDouble s.POut  |] 
             
    let flowInfoSeqProxy : RangeProxy<RegimeInfo> = 
        RangeProxy.fromSeq Horizontal flowInfoProxy 
        |> RangeProxy.contramap (fun x -> x.FlowInfos)
    let npsInfoSeqProxy =
         RangeProxy.fromSeq Horizontal npsInfoProxy 
         |> RangeProxy.contramap (fun x -> x.NpsInfos)
    
    let oilInfoProxy : RangeProxy<RegimeInfo> =
        RangeProxy.stackMappers Vertical [|
            fun s ->  CellContent.FromDouble s.Ro
            fun s ->  CellContent.FromDouble s.Nu|] 
        |> RangeProxy.contramap (fun x -> x.OilInfo)

    let regimeNameProxy : RangeProxy<RegimeInfo> = 
        RangeProxy.cell (fun x -> CellContent.FromString x.RegimeName)

    let regimeInfoProxy  =
        RangeProxy.Stack(Horizontal, fun main -> 
        [|
            regimeNameProxy
            oilInfoProxy   
            flowInfoSeqProxy
            npsInfoSeqProxy 
        |])


    let regimeReportProxy : RangeProxy<RegimeInfo> = 
        RangeProxy.Stack(Vertical, fun main -> 
        [|
            RangeProxy.constCell (CellContent.FromString "AZZ")
            regimeInfoProxy
//            regimeInfoProxy

        |])


    let regimeInfo  = {
        RegimeName = "RRR"
        OilInfo = {Ro = Some 988.; Nu = Some 8.9 }
        NpsInfos  =
            [|
              { Name = "NPS1" ; PIn = Some 1.1 ; POut = Some 1.2 ; PCol = Some 1.3; PDef = None; ActivePumps = [|{ Name = "NA1"}; {Name = "NA2"}|]}
              { Name = "NPS2" ; PIn = Some 2.1 ; POut = Some 2.2 ; PCol = Some 2.3; PDef = None; ActivePumps = [|{ Name = "NA1"}; {Name = "NA2"}|]}
              { Name = "NPS3" ; PIn = Some 3.1 ; POut = Some 3.2 ; PCol = Some 3.3; PDef = None; ActivePumps = [|{ Name = "NA1"}; {Name = "NA2"}|]}
            |] 
        FlowInfos = 
            [|
                { Name = "RP1";  PerDay = Some 0.1  ; PerYear= Some 0.2  ; PerMonth= Some 0.3 }
                { Name = "RP2";  PerDay = Some 1.1  ; PerYear= Some 1.2  ; PerMonth= Some 3.3 }
//                { Name = "RP3";  PerDay = Some 2.1  ; PerYear= Some 2.2  ; PerMonth= Some 3.3 }

            |]
    }


    let regimeInfo2  = {
        RegimeName = "RRR2"
        OilInfo = {Ro = Some 100.; Nu = Some 15. }
        NpsInfos  =
            [|
              { Name = "NPS1" ; PIn = Some 10.1 ; POut = Some 10.2 ; PCol = Some 10.3; PDef = None; ActivePumps = [|{ Name = "NA1"}; {Name = "NA2"}|]}
              { Name = "NPS2" ; PIn = Some 20.1 ; POut = Some 20.2 ; PCol = Some 20.3; PDef = None; ActivePumps = [|{ Name = "NA1"}; {Name = "NA2"}|]}
              { Name = "NPS3" ; PIn = Some 30.1 ; POut = Some 30.2 ; PCol = Some 30.3; PDef = None; ActivePumps = [|{ Name = "NA1"}; {Name = "NA2"}|]}
            |] 
        FlowInfos = 
            [|
                { Name = "RP1";  PerDay = Some 00.1  ; PerYear= Some 00.2  ; PerMonth= Some 00.3 }
                { Name = "RP2";  PerDay = Some 10.1  ; PerYear= Some 10.2  ; PerMonth= Some 30.3 }
//                { Name = "RP3";  PerDay = Some 20.1  ; PerYear= Some 20.2  ; PerMonth= Some 30.3 }
            |]
    }



