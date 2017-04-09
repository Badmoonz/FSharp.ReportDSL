
namespace FSharp.ReportDSL.Examples

open FSharp.ReportDSL
open FSharp.ReportDSL.Helpers
open FSharp.ReportDSL.Core
open FSharp.ReportDSL.Examples.RegimExample

module RegimExampleVtable =

   
    let flowInfoVTable : VTableInfo<FlowInfo> = {
         ShowLabelsAsColumn = false
         RowInfos = VTableRowInfo.fromMappers 
            [|
                (@"т / день", fun s -> CellContent.FromDouble s.PerDay)
                (@"тыс т. / мес.", fun s -> CellContent.FromDouble s.PerMonth)
                (@"млн. т. / год", fun s -> CellContent.FromDouble s.PerYear)
            |]
         Header  =  RangeProxy.cell (fun x -> CellContent.FromString x.Name)
    }

    let npsInfoVTable : VTableInfo<NpsInfo> = {
         ShowLabelsAsColumn = true
         RowInfos = VTableRowInfo.fromMappers 
            [|
                (@"Кол-во агрегатов", fun s -> CellContent.FromInt (Some s.ActivePumps.Length))
                (@"Агрегаты", fun s -> CellContent.FromString (s.ActivePumps |> Seq.map(fun x -> x.Name) |> String.concat ";"))
                (@"Pвх", fun s -> CellContent.FromDouble s.PIn)
                (@"Pкол", fun s -> CellContent.FromDouble s.PCol)
                (@"Pвых", fun s -> CellContent.FromDouble s.POut)
                (@"Pзащ", fun s -> CellContent.FromDouble s.PDef)
            |]
         Header  =  RangeProxy.cell (fun x -> CellContent.FromString x.Name)
    }

    let oilInfoVTable : VTableInfo<OilInfo> = {
         ShowLabelsAsColumn = false
         RowInfos = VTableRowInfo.fromMappers 
            [|
                (@"т./ м3", fun s -> CellContent.FromDouble s.Ro)
                (@"сСТ", fun s -> CellContent.FromDouble s.Nu)
            |]
         Header  =  RangeProxy.constCell (CellContent.FromString "параметры нефти")
    }


    let regimeNameTableView : VTableView<RegimeInfo> = {
         Data = RangeProxy.cell (fun x -> CellContent.FromString x.RegimeName)
         Header =  RangeProxy.constCell (CellContent.FromString "название режима" )
    }
    let flowInfoView : VTableView<RegimeInfo> = flowInfoVTable |> VTableInfo.fromSeq |> VTableView.contramap (fun info -> info.FlowInfos)
    let npsInfoView  : VTableView<RegimeInfo> = npsInfoVTable  |> VTableInfo.fromSeq |> VTableView.contramap (fun info -> info.NpsInfos)
    let oilInfoView  : VTableView<RegimeInfo> = oilInfoVTable  |> VTableInfo.fromSingle |> VTableView.contramap (fun info -> info.OilInfo)

    let singleRegimeInfoGrid = VTableView.combine [| regimeNameTableView; oilInfoView; flowInfoView ; npsInfoView  |] |> VTableView.fromSingle
    let manyRegimesInfoGrid = 
        RangeProxy.stack Vertical [|
            RangeProxy.constCell (CellContent.FromString "AZZ")
            RangeProxy.empty
            VTableView.combine [| regimeNameTableView;  oilInfoView; flowInfoView  ; npsInfoView |] |> VTableView.fromSeq

        |]




