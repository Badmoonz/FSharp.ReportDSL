
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
                ("т / день", fun s -> CellContent.FromDouble s.PerDay)
                ("тыс т. / мес.", fun s -> CellContent.FromDouble s.PerMonth)
                (@"млн. т. / год", fun s -> CellContent.FromDouble s.PerYear)
            |]
         Header  =  RangeProxy.cell (fun x -> CellContent.FromString x.Name)
    }
    let npsInfoVTable containsVfd : VTableInfo<NpsInfo> = {
         ShowLabelsAsColumn = true
         RowInfos = 
            Seq.toArray <| seq {
                yield! VTableRowInfo.fromMappers  [|
                    (@"Кол-во агрегатов", fun s ->  CellContent.FromInt (Some s.ActivePumps.Length))
                    (@"Агрегаты",   fun s -> CellContent.FromString (s.ActivePumps |> Seq.map(fun x -> x.Name) |> String.concat ";")) |] 
                if containsVfd then
                    yield VTableRowInfo.fromMapper(@"Частота" , fun s -> CellContent.FromDouble s.PIn)
                yield VTableRowInfo.groupMappers "P" [|
                    (@"Pвх",  fun s -> CellContent.FromDouble s.PIn)
                    (@"Pкол",  fun s -> CellContent.FromDouble s.PCol)
                    (@"Pвых",  fun s -> CellContent.FromDouble s.POut)
                    (@"Pзащ",  fun s -> CellContent.FromDouble s.PDef)|]  
            }
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


    let regimeNameTableView : TableView<RegimeInfo> = {
         Data = RangeProxy.cell (fun x -> CellContent.FromString x.RegimeName)
         Header =  RangeProxy.constCell (CellContent.FromString "название режима" )
    }

    let containsVfd (npsInfos : NpsInfo seq) = npsInfos |> Seq.filter(fun x -> x.AvgShaftSpeed.IsSome) |> Seq.isEmpty |> not

    let flowInfoView : TableView<RegimeInfo> = flowInfoVTable |> VTableInfo.fromSeq |> TableView.contramap (fun info -> info.FlowInfos)
    let npsInfoView  : TableView<RegimeInfo> = npsInfoVTable >> VTableInfo.fromSeq |> TableView.liftDependency |> TableView.contramap (fun info -> containsVfd info.NpsInfos, info.NpsInfos)
    let oilInfoView  : TableView<RegimeInfo> = oilInfoVTable  |> VTableInfo.fromSingle |> TableView.contramap (fun info -> info.OilInfo)

    let singleRegimeInfoGrid = TableView.combine [| regimeNameTableView; oilInfoView; flowInfoView ; npsInfoView  |] |> TableView.fromSingle
    let manyRegimesInfoGrid = 
        RangeProxy.stack Vertical [|
            RangeProxy.constCell (CellContent.FromString "AZZZXZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ")
            RangeProxy.empty
            RangeProxy.empty
            TableView.combine [| regimeNameTableView;  oilInfoView; flowInfoView  ; npsInfoView |] |> TableView.fromSeq

        |]




