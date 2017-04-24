namespace FSharp.ReportDSL
open System
open FSharp.ReportDSL
open FSharp.ReportDSL.Helpers
open FSharp.ReportDSL.Core

type VTableRowInfo<'t> =  {
    Content  : RangeProxy<'t>
    Label    : RangeProxy<'t> 
}

module VTableRowInfo = 
    let contramap f (rowInfo :  VTableRowInfo<'t>) ={
        Content = RangeProxy.contramap f rowInfo.Content
        Label  =  RangeProxy.contramap f rowInfo.Label
    }
    let fromMapper (label : string , mapper : 't -> CellContent) : VTableRowInfo<'t>  = {
        Content = RangeProxy.cell mapper
        Label = RangeProxy.constStr label
    }

    let group label (rows : VTableRowInfo<'t> []) : VTableRowInfo<'t> = {
        Content = RangeProxy.stack Vertical (rows |> Array.map (fun x -> x.Content) )
        Label   = RangeProxy.stack Horizontal [| RangeProxy.constStr label; RangeProxy.stack Vertical (rows |> Array.map (fun x -> x.Label));|]
    }

    let fromMappers (mappers :  (string * ('t -> CellContent)) seq) : VTableRowInfo<'t> [] =
        mappers |> Seq.map ( fun (label,mapper) -> fromMapper(label,mapper)) |> Seq.toArray

    let groupMappers label (mappers :  (string * ('t -> CellContent)) seq) : VTableRowInfo<'t> =
        fromMappers mappers |> group label

type VTableInfo<'t> =  {
     ShowLabelsAsColumn  : bool 
     RowInfos : VTableRowInfo<'t> [] 
     Header   :  RangeProxy<'t>
}

type VTableView<'t> = {
    Header :  RangeProxy<'t>
    Data :  RangeProxy<'t> 
}


module VTableView = 

    let liftDependency (f : 'a -> VTableView<'t>) : VTableView<'a * 't> = {
        Header = RangeProxy.liftDependency (fun a -> (f a).Header)
        Data = RangeProxy.liftDependency (fun a -> (f a).Data)
    }
        
    let contramap f (view :  VTableView<'t>) = {
         Header = RangeProxy.contramap f view.Header
         Data =  RangeProxy.contramap f view.Data
    }

    let combine (views :  VTableView<'t> []) : VTableView<'t> = 
        let newHeader = views |> Seq.map(fun x -> x.Header) |> RangeProxy.stack Horizontal 
        let newData = views |> Seq.map(fun x -> x.Data) |> RangeProxy.stack Horizontal 
        { Header = newHeader; Data = newData }


    let fromSingle (view :  VTableView<'t>) : RangeProxy<'t> = 
        RangeProxy.stack Vertical [ view.Header; view.Data ]

    let fromSeq (view :  VTableView<'t>) : RangeProxy<'t []> = 
        RangeProxy.stack Vertical [ RangeProxy.fromFirst view.Header; RangeProxy.fromSeq Vertical view.Data ]

module VTableInfo = 

    let contramap f (vtable :  VTableInfo<'t>) = {
         ShowLabelsAsColumn = vtable.ShowLabelsAsColumn
         RowInfos =  Array.map (VTableRowInfo.contramap f) vtable.RowInfos
         Header = RangeProxy.contramap f vtable.Header
    }

    /// try generalize with fromSeq
    let fromSingle (vtable: VTableInfo<'t>) : VTableView<'t> = 
       let rowsContent = 
            vtable.RowInfos
            |> Array.map(fun x -> x.Content)
            |> RangeProxy.stack Vertical

       let rowLablesContent : RangeProxy<'t> = 
            vtable.RowInfos 
            |> Array.map(fun x -> x.Label)
            |> RangeProxy.stack Vertical

       if vtable.ShowLabelsAsColumn
       then 
            {   
                Header = RangeProxy.stack Horizontal [RangeProxy.syncWidth rowLablesContent (CellContent.FromString "Наименование параметра") ; vtable.Header]
                Data =  RangeProxy.stack Horizontal [rowLablesContent ; rowsContent]
            }
       else 
            {   
                Header = RangeProxy.stack Vertical [vtable.Header; rowLablesContent]
                Data = rowsContent
            }

    let fromSeq (vtable : VTableInfo<'t>) : VTableView<'t []> =
        let rowLablesContent : RangeProxy<'t> = 
            vtable.RowInfos 
            |> Array.map(fun x -> x.Label)
            |> RangeProxy.stack Vertical

        let rowsContent = 
            vtable.RowInfos
            |> Array.map(fun x -> x.Content)
            |> RangeProxy.stack Vertical

        if vtable.ShowLabelsAsColumn
        then 
            {   
                Header = (RangeProxy.stack Horizontal [RangeProxy.syncWidth (RangeProxy.fromFirst rowLablesContent) (CellContent.FromString "Наименование параметра") ; RangeProxy.fromSeq Horizontal vtable.Header])
                Data =  (RangeProxy.stack Horizontal [ RangeProxy.fromFirst rowLablesContent ; RangeProxy.fromSeq Horizontal rowsContent])
            }
        else 
            {   
                Header = (RangeProxy.fromSeq Horizontal (RangeProxy.stack Vertical [vtable.Header; rowLablesContent]))
                Data = (RangeProxy.fromSeq Horizontal rowsContent)
            }


