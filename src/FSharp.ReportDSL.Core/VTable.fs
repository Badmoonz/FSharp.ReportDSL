namespace FSharp.ReportDSL
open System
open FSharp.ReportDSL
open FSharp.ReportDSL.Helpers
open FSharp.ReportDSL.Core

type ColumnInfo<'t> =  {
    Content  : RangeProxy<'t>
    Label    : RangeProxy<'t> 
}


type VTableInfo<'t> =  {
     ShowLabelsAsColumn  : bool 
     RowInfos : ColumnInfo<'t> [] 
     Header   :  RangeProxy<'t>
}

type TableInfo<'t> =  {
     ColumnInfos : ColumnInfo<'t> [] 
     Header   :  RangeProxy<'t>
}

type TableView<'t> = {
    Header :  RangeProxy<'t>
    Data :  RangeProxy<'t> 
}

module ColumnInfo = 
    let contramap f (columnInfo :  ColumnInfo<'t>) ={
        Content = RangeProxy.contramap f columnInfo.Content
        Label  =  RangeProxy.contramap f columnInfo.Label
    }
    let fromMapper (label : string , mapper : 't -> CellContent) : ColumnInfo<'t>  = 
        let content = RangeProxy.cell mapper
        {
            Content = content
            Label = RangeProxy.constStr label
        }

    let group label (columns : ColumnInfo<'t> []) : ColumnInfo<'t> = {
        Content = RangeProxy.stack Vertical (columns |> Array.map (fun x -> x.Content) )
        Label   = RangeProxy.stack Horizontal [| RangeProxy.constStr label; RangeProxy.stack Vertical (columns |> Array.map (fun x -> x.Label));|]
    }

    let fromMappers (mappers :  (string * ('t -> CellContent)) seq) : ColumnInfo<'t> [] =
        mappers |> Seq.map ( fun (label,mapper) -> fromMapper(label,mapper)) |> Seq.toArray

    let groupMappers label (mappers :  (string * ('t -> CellContent)) seq) : ColumnInfo<'t> =
        fromMappers mappers |> group label

module TableView = 

    let liftDependency (f : 'a -> TableView<'t>) : TableView<'a * 't> = {
        Header = RangeProxy.liftDependency (fun a -> (f a).Header)
        Data = RangeProxy.liftDependency (fun a -> (f a).Data)
    }
        
    let contramap f (view :  TableView<'t>) = {
         Header = RangeProxy.contramap f view.Header
         Data =  RangeProxy.contramap f view.Data
    }

    let combine (views :  TableView<'t> []) : TableView<'t> = 
        let newHeader = views |> Seq.map(fun x -> x.Header) |> RangeProxy.stack Horizontal 
        let newData = views |> Seq.map(fun x -> x.Data) |> RangeProxy.stack Horizontal 
        { Header = newHeader; Data = newData }

    
    let fromSeq (view :  TableView<'t>) : TableView<'t []> =  {
        Header = RangeProxy.fromFirst view.Header
        Data = RangeProxy.fromSeq Vertical view.Data
    }

    let toRangeProxy (view :  TableView<'t>) : RangeProxy<'t> = 
        RangeProxy.stack Vertical [ view.Header; view.Data ]


module TableInfo = 
     let contramap f (table :  TableInfo<'t>) : TableInfo<'u> = {
            ColumnInfos =  Array.map (ColumnInfo.contramap f) table.ColumnInfos
            Header = RangeProxy.contramap f table.Header
        }

     let toTableView (table : TableInfo<'t>) : TableView<'t []> =

        let boundedColumns = 
            table.ColumnInfos
            |> Seq.map (fun x -> RangeProxy.syncWidth [x.Label; x.Content] |> Seq.toArray)
            |> Seq.cache

        let columnLablesContent : RangeProxy<'t> = 
            boundedColumns
            |> Seq.map (Array.item 0)
            |> RangeProxy.stack Horizontal

        let columnsContent = 
            boundedColumns
            |> Seq.map (Array.item 1)
            |> RangeProxy.stack Horizontal
        {   
            Header = (RangeProxy.fromFirst (RangeProxy.stack Vertical [table.Header; columnLablesContent]))
            Data = (RangeProxy.fromSeq Vertical columnsContent)
        }

module VTableInfo = 
    let contramap f (vtable :  VTableInfo<'t>) = {
         ShowLabelsAsColumn = vtable.ShowLabelsAsColumn
         RowInfos =  Array.map (ColumnInfo.contramap f) vtable.RowInfos
         Header = RangeProxy.contramap f vtable.Header
    }
      
    let toTableView (vtable : VTableInfo<'t>) : TableView<'t []> =

        let sync = if vtable.ShowLabelsAsColumn then RangeProxy.syncHeight else RangeProxy.syncWidth
        let boundedColumns = 
            vtable.RowInfos
            |> Seq.map (fun x -> sync [x.Label; x.Content] |> Seq.toArray)
            |> Seq.cache


        let rowLablesContent : RangeProxy<'t> = 
            boundedColumns
            |> Seq.map (Array.item 0)
            |> RangeProxy.stack Vertical

        let rowsContent = 
            boundedColumns
            |> Seq.map (Array.item 1)
            |> RangeProxy.stack Vertical

        if vtable.ShowLabelsAsColumn
        then 
            {   
                Header = (RangeProxy.stack Horizontal [ RangeProxy.constStr "Наименование параметра" ; RangeProxy.fromSeq Horizontal vtable.Header])
                Data =  (RangeProxy.stack Horizontal [ RangeProxy.fromFirst rowLablesContent ; RangeProxy.fromSeq Horizontal rowsContent])
            }
        else 
            {   
                Header = (RangeProxy.fromSeq Horizontal (RangeProxy.stack Vertical [vtable.Header; rowLablesContent]))
                Data = (RangeProxy.fromSeq Horizontal rowsContent)
            }


