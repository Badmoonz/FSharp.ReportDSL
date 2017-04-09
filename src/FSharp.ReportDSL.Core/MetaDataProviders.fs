namespace FSharp.ReportDSL.Converters  
open FSharp.ReportDSL.Core


type Nothing = Nothing
type NoMetaDataProvider() = 
    interface  IMetaDataProvider<Nothing> with
        member __.UpdateMetaData(Nothing, stackType, contentsize) = Nothing
        member __.Default = Nothing

type AbsoluteCellPosition = {X : int; Y : int}

type AbsoluteCellPositionMetaDataProvider() = 
    interface  IMetaDataProvider<AbsoluteCellPosition> with
        member __.UpdateMetaData(position, stackType, contentsize) : AbsoluteCellPosition = 
            match stackType with
            | Horizontal -> {position with X = position.X + contentsize.Width}
            | Vertical -> {position with Y = position.Y + contentsize.Height}
        member __.Default = {X = 0; Y = 0}

type RelativeCellPosition = {Column : int; Row: int}

type RelativeCellPositionMetaDataProvider() = 
    interface  IMetaDataProvider<RelativeCellPosition> with
        member __.UpdateMetaData(position, stackType, contentsize) : RelativeCellPosition = 
            match stackType with
            | Horizontal -> {position with Column = position.Column + 1}
            | Vertical -> {position with Row = position.Row + 1}
        member __.Default = {Row = 0; Column = 0}

type TablePosition = {Column : int; Row: int;}
type TablePositionMetaDataProvider() = 
    interface  IMetaDataProvider<TablePosition> with
        member __.UpdateMetaData(position, stackType, contentsize) : TablePosition = 
            match stackType with
            | Horizontal -> {position with Column = position.Column +  contentsize.Width}
            | Vertical -> {position with Row = position.Row +  contentsize.Height}
        member __.Default = {Row = 0; Column = 0;}