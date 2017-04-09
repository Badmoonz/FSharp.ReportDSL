namespace FSharp.ReportDSL.Converters  
open System  
open FSharp.ReportDSL  
open FSharp.ReportDSL.Core 

type StringCell = 
    | EmptyCell
    | StringCell of string array

    static member hjoin sc1 sc2 : StringCell = 
        match sc1,sc2 with
        | (StringCell ss1) ,(StringCell ss2) ->
            if (ss1.Length <> ss2.Length)
            then raise (Exception(sprintf "StringCell.hjoin failed %d <> %d" ss1.Length ss2.Length))
            else Array.map2 (fun s1 s2 ->  s1 + "|" + s2 ) ss1 ss2 |> StringCell
        | EmptyCell , _ -> sc2
        | _ , EmptyCell -> sc1
    static member vjoin  sc1 sc2 : StringCell = 
        match sc1,sc2 with
        | (StringCell ss1) ,(StringCell ss2) ->
            let untilLast, lastStr =  Array.splitAt (ss1.Length - 1) ss1
            let updatedLast = lastStr |> Array.map(fun s -> s.Replace(" ", "_"))
            Array.concat([|untilLast ; updatedLast; ss2|]) |> StringCell
        | EmptyCell , _ -> sc2
        | _ , EmptyCell -> sc1
    static member create' (content : string) ({Width = w; Height = h} : ContentSize) = 
        if w = 0 || h = 0
        then 
            EmptyCell
        else
            let h = h * 3
            let midH = h / 2
            let defaultCellWidth = 16
            let content = if content.Length > defaultCellWidth then content.Substring(0, defaultCellWidth) else content
            let contentLength = content.Length
            let cellWidth = defaultCellWidth * w + (w - 1)
            let beforeContent =  (cellWidth - contentLength) / 2
            let afterContent = cellWidth - beforeContent -  contentLength
            let rows = seq {
                for _ in [|0..midH-1|] do
                    yield System.String(' ', cellWidth)
                yield (System.String(' ', beforeContent) + content +  System.String(' ',  afterContent))
                for _ in [|midH+1..h-1|] do
                    yield System.String(' ', cellWidth) 
            }
            rows
            |>Seq.toArray
            |>StringCell 
    static member create (content : string) ({Width = actualWidth; Height = actualHeight} : ContentSize) ({Width = allowedWidth; Height = allowedHeight} : ContentSize) = 
            StringCell.hjoin 
                (StringCell.vjoin (StringCell.create' content {Width = actualWidth; Height = actualHeight} ) (StringCell.create' String.Empty {Width = allowedWidth - actualWidth; Height = actualHeight} ))
                (StringCell.vjoin (StringCell.create' String.Empty {Width = actualWidth; Height = allowedHeight - actualHeight} ) (StringCell.create' String.Empty {Width = allowedWidth - actualWidth; Height =  allowedHeight - actualHeight}))



    static member toText = function
        |(StringCell strings) ->
             strings
             |> Seq.map( fun s -> "|" + s + "|")
             |> String.concat System.Environment.NewLine
        | EmptyCell -> String.Empty
        
type StringCellConverter() =
    interface IStackViewAggregator<Nothing,StringCell> with 
        member __.Convert(x) = 
            let pos , size, content = x
            let contentStr = 
                    match content with
                    | Empty    -> String.Empty
                    | String s -> s
                    | Int x -> String.Format("{0}", x)
                    | Float (x, f) -> String.Format("{0:f3}", x)
            StringCell.create' contentStr size
        member __.VCombine (l,r) = StringCell.vjoin l r
        member __.HCombine (l,r) = StringCell.hjoin l r


type StringCellConverterAbsoluteCellPosition() =
    interface IStackViewAggregator<AbsoluteCellPosition,StringCell> with 
        member __.Convert(x) = 
            let pos , size, content = x
            StringCell.create' (sprintf "(%d,%d)" pos.X pos.Y) size
        member __.VCombine (l,r) = StringCell.vjoin l r
        member __.HCombine (l,r) = StringCell.hjoin l r

type StringCellConverterTablePosition() =
    interface IStackViewAggregator<TablePosition,StringCell> with 
        member __.Convert(x) = 
            let pos , size, content = x
            StringCell.create' (sprintf "(%d;%d) (%d;%d)" pos.Row pos.Column size.Height size.Width) size
        member __.VCombine (l,r) = StringCell.vjoin l r
        member __.HCombine (l,r) = StringCell.hjoin l r