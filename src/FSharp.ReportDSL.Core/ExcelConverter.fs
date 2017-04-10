namespace FSharp.ReportDSL.Converters  
open System  
open FSharp.ReportDSL  
open FSharp.ReportDSL.Core 
open ClosedXML
open ClosedXML.Excel

module ExcelConverter =


   let rec private generateCells (initPostion : TablePosition) = function 
        | CellView (_, size,_) when size.Height = 0 || size.Width = 0 -> Seq.empty
        | CellView (pos, size,content) ->  Seq.singleton ( {pos with Row = pos.Row + initPostion.Row; Column = pos.Column + initPostion.Column}, size , content )
        | StackView (_, subviews)      -> subviews |> Seq.collect (generateCells initPostion)

   let private setCellValue (cell : IXLCell) =  function
        | String s when not (String.IsNullOrEmpty s) ->   
             cell.Value <- s
             cell.DataType <- XLCellValues.Text
             if s.Length > 10
             then
                cell.Style.Alignment.WrapText <- true
             true
        | Int x ->
             cell.Value <- x
             cell.DataType <- XLCellValues.Number    
             true   
        | Float (x, f) ->
             cell.Value <- x
             cell.DataType <- XLCellValues.Number 
             cell.Style.NumberFormat.Format <- f    
             true
        | _ -> false

   let createTable (path : string) (view : StackView<TablePosition>) =
        let contentSize = StackView.contentSize view
        let workbook = new XLWorkbook();
        let ws = workbook.Worksheets.Add("Sample Sheet");

        view 
        |> generateCells { Row =2 ; Column = 2 }
        |> Seq.iter(fun (pos,size,content) ->
            let cell = ws.Cell(pos.Row,pos.Column)
            let hasContent = setCellValue cell content
            if hasContent then
                cell.Style.Alignment.Horizontal <- XLAlignmentHorizontalValues.Center
                cell.Style.Alignment.Vertical <- XLAlignmentVerticalValues.Center
                if(size.Width > 1 || size.Height > 1)
                then 
                    let range = ws.Range(pos.Row,pos.Column, pos.Row + size.Height - 1, pos.Column + size.Width - 1)
                    range.Merge() |> ignore
                    range.Style.Border.BottomBorder <- XLBorderStyleValues.Medium  
                    range.Style.Border.TopBorder <- XLBorderStyleValues.Medium  
                    range.Style.Border.LeftBorder <- XLBorderStyleValues.Medium  
                    range.Style.Border.RightBorder <- XLBorderStyleValues.Medium  
                else 
                    cell.Style.Border.BottomBorder <- XLBorderStyleValues.Medium  
                    cell.Style.Border.TopBorder <- XLBorderStyleValues.Medium  
                    cell.Style.Border.LeftBorder <- XLBorderStyleValues.Medium  
                    cell.Style.Border.RightBorder <- XLBorderStyleValues.Medium  
        )
        ws.Columns().AdjustToContents() |> ignore
        ws.Column(3).Width <- 13.
        workbook.SaveAs(path);