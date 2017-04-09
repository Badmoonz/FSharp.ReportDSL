namespace FSharp.ReportDSL.Converters  
open System  
open FSharp.ReportDSL  
open FSharp.ReportDSL.Core 
open System.Web.UI
open System.Web.UI.HtmlControls
module HtmlConverter = 
    let private initTableRows (contentSize : ContentSize) = 
        let t = new HtmlTable();
        t.Border <- 1
        for i in [|0..contentSize.Height+1|] do
            let row = new HtmlTableRow();
            t.Rows.Add row
        t
        
    let private contentStr = function
        | Empty    -> " "
        | String s -> if String.IsNullOrEmpty s then " " else s
        | Int x    -> String.Format("{0}", x)
        | Float (x, f) -> String.Format("{0:f3}", x)    


    let private removeCell (t : HtmlTable)  (pos : TablePosition) = 
        (t.Rows.Item pos.Row).Cells.RemoveAt pos.Column

    let private generateCell (size : ContentSize) (content : CellContent ) = 
        let cell = new HtmlTableCell()
        cell.Align <- "Center"
        cell.VAlign <- "Center"
//        let cell = ((t.Rows.Item pos.Row).Cells.Item pos.Column)
        cell.InnerText <- contentStr content
        if size.Width > 1
        then cell.ColSpan <- size.Width
        if size.Height > 1
        then cell.RowSpan <- size.Height
        cell
//        for i in [|size.Width..1|] do
//            for j in [|size.Height..1|] do
//                removeCell t {X = pos.X + i; Y = pos.Y + j}
        
    let rec private generateCells = function 
        | CellView (_, size,_) when size.Height = 0 || size.Width = 0 -> Seq.empty
        | CellView (pos, size,content) ->  Seq.singleton ( pos, generateCell  size content )
        | StackView (_, subviews)      -> subviews |> Seq.collect generateCells                               

    let createTable (view : StackView<TablePosition>) =
        let contentSize = StackView.contentSize view
        let t = initTableRows contentSize
        let groupedByRows = 
            generateCells view 
            |> Seq.groupBy (fun (pos, _) -> pos.Row)
            |> Seq.sortBy fst
            |> Seq.toArray
        groupedByRows
        |> Seq.iter(fun (rowId, items) -> 
            let row = t.Rows.Item rowId
            items 
            |> Seq.sortBy(fun (pos,_) -> pos.Column)
            |> Seq.map snd
            |> Seq.iter row.Cells.Add
            )
        t
          
    let toText ( t : HtmlTable) = 
        use sw = new System.IO.StringWriter()
        t.RenderControl(new HtmlTextWriter(sw));
        sw.ToString();

    let toFile (filePath : string) (t : HtmlTable) = 
        use sw = new System.IO.StreamWriter(filePath)//
        use writer = new HtmlTextWriter(sw)
        writer.RenderBeginTag("!DOCTYPE html");
        writer.RenderBeginTag(HtmlTextWriterTag.Html);
            writer.RenderBeginTag(HtmlTextWriterTag.Head);
                writer.RenderBeginTag("meta charset=\"UTF-8\"");
                writer.RenderEndTag();
            writer.RenderEndTag();
            writer.RenderBeginTag(HtmlTextWriterTag.Body);
            t.RenderControl(writer);
            writer.RenderEndTag();
        writer.RenderEndTag();

