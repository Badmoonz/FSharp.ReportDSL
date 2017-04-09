namespace FSharp.ReportDSL.Converters  
open System  
open FSharp.ReportDSL  
open FSharp.ReportDSL.Core 
open System.Web.UI
open System.Web.UI.HtmlControls
module HtmlConverter = 
    let private initTable (contentSize : ContentSize) = 
        let t = new HtmlTable();
        t.Border <- 1
        for i in [|0..contentSize.Height-1|] do
            let row = new HtmlTableRow();
            t.Rows.Add row
            for j in [|0..contentSize.Width-1|] do    
                let cell = new HtmlTableCell()
                cell.Align <- "Center"
                cell.VAlign <- "Center"
                row.Cells.Add(cell)
        t
        
    let private contentStr = function
        | Empty    -> String.Empty
        | String s -> s
        | Int x    -> String.Format("{0}", x)
        | Float (x, f) -> String.Format("{0:f3}", x)    


    let private removeCell (t : HtmlTable)  (pos : CellPosition) = 
        (t.Rows.Item pos.Y).Cells.RemoveAt pos.X

    let private insertContent (t : HtmlTable) (pos : CellPosition) (size : ContentSize)(content : CellContent ) = 
        let cell = ((t.Rows.Item pos.Y).Cells.Item pos.X)
        cell.InnerText <- contentStr content
        cell.ColSpan <- size.Width
        cell.RowSpan <- size.Height
//        for i in [|size.Width..1|] do
//            for j in [|size.Height..1|] do
//                removeCell t {X = pos.X + i; Y = pos.Y + j}
        
    let rec private insertView (t : HtmlTable) = function 
        | CellView (pos, size,content) -> insertContent t pos size content
        | StackView (_, subviews)      -> subviews |> Seq.iter (insertView t)

    let createTable (view : StackView) =
        let contentSize = StackView.contentSize view
        let t = initTable contentSize
        insertView t view
        t
          
    let toText ( t : HtmlTable) = 
        use sw = new System.IO.StringWriter()
        t.RenderControl(new HtmlTextWriter(sw));
        sw.ToString();

    let toFile (filePath : string) (t : HtmlTable) = 
        use sw = new System.IO.StreamWriter(filePath)//
        t.RenderControl(new HtmlTextWriter(sw));