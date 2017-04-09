open System
open System.IO
open System.Web.UI
open System.Web.UI.HtmlControls

let getTable () =
    let t = new HtmlTable();
    t.Border <- 1
    for i in [0..10] do
        let row = new HtmlTableRow();
        row.Attributes.Add("border-bottom", "solid red 1px");
        t.Rows.Add row
        for j in [0..10] do    
            let cell = new HtmlTableCell()
            cell.InnerText <- sprintf "(%d,%d)" i j
            cell.Align <- "Center"
            cell.VAlign <- "Center"
            if i = j && i % 3 = 0
            then
                cell.ColSpan <- 3 
                cell.RowSpan <- 3 
            row.Cells.Insert(j, cell)
    t


let print ( t : HtmlTable) = 
    use sw = new StringWriter()
    t.RenderControl(new HtmlTextWriter(sw));
    sw.ToString();

getTable() |> print |> printfn "%s"
