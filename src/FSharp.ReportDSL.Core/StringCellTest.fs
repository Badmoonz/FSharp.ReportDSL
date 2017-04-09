namespace FSharp.ReportDSL
module StrigCellTEst = 
    open FSharp.ReportDSL
    open FSharp.ReportDSL.Converters
    open Helpers

    let s1 = StringCell.create "123" {Width = 1; Height = 3}
    let s2 = StringCell.create "123" {Width = 1; Height = 3}


    let createCells x = Seq.unfold (fun i ->  if i <= x then Some (StringCell.create (string i) {Width = 1; Height = 3}, i + 1) else None) 1
    
//    
//    let createGrid w h = 
//        createCells (w*h)
//        |> Seq.mapi (fun i x -> i, x) 
//        |> Seq.groupBy (fun (i,x) -> i / w)
//        |> Seq.map(fun (_, items) -> items |> Seq.map snd |> fold1 StringCell.hjoin)
//        |> fold1 StringCell.vjoin
//    
//
//    let test() = 
//        createGrid 3 5
//        |> StringCell.toText
//        |> printfn "%s"
//    

