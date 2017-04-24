module FSharp.ReportDSL.Tests

open FSharp.ReportDSL
open FSharp.ReportDSL.Helpers

open FSharp.ReportDSL.Core
open FSharp.ReportDSL.Converters
open FSharp.ReportDSL.Examples.RegimExample
open FSharp.ReportDSL.Examples
open NUnit.Framework
open System



let abs = AbsoluteCellPositionMetaDataProvider()
let table = TablePositionMetaDataProvider()



[<Test>]
let ``RangeProxy.updateMinSize cell`` () = 
    let rangeProxy : RangeProxy<int> = RangeProxy.cell (fun x -> Int (Nullable x))

    let minContentSize = const' {Width = 2; Height = 3}
    let updatedRange = RangeProxy.updateMinSize minContentSize rangeProxy
    let rangeContent =RangeProxy.minimalContentSize updatedRange 1
    Assert.AreEqual(minContentSize 1, rangeContent)


    let minContentSize2 = const' {Width = 1; Height = 2}

    let updatedRange2 = RangeProxy.updateMinSize minContentSize2 updatedRange

    let rangeContent2 =RangeProxy.minimalContentSize updatedRange 1

    Assert.AreEqual(minContentSize 1, rangeContent2, sprintf "%A <> %A" (minContentSize 1) rangeContent2)



[<Test>]
let ``RangeProxy.updateMinSize stack`` () = 
    let rangeProxy : RangeProxy<int> = 
        RangeProxy.stack StackType.Horizontal [|
            RangeProxy.cell (fun x -> Int (Nullable x))
            RangeProxy.cell (fun x -> Int (Nullable x))
        |] 

    let minContentSize = const' {Width = 4; Height = 3}
    let updatedRange = RangeProxy.updateMinSize minContentSize rangeProxy
    let rangeContent = RangeProxy.minimalContentSize updatedRange 1
    Assert.AreEqual(minContentSize 1, rangeContent, sprintf "%A <> %A" (minContentSize 1) rangeContent)

[<Test>]
let ``test regimeComplianceTemplate string`` () = 
    RangeProxyConveter.convert table RegimeComplianceExample.regimeComplianceTemplate RegimeComplianceExample.regimeCompliance
    |> StackView.aggregate (StringCellConverter())
    |> StringCell.toText
    |> printfn "%s"



[<Test>]
let ``test regimeReportProxy string`` () = 
    RangeProxyConveter.convert table RegimExample.regimeReportProxy RegimExample.regimeInfo
    |> StackView.aggregate (StringCellConverter())
    |> StringCell.toText
    |> printfn "%s"



[<Test>]
let ``test manyRegimesInfoGrid string`` () = 
    RangeProxyConveter.convert table RegimExampleVtable.manyRegimesInfoGrid [| RegimExample.regimeInfo2;RegimExample.regimeInfo |]
    |> StackView.aggregate (StringCellConverter())
    |> StringCell.toText
    |> printfn "%s"



//[<Test>]
//let ``hello returns 42`` () =
//  let result = Library.hello 42
//  printfn "%i" result
//  Assert.AreEqual(42,result)
