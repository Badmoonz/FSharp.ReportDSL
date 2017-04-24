module FSharp.ReportDSL.Tests

open FSharp.ReportDSL
open FSharp.ReportDSL.Converters
open FSharp.ReportDSL.Examples.RegimExample
open FSharp.ReportDSL.Examples
open NUnit.Framework




let abs = AbsoluteCellPositionMetaDataProvider()
let table = TablePositionMetaDataProvider()

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
