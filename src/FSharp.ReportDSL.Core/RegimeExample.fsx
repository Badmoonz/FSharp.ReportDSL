#I @"..\..\packages\build"

#r @"ClosedXML\lib\net40\ClosedXML.dll"
#r @"DocumentFormat.OpenXml\lib\DocumentFormat.OpenXml.dll"

#load "Helpers.fs"
#load "RangeProxy.fs"
#load "VTable.fs"
#load "RangeProxyConverter.fs"
#load "MetaDataProviders.fs"
#load "StringCellConverter.fs"
#load "HtmlConverter.fs"

#load "RegimeExample.fs"
#load "RegimeExampleVTable.fs"

#load "ExcelConverter.fs"


open FSharp.ReportDSL
open FSharp.ReportDSL.Converters
open FSharp.ReportDSL.Examples.RegimExample
open FSharp.ReportDSL.Examples.RegimExampleVtable


//
//StringCellConveter.convert singleRegimeInfoGrid regimeInfo
//|> StringCell.toText
//|> printfn "%s"


let abs = AbsoluteCellPositionMetaDataProvider()
let table = TablePositionMetaDataProvider()



RangeProxyConveter.convert table  manyRegimesInfoGrid [|regimeInfo; regimeInfo2|]
|> ExcelConverter.createTable (System.IO.Path.Combine(__SOURCE_DIRECTORY__, "HelloWorld.xlsx"))

RangeProxyConveter.convert table  manyRegimesInfoGrid [|regimeInfo; regimeInfo2|]
|> HtmlConverter.createTable
|> HtmlConverter.toFile @"C:\Users\Badmoonz\Source\Repos\FSharp.ReportDSL\src\FSharp.ReportDSL.Core\test.html"



RangeProxyConveter.convert abs manyRegimesInfoGrid [|regimeInfo; regimeInfo2|]
|> StackView.aggregate (StringCellConverterAbsoluteCellPosition())
|> StringCell.toText
|> printfn "%s"


printfn ""
printfn ""

RangeProxyConveter.convert table manyRegimesInfoGrid [|regimeInfo; regimeInfo2|]
|> StackView.aggregate (StringCellConverterTablePosition())
|> StringCell.toText
|> printfn "%s"


//
//RangeProxyConveter.convert {X =0; Y = 0}  regimeReportProxy regimeInfo
//|> StackView.aggregate stringCellConverter
//|> StringCell.toText
//|> printfn "%s"