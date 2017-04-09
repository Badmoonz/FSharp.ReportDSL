#load "Helpers.fs"
#load "RangeProxy.fs"
#load "VTable.fs"
#load "RangeProxyConverter.fs"
#load "StringCellConverter.fs"
#load "HtmlConverter.fs"
#load "RegimeExample.fs"
#load "RegimeExampleVTable.fs"

open FSharp.ReportDSL
open FSharp.ReportDSL.Converters
open FSharp.ReportDSL.Examples.RegimExample
open FSharp.ReportDSL.Examples.RegimExampleVtable


//
//StringCellConveter.convert singleRegimeInfoGrid regimeInfo
//|> StringCell.toText
//|> printfn "%s"

RangeProxyConveter.convert {X = 0 ; Y = 0} manyRegimesInfoGrid [|regimeInfo; regimeInfo2|]
|> HtmlConverter.createTable
|> HtmlConverter.toFile @"C:\Users\Badmoonz\Source\Repos\FSharp.ReportDSL\src\FSharp.ReportDSL.Core\test.html"

let stringCellConverter = StringCellConverter()

let stringCellConverter2 = StringCellConverter2()

RangeProxyConveter.convert {X =0 ; Y = 0} manyRegimesInfoGrid [|regimeInfo; regimeInfo2|]
|> StackView.aggregate stringCellConverter
|> StringCell.toText
|> printfn "%s"


RangeProxyConveter.convert {X =0 ; Y = 0} manyRegimesInfoGrid [|regimeInfo; regimeInfo2|]
|> StackView.aggregate stringCellConverter2
|> StringCell.toText
|> printfn "%s"

RangeProxyConveter.convert {X =0; Y = 0}  regimeReportProxy regimeInfo
|> StackView.aggregate stringCellConverter
|> StringCell.toText
|> printfn "%s"