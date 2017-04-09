namespace FSharp.ReportDSL

namespace FSharp.ReportDSL.Converters  
open System  
open FSharp.ReportDSL  
open FSharp.ReportDSL.Helpers
open FSharp.ReportDSL.Core 




type StackView<'m> =
    | CellView of MetaData : 'm * CellSize : ContentSize * CellContent : CellContent 
    | StackView of StackType * StackView<'m> []


type IStackViewAggregator<'m, 't> = 
    interface
        abstract Convert : ('m * ContentSize * CellContent) -> 't
        abstract VCombine : 't * 't -> 't
        abstract HCombine : 't * 't -> 't
    end
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]       
module StackView = 

    let rec aggregate (converter :  IStackViewAggregator<'m, 't>) (view : StackView<'m>) = 
        match view with
        | CellView (pos,size,content) -> converter.Convert(pos,size,content)
        | StackView (Horizontal, views) -> fold1 (fun l r -> converter.HCombine(l,r)) (Seq.map (aggregate converter) views)
        | StackView (Vertical, views) ->  fold1 (fun l r -> converter.VCombine(l,r)) (Seq.map (aggregate converter) views)
    let rec contentSize  = function
        | CellView (_,size,_) -> size
        | StackView (Horizontal, views) -> Seq.map contentSize views |> ContentSize.combineH
        | StackView (Vertical, views)   -> Seq.map contentSize views |> ContentSize.combineV   


type IMetaDataProvider<'m> = 
    interface 
        abstract UpdateMetaData :'m * StackType * ContentSize -> 'm
        abstract Default :'m 
    end


module RangeProxyConveter =
    open Helpers

    let private convertCell (metadataProvider : IMetaDataProvider<'m>) (metaData : 'm) (actualSize : ContentSize) ({Width = allowedWidth; Height = allowedHeight} : ContentSize) (content : CellContent)  = 
        let ({Width = actualWidth; Height = actualHeight} : ContentSize) = actualSize

        let urMetadata = metadataProvider.UpdateMetaData(metaData, Horizontal,  actualSize)
        let llMetadata = metadataProvider.UpdateMetaData(metaData, Vertical,  actualSize)
        let lrMetadata = metadataProvider.UpdateMetaData(llMetadata, Horizontal,  actualSize)
        
        StackView(Vertical, 
            [|
            StackView (Horizontal, 
                [| 
                    CellView (metaData, {Width = actualWidth; Height = actualHeight},content)
                    CellView (urMetadata, {Width = allowedWidth - actualWidth; Height = actualHeight},CellContent.Empty)
                |] )
            StackView (Horizontal,
                [|
                    CellView (llMetadata, {Width = actualWidth; Height = allowedHeight - actualHeight},CellContent.Empty )
                    CellView (lrMetadata, {Width = allowedWidth - actualWidth; Height =  allowedHeight - actualHeight}, CellContent.Empty)
                |])
            |])

    let private joinSizes stackType (a : ContentSize) (b : ContentSize) : ContentSize =
            match stackType with
            | Horizontal -> {Width = a.Width + b.Width; Height = Math.Max(a.Height, b.Height)}
            | Vertical -> {Width = Math.Max(a.Width ,b.Width); Height = a.Height + b.Height}

    let private substractSize stackType (a : ContentSize) (b : ContentSize) : ContentSize =
            match stackType with
            | Horizontal -> { a with Width = a.Width - b.Width}
            | Vertical -> {a with Height = a.Height - b.Height}

    

    let convert (metadataProvider : IMetaDataProvider<'m>) (proxy  : RangeProxy<'t>) (value : 't) : StackView<'m> = 
        let totalSize = RangeProxy.minimalContentSize proxy value
     
        let rec convert' (value : 't) (metaData : 'm) (allowedSize : ContentSize) (proxy  : RangeProxy<'t>) : StackView<'m>  =
            let minimalSize = RangeProxy.minimalContentSize proxy value
            let dynamicWidth, dynamicHeight = RangeProxy.hasDynamicParts proxy value
            match proxy with
            | Cell {ContentMapper = contentMapper;  CellSize = cellSize} -> 

                let actualSize : ContentSize = { 
                    Width = if dynamicWidth then allowedSize.Width else minimalSize.Width
                    Height = if dynamicHeight then allowedSize.Height else minimalSize.Height
                }
                convertCell metadataProvider metaData actualSize allowedSize (contentMapper value)
            | Stack (stackType, items) -> 
                let subRanges =  items value
                let subRangesDynamic = subRanges |> Array.map (fun x -> RangeProxy.minimalContentSize x value, RangeProxy.hasDynamicParts x value)
                let dynamicWidths = subRangesDynamic |> Seq.map (snd >> fst) |> Seq.length
                let dynamicHeights = subRangesDynamic |> Seq.map (snd >> snd) |> Seq.length

                let upperDiv x y = (int) (Math.Ceiling((float)x / (float) y))

                let maxDynamicWidths =  if stackType = Horizontal then 1 + upperDiv (allowedSize.Width - minimalSize.Width) dynamicWidths else allowedSize.Width
                let maxDynamicHeight = if stackType = Vertical then 1 + upperDiv (allowedSize.Height - minimalSize.Height) dynamicHeights else allowedSize.Height

//                printfn "convert' %A with %s" stackType (sprintf "{ Width = %d; Height = %d }" allowedSize.Width  allowedSize.Height)
//
//                printfn "maxDynamicWidths %d" maxDynamicWidths
//                printfn "maxDynamicHeight %d" maxDynamicHeight
//
//                printfn "minimalContentSizes %A"  (String.Join("|", Seq.map  (fst >> (fun x -> sprintf "{ Width = %d; Height = %d }" x.Width x.Height)) subRangesDynamic )) 

                let contentSizes = 
                    (allowedSize , subRangesDynamic)
                    ||> Seq.mapFold(fun restSize (contentSize,(dynamicWidth,dynamicHeight)) ->
                        let actualSize : ContentSize = { 
                            Width = if dynamicWidth then Math.Min(restSize.Width, Math.Max(maxDynamicWidths, contentSize.Width)) else contentSize.Width
                            Height = if dynamicHeight then Math.Min(restSize.Height, Math.Max(maxDynamicHeight, contentSize.Height)) else contentSize.Height
                        }
                        actualSize, substractSize stackType restSize actualSize ) 
                    |> fst
                    |> Seq.toArray

//                printfn "actualContentSizes  %A" (String.Join("|", Seq.map (fun x -> sprintf "{ Width = %d; Height = %d }" x.Width x.Height) contentSizes )) 
                let updateMetaData = fun (m,size) -> metadataProvider.UpdateMetaData(m,stackType,size)
                let views =
                    Seq.mapFold(fun pos (size, range) -> convert' value pos  size range, updateMetaData(pos,size)) metaData (Seq.zip contentSizes subRanges)
                    |> fst
                    |> Seq.toArray

                StackView (stackType, views)
        convert' value metadataProvider.Default totalSize proxy
    //
//    