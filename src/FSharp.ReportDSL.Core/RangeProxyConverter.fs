namespace FSharp.ReportDSL

namespace FSharp.ReportDSL.Converters  
open System  
open FSharp.ReportDSL  
open FSharp.ReportDSL.Helpers
open FSharp.ReportDSL.Core 


type CellPositionType = | Absolute | Relative

type CellPosition = {X : int; Y : int}

type StackView =
    | CellView of CellPosition : CellPosition * CellSize : ContentSize * CellContent : CellContent 
    | StackView of StackType * StackView []


type IStackViewAggregator<'t> = 
    interface
        abstract Convert : (CellPosition * ContentSize * CellContent) -> 't
        abstract VCombine : 't * 't -> 't
        abstract HCombine : 't * 't -> 't
    end
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]       
module StackView = 
    let rec aggregate (converter :  IStackViewAggregator<'t>) (view : StackView) = 
        match view with
        | CellView (pos,size,content) -> converter.Convert(pos,size,content)
        | StackView (Horizontal, views) -> fold1 (fun l r -> converter.HCombine(l,r)) (Seq.map (aggregate converter) views)
        | StackView (Vertical, views) ->  fold1 (fun l r -> converter.VCombine(l,r)) (Seq.map (aggregate converter) views)
    let rec contentSize  = function
        | CellView (_,size,_) -> size
        | StackView (Horizontal, views) -> Seq.map contentSize views |> ContentSize.combineH
        | StackView (Vertical, views)   -> Seq.map contentSize views |> ContentSize.combineV   

module RangeProxyConveter =
    open Helpers

    let private updatePosition stackType (position : CellPosition, contentsize : ContentSize) =
        match stackType with
        | Horizontal -> {position with X = position.X + contentsize.Width}
        | Vertical -> {position with Y = position.Y + contentsize.Height}

    let private convertCell (position : CellPosition) ({Width = actualWidth; Height = actualHeight} : ContentSize) ({Width = allowedWidth; Height = allowedHeight} : ContentSize) (content : CellContent)  = 
        StackView(Horizontal, 
            [|
            StackView (Vertical, 
                [| 
                    CellView (position, {Width = actualWidth; Height = actualHeight},content)
                    CellView ({ position with X = position.X + actualWidth}, {Width = allowedWidth - actualWidth; Height = actualHeight},CellContent.Empty)
                |] )
            StackView (Vertical,
                [|
                    CellView ({position with Y = position.Y + actualHeight}, {Width = actualWidth; Height = allowedHeight - actualHeight},CellContent.Empty )
                    CellView ({X = position.X + actualWidth; Y = position.Y + actualHeight}, {Width = allowedWidth - actualWidth; Height =  allowedHeight - actualHeight}, CellContent.Empty)
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

    

    let convert (proxy  : RangeProxy<'t>) (value : 't) : StackView = 
        let totalSize = RangeProxy.minimalContentSize proxy value
     
        let rec convert' (value : 't) (position : CellPosition) (allowedSize : ContentSize) (proxy  : RangeProxy<'t>) : StackView  =
            let minimalSize = RangeProxy.minimalContentSize proxy value
            let dynamicWidth, dynamicHeight = RangeProxy.hasDynamicParts proxy value
            match proxy with
            | Cell {ContentMapper = contentMapper;  CellSize = cellSize} -> 

                let actualSize : ContentSize = { 
                    Width = if dynamicWidth then allowedSize.Width else minimalSize.Width
                    Height = if dynamicHeight then allowedSize.Height else minimalSize.Height
                }
                convertCell position actualSize allowedSize (contentMapper value)
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
                let updatePosition = updatePosition stackType
                let views =
                    Seq.mapFold(fun pos (size, range) -> convert' value pos  size range, updatePosition(pos,size)) position (Seq.zip contentSizes subRanges)
                    |> fst
                    |> Seq.toArray

                StackView (stackType, views)
        convert' value {X = 0; Y = 0} totalSize proxy
    //
//    