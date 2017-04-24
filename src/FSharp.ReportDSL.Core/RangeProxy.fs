namespace FSharp.ReportDSL.Core

open System
open FSharp.ReportDSL

type ContentSizeType = 
    | Dynamic of int  // min dynamic size
    | Static of int 
    member __.UpdateMinSize(x : int) = 
        match __ with
        | Dynamic min -> Dynamic (Math.Max(x, min)) 
        | Static min -> Static (Math.Max(x, min)) 
        



type NumericFormat = string

type CellContent = 
    | Empty
    | String of string
    | Int of Value : Nullable<int>
    | Float of Value : Nullable<double> * NumericFormat : NumericFormat
    static member FromString s = String s
    static member FromDouble d = Float (Option.toNullable d, "0.0")
    static member FromInt (i : int option) = Int (Option.toNullable i)


type ContentProxySize = 
    { Width : ContentSizeType; Height : ContentSizeType }
    member __.UpdateMinSize(minSize : ContentSize) = { Width = __.Width.UpdateMinSize minSize.Width; Height = __.Height.UpdateMinSize minSize.Height } 
    static member Zero = { Width = Static 0; Height =  Static 0}
    static member DefaultStatic =  { Width = Static 1; Height = Static 1 }
    static member DefaultDyanmic = { Width = Dynamic 1; Height =  Dynamic 1}


and ContentSize = 
    { Width : int; Height : int }
    static member width  ({ Width = x } : ContentSize) : int = x
    static member height ({ Height = x } : ContentSize) : int = x 
    static member combineH (cs : ContentSize seq) = 
        printfn "combineH : %A" cs
        {
            Width = (cs |> Seq.map ContentSize.width |> Seq.sum) 
            Height = (cs |> Seq.map ContentSize.height |> Seq.max) 
        }

    static member combineV (cs : ContentSize seq) =
        printfn "combineV : %A" cs
        { 
            Width = (cs |> Seq.map ContentSize.width |> Seq.max) 
            Height = (cs |> Seq.map ContentSize.height |> Seq.sum) 
        }


type CellProxy<'t> = { 
    ContentMapper : 't -> CellContent
    CellSize : 't -> ContentProxySize
}

module CellProxy =
    open Helpers
    let minimalSize = function
        | Dynamic min -> min
        | Static x -> x   

    let isDynamic = function
        | Dynamic _ -> true
        | Static x -> false 
  
    let minimalSizeFromProxy ({Width = width; Height = height} : ContentProxySize ) = { Width = minimalSize width; Height = minimalSize height}

    let dynamicCellsFromProxy ({Width = width; Height = height} : ContentProxySize ) = isDynamic width , isDynamic height 
       
    let minimalContentSize ({CellSize = cellSize})  = minimalSizeFromProxy << cellSize

    let hasDynamicParts  ({CellSize = cellSize}) =  dynamicCellsFromProxy << cellSize

    let contramap (f : 'u -> 't)  ({ ContentMapper = contentMapper; CellSize = cellSize } : CellProxy<'t>) : CellProxy<'u>  = {ContentMapper = f >> contentMapper;  CellSize =  f >> cellSize}


    let create<'t> mapping : CellProxy<'t>  = { ContentMapper = mapping; CellSize = const' ContentProxySize.DefaultDyanmic}


type StackType = | Horizontal | Vertical

type RangeProxy<'t> = 
    | Cell of CellProxy<'t>
    | Stack of StackType * ('t -> RangeProxy<'t> [])

module RangeProxy =
    open Helpers

    let internal substractSize stackType (a : ContentSize) (b : ContentSize) : ContentSize =
            match stackType with
            | Horizontal -> { a with Width = a.Width - b.Width}
            | Vertical -> {a with Height = a.Height - b.Height}

    let rec minimalContentSize = function 
        | Cell cell -> CellProxy.minimalContentSize cell
        | Stack (stacktype, items) -> 
            let sizes = fun x -> Seq.map (fun item -> minimalContentSize item x) (items x) |> Seq.toArray
            match stacktype with
                | Horizontal -> fun x -> ContentSize.combineH (sizes x)
                | Vertical -> fun x -> ContentSize.combineV (sizes x)

    let rec hasDynamicParts = function 
        | Cell cell -> CellProxy.hasDynamicParts cell
        | Stack (stacktype, items) -> 
            fun x ->
                items x
                |> Seq.map (fun item -> hasDynamicParts item x)
                |> fold1 (fun (w,h) (w',h') -> (w && w', h && h'))

    let rec updateMinSize (maxMinSize' : 't -> ContentSize) (proxy  : RangeProxy<'t>) : RangeProxy<'m>  = 
        printfn "updateMinSize : %A" maxMinSize'
        match proxy with
        | Cell ({CellSize = cellSize} as cell) -> 
            Cell { cell with CellSize = fun x -> (cellSize x).UpdateMinSize(maxMinSize' x)}
        | Stack (stackType, items) -> 
            let contentSizes value = 
                let maxMinSize = maxMinSize' value
                let minimalSize = minimalContentSize proxy value
                let subRanges =  items value
                let subRangesDynamic = subRanges |> Array.map (fun x -> minimalContentSize x value, hasDynamicParts x value)
                let dynamicWidths = subRangesDynamic |> Seq.map (snd >> fst) |> Seq.length
                let dynamicHeights = subRangesDynamic |> Seq.map (snd >> snd) |> Seq.length

                let upperDiv x y = (int) (Math.Ceiling((float)x / (float) y))

                let maxDynamicWidths = if stackType = Horizontal then 1 + upperDiv (maxMinSize.Width - minimalSize.Width) dynamicWidths else maxMinSize.Width
                let maxDynamicHeight = if stackType = Vertical then 1 + upperDiv (maxMinSize.Height - minimalSize.Height) dynamicHeights else maxMinSize.Height

                (maxMinSize , subRangesDynamic)
                ||> Seq.mapFold(fun restSize (contentSize,(dynamicWidth,dynamicHeight)) ->
                    let actualSize : ContentSize = { 
                        Width = if dynamicWidth then Math.Min(restSize.Width, Math.Max(maxDynamicWidths, contentSize.Width)) else contentSize.Width
                        Height = if dynamicHeight then Math.Min(restSize.Height, Math.Max(maxDynamicHeight, contentSize.Height)) else contentSize.Height
                    }
                    actualSize, substractSize stackType restSize actualSize ) 
                |> fst
                |> Seq.toArray

            Stack(stackType, fun t -> Array.map2 updateMinSize (contentSizes t |> Array.map const') (items t))

    let rec contramap (f : 'u -> 't)  = function
        | Cell x -> Cell (CellProxy.contramap f x)
        | Stack (stacktype, items) -> Stack (stacktype, fun u -> Array.map (contramap f) (items (f u)))

    let liftDependency (f : 'a -> RangeProxy<'t>) : RangeProxy<'a * 't> =
       Stack (Horizontal, fun (a,x) -> [| f a |> contramap snd |]) 

    let empty<'t> : RangeProxy<'t> = Cell ({ContentMapper = const' (CellContent.FromString  String.Empty); CellSize = const' ContentProxySize.DefaultDyanmic })

    let zero<'t> : RangeProxy<'t> = Cell ({ContentMapper = const' (CellContent.FromString  String.Empty); CellSize = const' ContentProxySize.Zero })

    let cell<'t> (mapper : ('t -> CellContent) ) = CellProxy.create mapper |> Cell

    let constCell<'t> (content : CellContent) : RangeProxy<'t> = CellProxy.create (const' content) |> Cell

    let constStr<'t> s : RangeProxy<'t> = constCell (CellContent.FromString s)


    let internal syncWidth (xs : RangeProxy<'t> seq) : RangeProxy<'t> seq  =
         let maxMinSize v = 
            let maxByWidth =  xs |> Seq.map (fun x -> minimalContentSize x v) |> Seq.maxBy(fun x -> x.Width) 
            {Width = maxByWidth.Width; Height = 1}
         xs |> Seq.map (fun x -> updateMinSize maxMinSize x)

    let internal syncHeight (xs : RangeProxy<'t> seq) : RangeProxy<'t> seq  =
         let maxMinSize v = 
            let maxByHeight =  xs |> Seq.map (fun x -> minimalContentSize x v) |> Seq.maxBy(fun x -> x.Height) 
            {Width = 1; Height = maxByHeight.Height}
         xs |> Seq.map (fun x -> updateMinSize maxMinSize x)

    /// hack :(
//    let internal syncWidth (from : RangeProxy<'t>) (content : CellContent) =
//         Cell <| { 
//            ContentMapper = const' content
//            CellSize = fun x -> let minSize = minimalContentSize from x in  { Width = Dynamic minSize.Width; Height = Dynamic 1 }
//        }
//
//    let internal syncHeight (from : RangeProxy<'t>) (content : CellContent) =
//         Cell <| { 
//            ContentMapper = const' content
//            CellSize = fun x -> let minSize = minimalContentSize from x in  { Width = Dynamic 1; Height = Dynamic minSize.Height }
//        }


    let stack<'t> stackType (ranges : RangeProxy<'t> seq) = 
        Stack (stackType, const' (ranges |> Seq.toArray))       
    
    let stackMappers<'t> stackType (mappers : ('t -> CellContent) seq) = 
        Stack (stackType, const' (mappers |> Seq.map (CellProxy.create >> Cell) |> Seq.toArray))        

    let fromSeq stackType (rp : RangeProxy<'t>) : RangeProxy<'t []> = 
        let constItem (item :'t) : RangeProxy<'t []> = contramap (const' item) rp
        Stack (stackType , Array.map constItem)     

    let fromFirst (rp : RangeProxy<'t>) : RangeProxy<'t []> = 
        Stack (Horizontal, Seq.tryHead >> Option.toArray >> Array.map (fun x -> contramap (const' x) rp))    
