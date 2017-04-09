namespace FSharp.ReportDSL

module Helpers = 
    let const' a b = a   

    let fold1 acc (xs : _ seq) =
        let xs' = Seq.toArray xs
        let head =  Array.head xs'    
        let tail =  Array.tail xs'    
        Seq.fold acc head tail