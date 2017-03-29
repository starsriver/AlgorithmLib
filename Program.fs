// Learn more about F# at http://fsharp.org

open System
open AlgorithmLib
[<EntryPoint>]
let main argv = 
    printfn "1:"
    let arr = [|1;5;9;2;8;3;7;6;4;4|]
    printfn "%A" (SumOfTwoNumber arr 8)

    printfn "2:"
    let arr2 = [|1;5;9;2;8;3;7;6;4;4|]
    let quene = PriorityQuene(arr2)
    printfn "%A" quene.Quene
    printfn "%A" (quene.HeapMaximum ())
    ignore (quene.HeapExtractMax())
    printfn "%A" quene.Quene
    quene.HeapIncreaseKey 8 15
    printfn "%A" quene.Quene
    quene.MaxHeapInsert 20
    printfn "%A" quene.Quene
    
    printfn "3:"    
    let arr3 = [1;5;9;2;8;3;7;6;4;4]
    printfn "%A" (qsort arr3)

    printfn "4:"
    let arr4 = [|105;568;917;255;813;30;7;699;445;400|]
    BuckerSort arr4 2
    printfn "%A" arr4

    printfn "5:"
    let arr5 = [|0;1;1;0;1;1;1;0;0;0|]
    BuckerSort arr5 0
    printfn "%A" arr5

    let mutable a = 1.2
    let mutable b = 12.7
    printfn "%A,%A" a b
    &a <-> &b
    printfn "%A,%A" a b
    0 // return an integer exit code
