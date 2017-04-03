module Test
    open AlgorithmLib1
    open AlgorithmLib2
    let P1 () =
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
    let P2 () =
        printfn "\n\n1:"
        let arra = [|3;5;2;1;0|]
        printfn "Matrix Set A:%A" arra     
        let _,s = MatrixChainOrder arra
        PrintOptimalParens s 1 (arra.Length - 1)
        let arrb = [|2;7;3;6;10|]
        printfn "\nMatrix Set B:%A" arrb       
        let _,s = MatrixChainOrder arrb
        PrintOptimalParens s 1 (arrb.Length - 1)
        let arrc = [|10;3;15;12;7;2|]
        printfn "\nMatrix Set C:%A" arrc      
        let _,s = MatrixChainOrder arrc
        PrintOptimalParens s 1 (arrc.Length - 1)
        let arrd = [|7;2;4;15;20;5|]
        printfn "\nMatrix Set D:%A" arrd     
        let _,s = MatrixChainOrder arrd
        PrintOptimalParens s 1 (arrd.Length - 1)

        printfn "\n\n2:"
        let X1 = " xzyzzyx"
        let Y1 = "zxyyzxz"
        printfn "X:%s\tY:%s" X1 Y1
        printf "LCS:"
        PrintLCS (LCSLength X1 Y1) X1 Y1 X1.Length Y1.Length
        let X2 = "ALLAAQANKESSSESFISRLLAIVAD"
        let Y2 = "KLQKKLAETEKRCTLLAAQANKENSNESFISRLLAIVAG"
        printfn "\nX:%s\tY:%s" X2 Y2
        printf "LCS:"
        PrintLCS (LCSLength X2 Y2) X2 Y2 X2.Length Y2.Length

        printfn "\n\n3:"
        let arr3 = [|-2;11;-4;13;-5;-2|]
        let sum,beginIndex,endIndex = MaxSum arr3
        printfn "MaxSun:%i" sum
        printfn "subsequence:%A" arr3.[beginIndex .. endIndex]