/// <summary>
/// 算法导论上机使用的算法测试部分
/// Auther：乔新文
/// StudentID：14130140393
/// </summary>
module Test
    open SRAlgorithmLib.AlgorithmLib1
    open SRAlgorithmLib.AlgorithmLib2
    open SRAlgorithmLib.AlgorithmLib3
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

        printfn "\n\n4:"
        let adjList: Set<Node>[] = Array.create 16 Set.empty<Node>
        adjList.[0] <- set [Node(1,5.0);Node(2,3.0)]
        adjList.[1] <- set [Node(0,5.0);Node(3,1.0);Node(4,3.0);Node(5,6.0)]
        adjList.[2] <- set [Node(0,3.0);Node(4,8.0);Node(5,7.0);Node(6,6.0)]
        adjList.[3] <- set [Node(1,1.0);Node(7,6.0);Node(8,8.0)]
        adjList.[4] <- set [Node(1,3.0);Node(2,8.0);Node(7,3.0);Node(8,5.0)]
        adjList.[5] <- set [Node(1,6.0);Node(2,7.0);Node(8,3.0);Node(9,3.0)]
        adjList.[6] <- set [Node(2,6.0);Node(8,8.0);Node(9,4.0)]
        adjList.[7] <- set [Node(3,6.0);Node(4,3.0);Node(10,2.0);Node(11,2.0)]
        adjList.[8] <- set [Node(3,8.0);Node(4,5.0);Node(5,3.0);Node(6,8.0);Node(11,1.0);Node(12,2.0)]
        adjList.[9] <- set [Node(5,3.0);Node(6,4.0);Node(11,3.0);Node(12,3.0)]
        adjList.[10] <- set [Node(7,2.0);Node(13,3.0);Node(14,5.0)]
        adjList.[11] <- set [Node(7,2.0);Node(8,1.0);Node(9,3.0);Node(13,5.0);Node(14,2.0)]
        adjList.[12] <- set [Node(8,2.0);Node(9,3.0);Node(13,6.0);Node(14,6.0)]
        adjList.[13] <- set [Node(10,3.0);Node(11,5.0);Node(12,6.0);Node(15,4.0)]
        adjList.[14] <- set [Node(10,5.0);Node(11,2.0);Node(12,6.0);Node(15,3.0)]
        adjList.[15] <- set [Node(13,4.0);Node(14,3.0)]
        let arr4 = AdjListToAdjMatrix adjList
        let m,s = Floyd arr4
        PrintFloyd s 0 15

        printfn "\n\n5:"
        let X51 = "xzyzzyx"
        let Y51 = "zxyyzxz"
        printfn "X:%s\tY:%s" X51 Y51
        printfn "LCSs:%A" (LCSs X51 Y51)
        let X52 = "MAEEEVAKLEKHLMLLRQEYVKLQKKLAETEKRCALLAAQANKESSSESFISRLLAIVAD"
        let Y52 = "MAEEEVAKLEKHLMLLRQEYVKLQKKLAETEKRCTLLAAQANKENSNESFISRLLAIVAG"
        printfn "X:%s\tY:%s" X52 Y52
        printfn "LCSs:%A" (LCSs X52 Y52)
    let P3 () =
        printfn "\n\n1:"
        let w1 = [50;30;45;25;5]
        let v1 = [200;180;225;200;50]
        printfn "%A" (ks 100 w1 v1)

        printfn "\n\n2:"
        let w2 = [50;30;45;25;5]
        let v2 = [200;180;225;200;50]
        printfn "%A" (ksf 100 w2 v2)

        printfn "\n\n3:"
        let j3 = ["j1";"j2";"j3";"j4"]
        let t3 = [15;8;3;10]
        printfn "%A" (SJF j3 t3)

        printfn "\n\n4:"
        let s4 = [ 0.5;0.7;0.3;0.9;0.6;0.8;0.1;0.4;0.2;0.5]
        let c5 = 1.0
        printfn "%A" (Packing c5 s4)