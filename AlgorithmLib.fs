module AlgorithmLib
    let random = System.Random()

    let inline (<->) (x:'T byref) (y:'T byref) = 
        let temp = x
        x <- y
        y <- temp 
        ()

    type System.Int32 with
        member this.Tag (x:int) =
            let temp = int (10.0**float x)
            this / temp % 10

    let Partition (A: int []) p r =
        let x = A.[r]
        let mutable i = p - 1
        for j = p to r - 1 do
            if A.[j] < x then 
                i <- i + 1
                &A.[i] <-> &A.[j]
        &A.[i + 1] <-> &A.[r]
        i + 1
    let rec QuickSort (A:int []) p r =
        if p < r then
            let q = Partition A p r
            QuickSort A p (q - 1)
            QuickSort A (q + 1) r
    let RandomizePartition (A:int []) p r =
        let i = random.Next (p, r)
        &A.[r] <-> &A.[i]
        Partition A p r
    let rec RandomizeQuickSort (A:int []) p r =
        if p < r then
            let q = RandomizePartition A p r
            RandomizeQuickSort A p (q - 1)
            RandomizeQuickSort A (q + 1) r

    let rec qsort (A:int list) = 
        match A with 
            | [] -> []
            | [a] -> [a]
            | head :: tail -> 
                let smaller = List.filter ( (>=) head) tail
                let larger = List.filter ( (<) head) tail
                (qsort smaller) @ [head] @ (qsort larger)
    let rec rqsort (A:int list) =
        match A with
        | [] -> []
        | [a] -> [a]
        | _ ->
            let rmidnum = random.Next (0, A.Length)
            let temp = A.[0 .. rmidnum - 1] @ A.[rmidnum + 1 .. A.Length - 1] //temp is A - [A.[rmidnum]]
            let smaller = List.filter ( (>=) A.[rmidnum]) temp
            let larger = List.filter ( (<) A.[rmidnum]) temp
            (rqsort smaller) @ [A.[rmidnum]] @ (rqsort larger)
    let rec HalfSearch (A: 'T []) (a: 'T) = 
        // A.[0] is min value
        if A.Length = 1 then (A.[0] = a, 0)
        elif A.Length = 0 then (false, 0)
        else
            let mid = A.Length / 2
            if a = A.[mid] then (true,mid)
            elif a < A.[mid] then HalfSearch A.[0 .. mid - 1] a
            else HalfSearch A.[mid + 1 .. A.Length - 1] a

    let ParentNode i =
        (i + 1 ) / 2 - 1
    let LeftNode i =
        i * 2 + 1
    let RightNode i = 
        i * 2 + 2
    let rec MaxHeapify (A: int []) i heapsize =
        let l = LeftNode i
        let r = RightNode i
        let mutable largest = 0
        if l < heapsize && A.[l] >  A.[i] then largest <- l
        else largest <- i

        if r < heapsize &&  A.[r] >  A.[largest] then largest <- r

        if largest <> i then 
            &A.[i] <-> &A.[largest]
            MaxHeapify A largest heapsize

    let BulidHeap (A:int []) =
        for i = A.Length / 2 downto 0 do MaxHeapify A i A.Length

    let HeapSort (A:int []) = 
        BulidHeap A
        for i = A.Length - 1 downto 1 do
            &A.[0] <-> &A.[i]
            MaxHeapify A 0 i

    type PriorityQuene(A:int []) = 
        let mutable quene = Array.empty<int>
        do
            quene <- A.[0 .. A.Length - 1]
            BulidHeap quene
        member this.Quene 
            with get() = quene
        member this.Item
            with get(index) = quene.[index]
            and set index value = quene.[index] <- value
        member this.HeapMaximum () = 
            if quene.Length = 0 then raise (System.Exception ("empty quene"))
            quene.[0]
        member this.HeapExtractMax () = 
            if quene.Length = 0 then raise (System.Exception ("empty quene"))
            let max = quene.[0]
            quene <- quene.[1 .. quene.Length - 1]
            MaxHeapify quene 0 quene.Length
            max
        member this.HeapIncreaseKey i k =
            let mutable j = i
            if k < quene.[j] then raise (System.Exception ("new k is smaller than curren key"))
            quene.[j] <- k
            while j > 0 && quene.[ParentNode j] < quene.[j] do
                &quene.[j] <-> &quene.[ParentNode j]
                j <- ParentNode j
        member this.MaxHeapInsert key = 
            quene <- Array.append quene [|0|]
            this.HeapIncreaseKey (quene.Length - 1) key
    
    let rec InsertSort (A:int list) (s:int)=
        match A with
            | [] -> [s]
            | [a] -> 
                if s < a then [s] @ [a]
                else [a] @ [s]
            | head :: tail ->
                if s < head then [s] @ A
                else [head] @ (InsertSort tail s)
        
    let RadixSort (A:int []) (x: int)= 
        let bucket = Array.create 10 List.empty<int>
        for i = 0 to A.Length - 1 do
            let temp = int (A.[i].Tag x)
            bucket.[temp] <- InsertSort bucket.[temp] A.[i]
        let mutable resurt = List.empty<int>
        for i = 0 to bucket.Length - 1 do
            resurt <- resurt @ bucket.[i]
        for i = 0 to A.Length - 1 do
            A.[i] <- resurt.[i]
        ()

    let BuckerSort (A:int []) (x: int)=
        for i = 0 to x do
            RadixSort A i
        ()

    let Sum (A:int []) (a:int) =
        QuickSort A 0 (A.Length - 1)
        printfn " %A" A
        let mutable i = 0
        let mutable result = (false, 0)
        while i < A.Length && not (fst result) do
            result <- HalfSearch A (a - A.[i])
            i <- i + 1
        (result,A.[i],a - A.[i])