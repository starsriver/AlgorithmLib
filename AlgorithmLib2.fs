module AlgorithmLib2
    open  System.Collections.Generic 
    let temp = 0
    let memoize (f:'a -> 'b) =
        let t = new System.Collections.Generic.Dictionary<'a,'b>()
        fun n ->
            if t.ContainsKey(n) then t.[n]
            else 
                let r = f n
                t.Add(n, r)
                r

    let MatrixChainOrder (p: int []) =
        let n = p.Length - 1
        let m = Array2D.create (n + 1) (n + 1) 0.0
        let s = Array2D.create n (n + 1) 0
        for l = 2 to n do
            for i = 1 to n - l + 1 do 
                let j = i + l - 1
                m.[i,j] <- infinity
                for k = i to j - 1 do
                    let q = m.[i,k] + m.[k + 1,j] + float (p.[i - 1] * p.[k] * p.[j])
                    if q < m.[i,j] then
                        m.[i,j] <- q
                        s.[i,j] <- k
        (m,s)

    let rec PrintOptimalParens (s:int [,]) (i:int) (j:int) =
        if i = j then 
            printf "A%i" i
        else 
            printf "("
            PrintOptimalParens s i s.[i,j]
            PrintOptimalParens s (s.[i,j] + 1) j
            printf ")"

    let LCSLength (X:string) (Y:string) = 
        let m = X.Length
        let n = Y.Length
        let c = Array2D.create (m + 1) (n + 1) 0
        for i = 0 to m - 1 do
            for j = 0 to n - 1 do
                if X.[i] = Y.[j] then 
                    c.[i + 1,j + 1] <- c.[i,j] + 1
                elif c.[i,j + 1] >= c.[i + 1,j] then
                    c.[i + 1,j + 1] <- c.[i,j + 1]
                else
                    c.[i + 1,j + 1] <- c.[i + 1,j]
        c
    let rec PrintLCS (c : int [,]) (X:string) (Y:string) i j =
        if i = 0 || j = 0 then
            ()
        elif X.[i - 1] = Y.[j - 1] then 
            PrintLCS c X Y (i - 1) (j - 1)
            printf "%c" X.[i - 1]
        elif c.[i - 1,j] >= c.[i,j - 1] then
            PrintLCS c X Y (i - 1) j
        else
            PrintLCS c X Y i (j - 1)
        
    let MaxSum (A:int []) =
        let mutable i,nowSum,maxSum,beginIndex,endIndex = 0,0,0,0,0
        while i < A.Length do
            nowSum <- nowSum + A.[i]
            if nowSum > maxSum then
                maxSum <- nowSum
                endIndex <- i
            if nowSum < 0 then
                nowSum <- 0
                if i + 1 >= A.Length then
                    beginIndex <- i
                else
                    beginIndex <- i + 1
            i <- i + 1
        maxSum,beginIndex,endIndex

