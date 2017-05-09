namespace SRAlgorithmLib
/// <summary>
/// 算法导论第四次上机使用的算法模块
/// Auther：乔新文
/// StudentID：14130140393
/// </summary>
module AlgorithmLib4 = 
    open SRAlgorithmLib.AlgorithmLib2

    [<Class>]
    type Vertex = 
        val name : int
        val mutable d : float
        val mutable π : Vertex ref option
        new (_name : int, D : float, PI : Vertex ref option) = {name=_name; d=D; π=PI}

    [<Class>]
    type Edge = 
        val u : Vertex ref
        val v : Vertex ref
        val weight : float
        new (U : Vertex ref, V : Vertex ref, _weight : float) = {u=U; v=V; weight=_weight}

    [<Class>]
    type Graphics (adjMatrix : float [,]) as this =
        do
            this.V <- [for i in 0..(Array2D.length1 adjMatrix - 1) -> Vertex(i,infinity,None)]
            this.E <- [
                for i in 0..(Array2D.length1 adjMatrix - 1) do
                    for j in 0..(Array2D.length2 adjMatrix - 1) do
                        if adjMatrix.[i,j] <> infinity || adjMatrix.[i,j] <> 0.0 then
                            yield Edge(ref this.V.[i], ref this.V.[j], adjMatrix.[i,j])
            ]
        [<DefaultValue>] val mutable V : Vertex List
        [<DefaultValue>] val mutable E : Edge List
    
    let InitializeSingleSource (G:Graphics) (s:int) =
        G.V.[s].d <- 0.0

    let Relax (e:Edge)= 
        if (!e.v).d > (!e.u).d + e.weight then
            (!e.v).d <- (!e.u).d + e.weight
            (!e.v).π <- Some(e.v)
    let BellmanFord (G:float [,]) (s:int)= 
        let g = Graphics(G)
        InitializeSingleSource g s
        for i = 0 to g.V.Length - 2 do
            List.iter Relax g.E
        not (
                List.exists (fun (item : Edge) -> 
                    (!item.v).d > (!item.u).d + item.weight
                ) g.E
            )

    [<Struct>]
    type Node = 
        val ID:int
        val Weight:float
        new (id:int,weight:float) = {ID = id; Weight = weight}

    let AdjListToAdjMatrix (adjList : Set<Node>[]) =
        let adjMatrix = Array2D.create adjList.Length adjList.Length infinity
        for i = 0 to adjList.Length - 1 do
            adjMatrix.[i,i] <- 0.0
            for j in adjList.[i] do
                adjMatrix.[i,j.ID] <- j.Weight
        
        adjMatrix

    let Floyd (G:float [,]) = 
        let n = Array2D.length1 G
        let m = Array2D.copy G
        let s = Array2D.create n n -1
        for i = 0 to n - 1 do
            for j = 0 to n - 1 do
                for k = 1 to n - 1 do
                    let q = m.[i,k] + m.[k,j]
                    if q < m.[i,j] then
                         m.[i,j] <- q
                         s.[i,j] <- k
        (m,s)

    let rec PrintFloyd (s:int [,]) (i:int) (j:int) = 
        if i = j then
            printf "%A " i
        elif s.[i,j] = -1 then
            printf "%A %A " i j
        else
            PrintFloyd s i s.[i,j]
            PrintFloyd s s.[i,j] j

    let canPlace (s: int []) (n:int) =
        let mutable i = 0
        let mutable result = true
        while result && i < n do
            if s.[i] = s.[n] || abs (n - i) = abs(s.[n] - s.[i]) then
                result <- false
            else
                i <- i + 1
        result
    let nQuene n =
        let s = Array.create n 0
        let mutable k = 0
        while s.[0] < n do
            if s.[k] >= n then
                s.[k] <- 0
                k <- k - 1
                s.[k] <- s.[k] + 1
            else
                if canPlace s k then
                    if k = n - 1 then
                        printfn "%A" s
                        s.[k] <- s.[k] + 1
                    else 
                        k <- k + 1
                else
                    s.[k] <- s.[k] + 1