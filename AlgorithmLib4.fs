namespace SRAlgorithmLib
/// <summary>
/// 算法导论第二次上机使用的算法模块
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

    let Relax (u : Vertex ref) (v : Vertex ref) (E : Edge list)= 
        let index = List.tryFindIndex (fun (item : Edge) -> item.u = u && item.v = v) E
        if index.IsNone then
            ()
        else 
            if (!v).d > (!u).d + E.[index.Value].weight then
                (!v).d <- (!u).d + E.[index.Value].weight
                (!v).π <- Some(u)
    let BellmanFord (G:float [,]) = 
        let g = Graphics(G)
        for i = 0 to g.V.Length - 2 do
            List.iter (fun (item : Edge) -> Relax item.u item.v g.E) g.E
        ()