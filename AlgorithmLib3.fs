namespace SRAlgorithmLib
/// <summary>
/// 算法导论第三次上机使用的算法模块
/// Auther：乔新文
/// StudentID：14130140393
/// </summary>
module AlgorithmLib3 = 
    let rec ks c w (v:int list)=
        if c <= 0 then 0
        elif List.length w = 1 then
            if w.Head > c then 0
            else v.Head
        else
            let w' = w.Tail
            let v' = v.Tail
            if w.Head > c then 
                ks c w' v'
            else
                max (ks c w' v') ((ks (c - w.Head) w' v') + v.Head)

    let rec ksfWithSorted c w (vWith1W : float list) =
        if c <= 0.0 || List.length w <= 0 then 0.0
        else
            if c >= w.Head then
                w.Head * vWith1W.Head + (ksfWithSorted (c - w.Head) w.Tail vWith1W.Tail)
            else
                c * vWith1W.Head
    
    let ksf c (w: int list) (v: int list) =
        let c' = float c
        let wf = List.map (fun item -> (item |> float)) w
        let vWith1W = [for i in 0 .. w.Length - 1 -> (float v.[i])/wf.[i]]
        let vAndvWith1W = List.zip wf vWith1W
        let vAndvWith1WSorted = List.rev (List.sortBy (fun item -> item |> snd) vAndvWith1W)

        let (w',vWith1W') = List.unzip vAndvWith1WSorted
        ksfWithSorted c' w' vWith1W'

    let SJF j t =
        let jobsAndTime = List.zip j t
        let jobsSortedByTime = List.sortBy (fun item -> item |> snd) jobsAndTime
        let (j',t') = List.unzip jobsSortedByTime
        let sumTime = List.mapi (fun i item -> float (item * (t'.Length - i))) t'
        (j',List.average sumTime)
    
    let Packing c s =
        let s' = List.sort s
        let mutable boxs = Array.create 0 1.0
        List.iter (fun item -> 
            let index = Array.tryFindIndex (fun box -> (box - item) > -0.0000001) boxs
            if index.IsSome then
                boxs.[index.Value] <- boxs.[index.Value] - item
            else
                boxs <- Array.append boxs [|1.0|]
                boxs.[boxs.Length - 1] <- boxs.[boxs.Length - 1] - item
            ) s'
        boxs.Length