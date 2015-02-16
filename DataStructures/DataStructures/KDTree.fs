namespace DataStructures


open System
open System.IO


module KDTree =


    type IntVec = array<int>


    [<RequireQualifiedAccess>]
    type KDTreeNode = {
        LeftChild : option<KDTreeNode>;
        RightChild : option<KDTreeNode>;
        SplittingAxis : int;
        NodeVal : IntVec}


    let rnd = Random()


    let genRandIntVec (d : int) (xV : int) : IntVec =
        Array.init d (fun s -> rnd.Next() % xV)


    let genRandIntVecsArr (d : int) (nV : int) (xV : int) : array<IntVec> =
        Array.init nV (fun s -> genRandIntVec d xV)


    let buildKDTree (vA : array<IntVec>) : option<KDTreeNode> =
        let rec buildKDTreeHelper (d : int) (vA2 : array<IntVec>) : option<KDTreeNode> =
            if vA2.Length > 1 then
                let cSX = d % vA2.[0].Length
                let sVA = vA2 |> Array.sortBy (fun s -> s.[cSX])
                let mI = sVA.Length / 2
                {
                KDTreeNode.LeftChild = buildKDTreeHelper (d + 1) (sVA.[..(mI - 1)]);
                KDTreeNode.RightChild = buildKDTreeHelper (d + 1) (sVA.[(mI + 1)..]);
                KDTreeNode.SplittingAxis = cSX;
                KDTreeNode.NodeVal = sVA.[mI]} |> Some
            elif vA2.Length = 1 then
                let cSX = d % vA2.[0].Length
                {
                KDTreeNode.LeftChild = None;
                KDTreeNode.RightChild = None;
                KDTreeNode.SplittingAxis = cSX;
                KDTreeNode.NodeVal = vA2.[0]} |> Some
            else
                None
        buildKDTreeHelper 0 vA


    let distSq (v1 : IntVec) (v2 : IntVec) : int =
        (v1, v2)
        ||> Array.map2 (fun s t -> let u = s - t in u * u)
        |> Array.sum


    let getBestMatch (kDT : KDTreeNode) (tV : IntVec) (dF : IntVec -> IntVec -> int) : IntVec =
        let rec getCandBestMatch (cKDT : KDTreeNode) (tV2 : IntVec) (pA : list<KDTreeNode>) : KDTreeNode =
            match tV2.[cKDT.SplittingAxis] < cKDT.NodeVal.[cKDT.SplittingAxis], cKDT.LeftChild, cKDT.RightChild with
            | true, Some lC, _ | false, Some lC, None -> getCandBestMatch lC tV2 (cKDT :: pA)
            | true, None, Some rC | false, _, Some rC -> getCandBestMatch rC tV2 (cKDT :: pA)
            | _ -> checkCandBestMatch cKDT tV2 pA
        and checkCandBestMatch (cBMN : KDTreeNode) (tV2 : IntVec) (pA : list<KDTreeNode>) : KDTreeNode =
            match pA with
            | [] -> cBMN
            | pN :: t ->
                let cBMD = dF tV2 cBMN.NodeVal
                let pD = dF tV2 pN.NodeVal
                let pSD = tV2.[pN.SplittingAxis] - pN.NodeVal.[pN.SplittingAxis]
                let pSDSq = pSD * pSD
                match cBMD <= pD, cBMD < pSDSq, pD < pSDSq with
                | (a, b, c) when (a = true && b = false) || (a = false && c = false) ->
                    let (tN, tD) = if a then (cBMN, cBMD) else (pN, pD)
                    let nBMN =
                        if pN.RightChild.IsSome && pSD < 0 then getCandBestMatch pN.RightChild.Value tV2 []
                        elif pN.LeftChild.IsSome && pSD >= 0 then getCandBestMatch pN.LeftChild.Value tV2 []
                        else tN
                    if (dF tV2 nBMN.NodeVal) < tD then checkCandBestMatch nBMN tV2 t else checkCandBestMatch tN tV2 t
                | true, true, _ -> checkCandBestMatch cBMN tV2 t
                | _ -> checkCandBestMatch pN tV2 t
        (getCandBestMatch kDT tV []).NodeVal


    let getBestMatchByLinearScanning (vA : array<IntVec>) (tV : IntVec) (dF : IntVec -> IntVec -> int) : IntVec =
        (vA |> Array.sortBy (fun s -> dF s tV)).[0]

        