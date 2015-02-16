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


    let genRandIntVec (dim : int) (maxVal : int) : IntVec =
        Array.init dim (fun s -> rnd.Next() % maxVal)


    let genRandIntVecsArr (dim : int) (numOfVecs : int) (maxVal : int) : array<IntVec> =
        Array.init numOfVecs (fun s -> genRandIntVec dim maxVal)


    let buildKDTree (vecsArr : array<IntVec>) : option<KDTreeNode> =
        let rec buildKDTreeHelper (depth : int) (vecsArr2 : array<IntVec>) : option<KDTreeNode> =
            if vecsArr2.Length > 1 then
                let currSplittingAxis = depth % vecsArr2.[0].Length
                let sVA = vecsArr2 |> Array.sortBy (fun s -> s.[currSplittingAxis])
                let medIndex = sVA.Length / 2
                {
                KDTreeNode.LeftChild = buildKDTreeHelper (depth + 1) (sVA.[..(medIndex - 1)]);
                KDTreeNode.RightChild = buildKDTreeHelper (depth + 1) (sVA.[(medIndex + 1)..]);
                KDTreeNode.SplittingAxis = currSplittingAxis;
                KDTreeNode.NodeVal = sVA.[medIndex]} |> Some
            elif vecsArr2.Length = 1 then
                let cSX = depth % vecsArr2.[0].Length
                {
                KDTreeNode.LeftChild = None;
                KDTreeNode.RightChild = None;
                KDTreeNode.SplittingAxis = cSX;
                KDTreeNode.NodeVal = vecsArr2.[0]} |> Some
            else
                None
        buildKDTreeHelper 0 vecsArr


    let distSq (vec1 : IntVec) (vec2 : IntVec) : int =
        (vec1, vec2)
        ||> Array.map2 (fun s t -> let u = s - t in u * u)
        |> Array.sum


    let getBestMatch (kDT : KDTreeNode) (testVec : IntVec) (normFunc : IntVec -> IntVec -> int) : IntVec =
        let rec getCandBestMatch (currKDT : KDTreeNode) (testVec2 : IntVec) (parAcc : list<KDTreeNode>) : KDTreeNode =
            match (
                    testVec2.[currKDT.SplittingAxis] < currKDT.NodeVal.[currKDT.SplittingAxis],
                    currKDT.LeftChild, currKDT.RightChild) with
            | true, Some leftChild, _ | false, Some leftChild, None -> getCandBestMatch leftChild testVec2 (currKDT :: parAcc)
            | true, None, Some rightChild | false, _, Some rightChild -> getCandBestMatch rightChild testVec2 (currKDT :: parAcc)
            | _ -> checkCandBestMatch currKDT testVec2 parAcc
        and checkCandBestMatch (currBestMatchNode : KDTreeNode) (testVec2 : IntVec) (parAcc : list<KDTreeNode>) : KDTreeNode =
            match parAcc with
            | [] -> currBestMatchNode
            | parNode :: t ->
                let currBestMatchDist = normFunc testVec2 currBestMatchNode.NodeVal
                let parDist = normFunc testVec2 parNode.NodeVal
                let planeSplittingDist = testVec2.[parNode.SplittingAxis] - parNode.NodeVal.[parNode.SplittingAxis]
                let planeSplittingDistSq = planeSplittingDist * planeSplittingDist
                match (
                        currBestMatchDist <= parDist,
                        currBestMatchDist < planeSplittingDistSq,
                        parDist < planeSplittingDistSq) with
                | (a, b, c) when (a = true && b = false) || (a = false && c = false) ->
                    let (tN, tD) = if a then (currBestMatchNode, currBestMatchDist) else (parNode, parDist)
                    let testBestMatchNode =
                        if parNode.RightChild.IsSome && planeSplittingDist < 0 then getCandBestMatch parNode.RightChild.Value testVec2 []
                        elif parNode.LeftChild.IsSome && planeSplittingDist >= 0 then getCandBestMatch parNode.LeftChild.Value testVec2 []
                        else tN
                    if (normFunc testVec2 testBestMatchNode.NodeVal) < tD then
                        checkCandBestMatch testBestMatchNode testVec2 t
                    else checkCandBestMatch tN testVec2 t
                | true, true, _ -> checkCandBestMatch currBestMatchNode testVec2 t
                | _ -> checkCandBestMatch parNode testVec2 t
        (getCandBestMatch kDT testVec []).NodeVal


    let getBestMatchByLinearScanning (vecsArr : array<IntVec>) (testVec : IntVec) (normFunc : IntVec -> IntVec -> int) : IntVec =
        (vecsArr |> Array.sortBy (fun s -> normFunc s testVec)).[0]

        