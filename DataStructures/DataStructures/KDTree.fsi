namespace DataStructures


open System
open System.IO


module KDTree =


    type IntVec = array<int>


    type KDTreeNode = {
        LeftChild : option<KDTreeNode>;
        RightChild : option<KDTreeNode>;
        SplittingAxis : int;
        NodeVal : IntVec}


    val genRandIntVec : int -> int -> IntVec


    val genRandIntVecsArr : int -> int -> int -> array<IntVec>


    val buildKDTree : array<IntVec> -> option<KDTreeNode>


    val distSq : IntVec -> IntVec -> int


    val getBestMatch : KDTreeNode -> IntVec -> (IntVec -> IntVec -> int) -> IntVec


    val getBestMatchByLinearScanning : array<IntVec> -> IntVec -> (IntVec -> IntVec -> int) -> IntVec

