#load "KDTree.fs"
open DataStructures
open System.Diagnostics

let d = 4

let nV = 102400 * 5

let xV = 128

let vA = KDTree.genRandIntVecsArr d nV xV

let kDT = KDTree.buildKDTree vA

let tV = KDTree.genRandIntVec d xV

let sW = Stopwatch()
sW.Start()
let bM = KDTree.getBestMatch kDT.Value tV KDTree.distSq
sW.Stop()
printfn "bM = %A; bMD = %A" bM (KDTree.distSq tV bM)
printfn "Time taken with a k-d tree: %A ms" sW.ElapsedMilliseconds

sW.Reset()
sW.Start()
let bML = KDTree.getBestMatchByLinearScanning vA tV KDTree.distSq
sW.Stop()
printfn "bML = %A; bMLD = %A" bML (KDTree.distSq tV bML)
printfn "Time taken with linear scanning: %A ms" sW.ElapsedMilliseconds
