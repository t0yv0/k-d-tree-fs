#load "KDTree.fs"
open DataStructures
open System.Diagnostics

let dim = 4

let numOfVecs = 1024000 * 25

let maxVal = 128

let vecsArr = KDTree.genRandIntVecsArr dim numOfVecs maxVal

let kDT = KDTree.buildKDTree vecsArr

let testVec = KDTree.genRandIntVec dim maxVal

let sW = Stopwatch()
sW.Start()
let bM = KDTree.getBestMatch kDT.Value testVec KDTree.distSq
sW.Stop()
printfn "bM = %A; bMD = %A" bM (KDTree.distSq testVec bM)
printfn "Time taken with a k-d tree: %A ms" sW.ElapsedMilliseconds

sW.Reset()
sW.Start()
let bML = KDTree.getBestMatchByLinearScanning vecsArr testVec KDTree.distSq
sW.Stop()
printfn "bML = %A; bMLD = %A" bML (KDTree.distSq testVec bML)
printfn "Time taken with linear scanning: %A ms" sW.ElapsedMilliseconds
