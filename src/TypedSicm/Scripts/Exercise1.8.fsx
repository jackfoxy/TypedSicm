#r "../bin/Release/netstandard2.0/TypedSicm.dll"

open TypedSicm
open GenericArithmetic
open Ch1_LagrangianMechanics

type Q = Indexable list
type Γ = UpIndexed -> Real -> Local

let f (q : Q) =

   (gamma q) >> (fun (l : Local) -> Real.Int 0)

let delta eta (f : Real -> Real) q =
    eta q + f q
