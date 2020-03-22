namespace TypedSicm

open FSharpx.Collections
open Utilities
open GenericArithmetic
open NelderMead
open Ch1_LagrangianMechanics.S4ComputingActions

module Vector = RandomAccessList

module Excercise1_5 =
    let increment = (pi/2) / 1000
    let increments = generateList 1000 (fun i -> i * increment)
    
    let harmonicPath path =
        path
        |> Vector.map (fun x -> 
             match x with 
             | Indexable.Scalar s ->
                let s' = scalarToFloat s
                [|s', s'|] |> Vector.ofSeq
             | Indexable.Func f -> 
                increments
                |> Vector.map (fun x -> 
                    scalarToFloat x, (f.Invoke x |> scalarToFloat)
                )
    
        ) 
        |> Vector.head
        |> Vector.toSeq
        |> Array.ofSeq

    let mutable incrementalPaths : RandomAccessList<Indexable> list = list.Empty 
    
    let parametricPathAction lagrangian t0 q0 t1 q1 qs =
        let path = makePath t0 q0 t1 q1 qs 
        // record path
        incrementalPaths <- path::incrementalPaths
        lagrangianAction lagrangian path t0 t1
    
    let findPath lagrangian time0 q0 time1 q1 n =
        let initialQs = linearInterpolants q0 q1 n
        let minimizingQs =
            multidimensionalMinimize
                (parametricPathAction lagrangian time0 q0 time1 q1)
                initialQs
        makePath time0 q0 time1 q1 minimizingQs
    
    let q = findPath (lagrangianHarmonic 1.0 1.0) (floatToTime 0.0) (Scalar.Float 1.0) (pi/2) (Scalar.Float 0.0) 3

    let getPaths =
        incrementalPaths
        |> List.rev
        |> List.map (fun xs -> harmonicPath xs)
        |> Array.ofList