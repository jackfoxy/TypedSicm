#load "main.group.fsx"
//#r "../bin/Debug/netstandard2.0/TypedSicm.dll"


#load "../GenericArithmetic.fs"
#load "../TypedSicmPrelude.fs"
#load "../Utilities.fs"
#load "../NelderMead.fs"
#load "../Ch1LagrangianMechanics.fs"

open XPlot.GoogleCharts
open FSharpx.Collections
open TypedSicm
open Utilities
open GenericArithmetic

module Vector = RandomAccessList

open Ch1_LagrangianMechanics.S4ComputingActions

let x = q

let options =
    Options(
        title = sprintf " Median:  StdDev:" ,
           // connName.Value stats.Name wellType stats.MeasurementType (significantDigitsToString stats.Median) (significantDigitsToString stats.StdDev),
        vAxis =
            Axis(
                title = "Count"
            ),
        hAxis =
            Axis(
                title = "Upper Bound"
            ),
        displayAnnotations = true
    )



//to do: x is a function, generate n plotable points

let increment = (pi/2) / 1000
let xs = generateList 1000 (fun i -> i * increment)


let xs' =
    x
    |> Vector.map (fun x -> 
         match x with 
         | Indexable.Scalar s ->
            let s' = scalarToFloat s
            [|s', s'|] |> Vector.ofSeq
         | Indexable.Func f -> 
            xs
            |> Vector.map (fun x -> 
                scalarToFloat x, (f.Invoke x |> scalarToFloat)
            )

    ) 



let blah = xs'.Head

let s = Vector.toSeq blah

let data =  Array.ofSeq s

let n = 1

let labels =  ["time"; ""]

(data, labels, options) |> Chart.Line |> Chart.Show
//@@@@@@@@@@@@@@@@

let options' =
    Options(
        title = sprintf "Pendulum" ,
           // connName.Value stats.Name wellType stats.MeasurementType (significantDigitsToString stats.Median) (significantDigitsToString stats.StdDev),
        vAxis =
            Axis(
                title = "Position"
            ),
        hAxis =
            Axis(
                title = "Time"
            ),
        displayAnnotations = true
    )

let ys' =
    xs
    |> Vector.map (fun x ->  (x |> scalarToFloat), System.Math.Cos (x |> scalarToFloat) )

let s' = Vector.toSeq  ys'

let data' =  Array.ofSeq s'

(data', labels, options') |> Chart.Line |> Chart.Show

let diff =
    Array.zip data data'
    |> Array.map (fun ((keyx, x), (keyy, y)) -> keyx, x - y)


(diff, labels, options') |> Chart.Line |> Chart.Show