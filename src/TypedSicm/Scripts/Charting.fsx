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
//open Utilities
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

x 
|> Vector.map (fun x -> 
    let x =
     match x with 
     | Indexable.Scalar s -> scalarToFloat s  :> obj
     | Indexable.Func f -> f.Invoke :> obj
    printfn "%A" x
) 
|> ignore


//let h = MathNet.Numerics.Statistics.Histogram(statistics, 20)

let data, labels = Vector.toSeq (x |> Vector.map (fun x -> match x with | Indexable.Scalar s -> scalarToFloat s )),  []
        //[
    //for i = 0 to h.BucketCount - 1 do
    //    yield (h.[i].LowerBound, h.[i].UpperBound, int h.[i].Count)
    //        ]
    //        |> Seq.map (fun (lowerBound, upperBound, count) ->
    //let data =
    //    significantDigitsToString upperBound, count

    //match
    //    [ 
    //        "fstStdDevAbove", median + stdDev
    //        "fstStdDevBelow", median - stdDev
    //        "twoStdDevAbove", median + (stdDev * 2.)
    //        "twoStdDevBelow", median - (stdDev * 2.)
    //        "median", median
    //    ]
    //    |> List.tryFind (fun (_, value) -> value >= lowerBound && value <= upperBound ) with
    //| Some (label, _) ->
    //    data, label
    //| None ->
    //    data, " "

    //)
    //|> Seq.toList
    //|> List.unzip

(data, labels, options) |> Chart.Line |> Chart.Show
