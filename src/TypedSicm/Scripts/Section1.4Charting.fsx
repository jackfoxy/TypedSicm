#load "main.group.fsx"
#r "../bin/Release/netstandard2.0/TypedSicm.dll"

//#load "../GenericArithmetic.fs"
//#load "../TypedSicmPrelude.fs"
//#load "../Utilities.fs"
//#load "../NelderMead.fs"
//#load "../Ch1LagrangianMechanics.fs"

open System.Diagnostics
open XPlot.GoogleCharts
open TypedSicm
open Utilities
open GenericArithmetic

module Vector = List

open Ch1_LagrangianMechanics.S4ComputingActions

let findHarmonicPath interpolants = 
    let watch = Stopwatch.StartNew()
    let result = 
        findPath (lagrangianHarmonic 1.0 1.0) (floatToTime 0.0) (Real.Float 1.0) (pi/2) (Real.Float 0.0) interpolants
    watch.Stop()
    printfn "Find harmonic path from %i interpolants in %A" interpolants watch.Elapsed
    result

let increment = (pi/2) / 1000
let increments = generateList 1000 (fun i -> i * increment)

let harmonicPathFrom3Interpolants = findHarmonicPath 3

let options =
    Options(
        title = sprintf "Harmonic Lagrangian" ,
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

let harmonicPath path =
    path
    |> Vector.map (fun x -> 
         match x with 
         | Indexable.Scalar s ->
            let s' = scalarToFloat s
            [s', s']
         | Indexable.Func f -> 
            increments
            |> Vector.map (fun x -> 
                scalarToFloat x, (f.Invoke x |> scalarToFloat)
            )

    ) 
    |> Vector.head
    |> Vector.toSeq
    |> Array.ofSeq

let path3 = harmonicPath harmonicPathFrom3Interpolants

path3
|> Chart.Line 
|> Chart.WithOptions options
|> Chart.Show

//@@@@@@@@@@@@@@@@

let options2 =
    Options(
        title = sprintf "Harmonic Lagrangian from Function" ,
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

let harmonicPathFromFunction =
    increments
    |> Vector.map (fun x ->  (x |> scalarToFloat), System.Math.Cos (x |> scalarToFloat) )
    |> Vector.toSeq
    |> Array.ofSeq

harmonicPathFromFunction
|> Chart.Line 
|> Chart.WithOptions options2
|> Chart.Show

//@@@@@@@@@@@@@@@@

let options3 =
    Options(
        title = sprintf "Harmonic Lagrangian vs Function" ,
        vAxis =
            Axis(
                title = "Error"
            ),
        hAxis =
            Axis(
                title = "Time"
            ),
        displayAnnotations = true
    )

let diff pathFromInterpolants =
    Array.zip pathFromInterpolants harmonicPathFromFunction
    |> Array.map (fun ((keyx, x), (_, y)) -> keyx, x - y)

let harmonicPathFrom2Interpolants = findHarmonicPath 2
let harmonicPathFrom4Interpolants = findHarmonicPath 4

let diff2Interpolations = harmonicPath harmonicPathFrom2Interpolants |> diff
let diff3Interpolations = diff path3
let diff4Interpolations = harmonicPath harmonicPathFrom4Interpolants |> diff

[diff2Interpolations; diff3Interpolations; diff4Interpolations]
|> Chart.Line 
|> Chart.WithOptions options3
|> Chart.WithLabels ["2 interpolations"; "3 interpolations"; "4 interpolations";]
|> Chart.Show

[diff3Interpolations; diff4Interpolations]
|> Chart.Line 
|> Chart.WithOptions options3
|> Chart.WithLabels ["3 interpolations"; "4 interpolations";]
|> Chart.Show

[diff4Interpolations]
|> Chart.Line 
|> Chart.WithOptions options3
|> Chart.WithLabels ["4 interpolations";]
|> Chart.Show
