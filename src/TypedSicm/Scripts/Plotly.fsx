#load "main.group.fsx"
//#r "../bin/Debug/netstandard2.0/TypedSicm.dll"


//#load "../GenericArithmetic.fs"
//#load "../TypedSicmPrelude.fs"
//#load "../Utilities.fs"
//#load "../NelderMead.fs"
//#load "../Ch1LagrangianMechanics.fs"

open XPlot.Plotly
open FSharpx.Collections
//open TypedSicm
//open Utilities
//open GenericArithmetic

let trace1 =
    Scatter(
        x = [1; 2; 3; 4],
        y = [10; 15; 13; 17]
    )

let trace2 =
    Scatter(
        x = [2; 3; 4; 5],
        y = [16; 5; 11; 9]
    )

[trace1; trace2]
|> Chart.Plot
|> Chart.WithWidth 700
|> Chart.WithHeight 500
|> Chart.Show

