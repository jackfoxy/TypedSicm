namespace TypedSicm

open Fable.Core
open Fable.Core.JsInterop

module Graphing =
  
    [<Emit("Plotly")>]
    let plotly : Plotly.Index.IExports = jsNative 
    
    let plotLine x y (color : string) (width : int) name =
        createObj [ 
            "x" ==> x; 
            "y" ==> y; 
            "line" ==> 
                createObj [ 
                    "simplify" ==> false 
                    "color" ==> color
                    "width" ==> width
                ]
            "name" ==> name
        ]

    let plot id (xs : float [][]) (ys  : float [][]) layout state = 
        plotly.newPlot(
            id,
            [ 
                if xs.Length > 2 then
                    plotLine xs.[2] ys.[2] "rgb(0, 221, 255)" 1 "-2 gen"

                if xs.Length > 1 then
                    plotLine xs.[1] ys.[1] "rgb(0, 132, 255)" 2 "-1 gen"

                plotLine xs.[0] ys.[0] "rgb(0, 51, 255)" 3 (if state = 0 then "initial path" else sprintf "interation %i" state)
            ] |> ResizeArray, 
            Some(layout))
        