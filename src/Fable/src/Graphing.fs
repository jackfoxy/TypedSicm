namespace TypedSicm

open Fable.Core
open Fable.Core.JsInterop

module Graphing =
  
  [<Emit("Plotly")>]
  let plotly : Plotly.Index.IExports = jsNative 
    

  let plot id x y layout = 
    plotly.newPlot(
      id,
      [ 
        createObj [ 
            "x" ==> x; 
            "y" ==> y; 
            "line" ==> 
                createObj [ 
                    "simplify" ==> false 
                    "color" ==> "rgb(128, 0, 128)"
                    "width" ==> 8
                ] 
        ] 
      ] |> ResizeArray, 
      Some(layout))

  let restyle id x1 y1 x2 y2 layout =
    plotly.newPlot(
      id,
      [ 
        createObj [ "x" ==> x1; "y" ==> y1; "line" ==> createObj [ "simplify" ==> false ] ] 
        createObj [ "x" ==> x2; "y" ==> y2; "lines+markers" ==> createObj [ "simplify" ==> false ] ] 
      ] |> ResizeArray, 
      Some(layout))
        