module App

open Fable.Core.JsInterop
open TypedSicm
open Graphing
open HTML

let xMin = 0.0
let xMax = 1.572

let layout = createObj [
    "xaxis" ==> 
        createObj [ 
                    "range" ==>  struct(xMin, xMax) 
                    "tickmode" ==> "array"
                    "tickvals" ==> [|0.785; 1.571|]
                    "ticktext" ==> [|"&#x3c4;/8"; "τ/4"|]
        ]
    "yaxis" ==> createObj [ "range" ==>  struct(0., 1.1) ]
    "title" ==>  "Evolution of Paths Using Nelder-Mead Simplex"
    "font" ==> 
        createObj [ 
                    "family" ==> "Noto Sans"
                    "size" ==> 18
        ]
    "height" ==> 850
    ]

let allPaths = Excercise1_5Data.xs

let mutable state = 0

let callPlot () =
    status.innerText <- 
        if state = 0 then
            "initial path"
        elif state =  allPaths.Length - 1 then
            "final path"
        else
            sprintf "path %i of %i" (state + 1) allPaths.Length

    let x0, y0 =
        allPaths.[state]
        |> Array.unzip

    if state > 0 then
        let x1, y1 =
            allPaths.[state - 1]
            |> Array.unzip
        if state > 1 then
            let x2, y2 =
                allPaths.[state - 2]
                |> Array.unzip
            plot !^ graph [|x0; x1; x2|] [|y0; y1; y2|] layout state |> ignore
        else
            plot !^ graph [|x0; x1|] [|y0; y1|] layout state |> ignore
    else
        plot !^ graph [|x0|] [|y0|] layout state |> ignore

resetButton.onclick <- (fun _ -> 
    state <- 0
    callPlot ()
)

forwardButton.onclick <- (fun _ -> 
    if state < allPaths.Length - 1 then
        state <- state + 1
    callPlot ()
)

backButton.onclick <- (fun _ -> 
    if state > 0 then
        state <- state - 1
    callPlot ()
)

callPlot ()
