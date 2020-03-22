module App

open Fable.Core.JsInterop
open TypedSicm
open Graphing
open HTML

let obj x = x ** 2.0

let xMin = 0.0
let xMax = 1.6

let layout = createObj [
      "xaxis" ==> createObj [ "range" ==>  struct(xMin, xMax) ]
      "yaxis" ==> createObj [ "range" ==>  struct(0., 1.1) ]
      "title", "Evolution of Paths" :> obj
    ]

let x = seq { xMin..xMax } |> Array.ofSeq
let y = x |> Array.map obj

//let allPaths = Excercise1_5.getPaths

//let x, y =
//    allPaths.[0]
//    |> Array.unzip

plot !^ graph x y layout |> ignore

let rnd = System.Random()

//let getStartingPoints () = 
//    [|0;1;2|]
//    |> Array.map (fun x -> 
//        let x = rnd.Next(int(xMin), int(xMax)) |> float
//        Point([x;])
//    )

//let mutable simplex = Simplex(getStartingPoints(), (fun p -> obj p.data.[0]))

//let restyle (simplex:Simplex) = 
//  let xs = simplex.points |> Array.map (fun p -> p.data.[0]) |> (fun x -> Array.append x x)
//  let ys = simplex.points |> Array.map (fun p -> obj p.data.[0]) |> (fun x -> Array.append x x)
//  restyle !^ graph x y xs ys layout

initButton.onclick <- (fun _ -> 
    (*
  let points = getStartingPoints()
  simplex <- Simplex(points, (fun p -> obj p.data.[0]))
  restyle simplex
  *)
  let obj x = x ** 2.5
  let x = seq { xMin..xMax } |> Array.ofSeq
  let y = x |> Array.map obj
  plot !^ graph x y layout
  initButton.innerText <- "asdfbah"
)

//stepButton.onclick <- (fun _ -> 

//  // console.log(simplex.points |> Array.map(fun x -> x.data |> Array.ofList))
//  simplex <- simplex.compute()
//  // console.log(simplex.points |> Array.map(fun x -> x.data |> Array.ofList))
//  restyle simplex
//)


