[<AutoOpen>]
module TypedSicm.TypedSicmPrelude

open MathNet.Numerics
open FSharpx.Collections
open GenericArithmetic

module Vector = RandomAccessList
    
let scalarToFloat scalar =
     match scalar with
     | Int x -> float x
     | Int64 x -> float x
     | BigInt x -> float x
     | Float x -> x

let inline floatToTime t : Time = Scalar.Float t

let wrapScalarFunction (f : (Scalar -> Scalar)) =
    fun (y : float) -> 
        f (Float y)
        |> scalarToFloat

let wrapScalarOrFunction (f :  Scalar -> ScalarOrFunc) =
    fun (y : float) -> 
        match f (Float y) with
        | Scalar s ->
            scalarToFloat s
        | _ ->
                invalidArg "wrapScalarFunction" "klunky, fix later"

let wrapFloatFunction (f : (float -> float)) =
    fun (y : Scalar) -> 
        match y with
        | Int y ->  f (float y) |> Scalar.Float
        | Int64 y -> f (float  y)  |> Scalar.Float
        | BigInt y -> f (float  y) |> Scalar.Float
        | Float y -> f y |> Scalar.Float 

// to do: look at http://diffsharp.github.io/DiffSharp/index.html
let derivitave (f : (Scalar -> Scalar)) =
    Differentiate.firstDerivativeFunc (wrapScalarFunction f) 
    |> wrapFloatFunction

//let definiteIntegral (f : (Scalar -> Scalar)) start finish =
let definiteIntegral (f : Scalar -> ScalarOrFunc) start finish =
    let f' : System.Func<float, float> = System.Func<float, float>(wrapScalarOrFunction f)
    Integrate.OnClosedInterval(f', scalarToFloat start, scalarToFloat finish)

let vectorConj x (xs : UpIndexed) =
    Vector.rev xs
    |> (Vector.cons x)
    |> Vector.rev

let scalarOrFuncToScalar x =
    match x with
    | ScalarOrFunc.Scalar s -> s
    | _ -> invalidArg "scalarOrFuncToScalar" "should not get here"

let indexableFunc f =
    f |> ScalarFunc |> ScalarOrFunc.Func
