[<AutoOpen>]
module TypedSicm.TypedSicmPrelude

open MathNet.Numerics
open GenericArithmetic
open DiffSharp.Numerical.Float64

module Vector = List
    
let scalarToFloat scalar =
     match scalar with
     | Int x -> float x
     | Int64 x -> float x
     | BigInt x -> float x
     | Float x -> x

let inline floatToTime t : Time = Real.Float t

let wrapScalarFunction (f : (Real -> Real)) =
    fun (y : float) -> 
        f (Float y)
        |> scalarToFloat

let wrapScalarOrFunction (f :  Real -> Indexable) =
    fun (y : float) -> 
        match f (Float y) with
        | Scalar s ->
            scalarToFloat s
        | Indexable.Func func -> 
            func.Invoke (Float y) |> scalarToFloat

let wrapFloatFunction (f : (float -> float)) =
    fun (y : Real) -> 
        match y with
        | Int y ->  f (float y) |> Real.Float
        | Int64 y -> f (float  y)  |> Real.Float
        | BigInt y -> f (float  y) |> Real.Float
        | Float y -> f y |> Real.Float 

let derivative (f : (Real -> Real)) =
    diff (wrapScalarFunction f) 
    |> wrapFloatFunction

let definiteIntegral (f : Real -> Indexable) start finish =
    let f' : System.Func<float, float> = System.Func<float, float>(wrapScalarOrFunction f)
    Integrate.OnClosedInterval(f', scalarToFloat start, scalarToFloat finish)

let scalarOrFuncToScalar x =
    match x with
    | Indexable.Scalar s -> s
    | _ -> invalidArg "scalarOrFuncToScalar" "should not get here"

let indexableFunc f =
    f |> ScalarFunc |> Indexable.Func

let inline squareVector vector =
    vector
    |> Vector.map (fun x -> x * x)
    |> Vector.reduce (+)

let pi = Real.Float System.Math.PI

type List<'T> with
    static member inline Add (xs : list<'T1>, ys : list<'T1>) =
        if xs.Length = ys.Length  then
            if xs.Length = 0 then
                xs
            else
                List.zip xs ys
                |> List.map (fun (x, y) -> x + y)
        else
            invalidArg "zip" "length of list not the same"

    static member inline Add (xs : list<'T1>, x : 'T1) =
        if xs.Length = 0 then
            xs
        else
           xs
           |> List.map (fun y -> x + y)

    static member inline Multiply (xs : list<'T1>, ys : list<'T1>) = 
        if xs.Length = ys.Length  then
            if xs.Length = 0 then
                xs
            else
               List.zip xs ys
               |> List.map (fun (x, y) -> x * y)
        else
            invalidArg "zip" "length of list not the same"

    static member inline Multiply (xs : list<'T1>, x : 'T1) = 
        if xs.Length = 0 then
            xs
        else
           xs
           |> List.map (fun y -> x * y)

    static member inline Subtract (xs : list<'T1>, ys : list<'T1>) = 
        if xs.Length = ys.Length  then
            if xs.Length = 0 then
                xs
            else
                List.zip xs ys
                |> List.map (fun (x, y) -> x - y)
        else
         invalidArg "zip" "length of list not the same"
