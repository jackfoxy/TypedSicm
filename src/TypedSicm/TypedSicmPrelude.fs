[<AutoOpen>]
module TypedSicm.TypedSicmPrelude

open MathNet.Numerics
open GenericArithmetic

module Vector = List
    
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

let wrapScalarOrFunction (f :  Scalar -> Indexable) =
    fun (y : float) -> 
        match f (Float y) with
        | Scalar s ->
            scalarToFloat s
        | Indexable.Func func -> 
            func.Invoke (Float y) |> scalarToFloat

let wrapFloatFunction (f : (float -> float)) =
    fun (y : Scalar) -> 
        match y with
        | Int y ->  f (float y) |> Scalar.Float
        | Int64 y -> f (float  y)  |> Scalar.Float
        | BigInt y -> f (float  y) |> Scalar.Float
        | Float y -> f y |> Scalar.Float 

// to do: look at http://diffsharp.github.io/DiffSharp/index.html
let derivative (f : (Scalar -> Scalar)) =
    Differentiate.firstDerivativeFunc (wrapScalarFunction f) 
    |> wrapFloatFunction

//let definiteIntegral (f : (Scalar -> Scalar)) start finish =
let definiteIntegral (f : Scalar -> Indexable) start finish =
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

let pi = Scalar.Float System.Math.PI

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
