[<AutoOpen>]
module TypedSicm.TypedSicmPrelude

#if INTERACTIVE
    #r @"E:\GitRepos\TypedSicm\packages\MathNet.Numerics\lib\netstandard2.0\MathNet.Numerics.dll"
    #r @"E:\GitRepos\TypedSicm\packages\MathNet.Numerics.FSharp\lib\netstandard2.0\MathNet.Numerics.FSharp.dll"
#endif

open System
open System.Numerics
open MathNet.Numerics

type Scalar =
    | Int of int
    | Int64 of int64
    | BigInt of BigInteger
    | Float of float
    | Complex of Complex
   // | Quaternion of Quaternion
    | Func1 of (Scalar -> Scalar)
   with 

    static member (+) (x, y) = 
        match x, y with
        | Int x, Int y -> Int (x + y)
        | Int x, Int64 y -> Int64 (int64 x + y)
        | Int x, BigInt y -> BigInt (BigInteger x + y)
        | Int x, Float y -> Float (float x + y)
        | Int x, Complex y -> Complex (new Complex(float x, 0.) + y)
        | Int x, Func1 y -> Func1 (fun z -> (y z) + Int x)

        | Int64 x, Int y -> Int64 (x + int64 y)
        | Int64 x, Int64 y -> Int64 (x + y)
        | Int64 x, BigInt y -> BigInt (BigInteger x + y)
        | Int64 x, Float y -> Float (float x + y)
        | Int64 x, Complex y -> Complex (new Complex(float x, 0.) + y)
        | Int64 x, Func1 y -> Func1 (fun z -> (y z) + Int64 x)

        | BigInt x, Int y -> BigInt (x + BigInteger y)
        | BigInt x, Int64 y -> BigInt (x + BigInteger y)
        | BigInt x, BigInt y -> BigInt (x + y)
        | BigInt x, Float y -> Float (float x + y)
        | BigInt x, Complex y -> Complex (new Complex(float x, 0.) + y)
        | BigInt x, Func1 y -> Func1 (fun z -> (y z) + BigInt x)

        | Float x, Int y -> Float (x + float y)
        | Float x, Int64 y -> Float (x + float y)
        | Float x, BigInt y -> Float (x + float y)
        | Float x, Float y -> Float (x + y)
        | Float x, Complex y -> Complex (new Complex(x, 0.) + y)
        | Float x, Func1 y -> Func1 (fun z -> (y z) + Float x)

        | Complex x, Int y -> Complex (x + new Complex(float y, 0.))
        | Complex x, Int64 y -> Complex (x + new Complex(float y, 0.))
        | Complex x, BigInt y -> Complex (x + new Complex(float y, 0.))
        | Complex x, Float y -> Complex (x + new Complex(float y, 0.))
        | Complex x, Complex y -> Complex (x + y)
        | Complex x, Func1 y -> Func1 (fun z -> (y z) + Complex x)

        | Func1 x, Int _ 
        | Func1 x, Int64 _ 
        | Func1 x, BigInt _
        | Func1 x, Float _
        | Func1 x, Complex _ -> Func1 (x >> (+) y)          
        | Func1 x, Func1 y -> Func1 (fun z -> (x z) + (y z))

    static member (*) (x, y) = 
        match x, y with
        | Int x, Int y -> Int (x * y)
        | Int x, Int64 y -> Int64 (int64 x * y)
        | Int x, BigInt y -> BigInt (BigInteger x * y)
        | Int x, Float y -> Float (float x * y)
        | Int x, Complex y -> Complex (new Complex(float x, 0.) * y)
        | Int x, Func1 y -> Func1 (fun z -> (y z) * Int x)

        | Int64 x, Int y -> Int64 (x * int64 y)
        | Int64 x, Int64 y -> Int64 (x * y)
        | Int64 x, BigInt y -> BigInt (BigInteger x * y)
        | Int64 x, Float y -> Float (float x * y)
        | Int64 x, Complex y -> Complex (new Complex(float x, 0.) * y)
        | Int64 x, Func1 y -> Func1 (fun z -> (y z) * Int64 x)

        | BigInt x, Int y -> BigInt (x * BigInteger y)
        | BigInt x, Int64 y -> BigInt (x * BigInteger y)
        | BigInt x, BigInt y -> BigInt (x * y)
        | BigInt x, Float y -> Float (float x * y)
        | BigInt x, Complex y -> Complex (new Complex(float x, 0.) * y)
        | BigInt x, Func1 y -> Func1 (fun z -> (y z) * BigInt x)

        | Float x, Int y -> Float (x * float y)
        | Float x, Int64 y -> Float (x * float y)
        | Float x, BigInt y -> Float (x * float y)
        | Float x, Float y -> Float (x * y)
        | Float x, Complex y -> Complex (new Complex(x, 0.) * y)
        | Float x, Func1 y -> Func1 (fun z -> (y z) * Float x)

        | Complex x, Int y -> Complex (x * new Complex(float y, 0.))
        | Complex x, Int64 y -> Complex (x * new Complex(float y, 0.))
        | Complex x, BigInt y -> Complex (x * new Complex(float y, 0.))
        | Complex x, Float y -> Complex (x * new Complex(float y, 0.))
        | Complex x, Complex y -> Complex (x * y)
        | Complex x, Func1 y -> Func1 (fun z -> (y z) * Complex x)

        | Func1 x, Int _ 
        | Func1 x, Int64 _ 
        | Func1 x, BigInt _
        | Func1 x, Float _
        | Func1 x, Complex _ -> Func1 (x >> (*) y)          
        | Func1 x, Func1 y -> Func1 (fun z -> (x z) * (y z))

    static member (-) (x, y) = 
        match x, y with
        | Int x, Int y -> Int (x - y)
        | Int x, Int64 y -> Int64 (int64 x - y)
        | Int x, BigInt y -> BigInt (BigInteger x - y)
        | Int x, Float y -> Float (float x - y)
        | Int x, Complex y -> Complex (new Complex(float x, 0.) - y)
        | Int x, Func1 y -> Func1 (fun z -> (y z) - Int x)

        | Int64 x, Int y -> Int64 (x - int64 y)
        | Int64 x, Int64 y -> Int64 (x - y)
        | Int64 x, BigInt y -> BigInt (BigInteger x - y)
        | Int64 x, Float y -> Float (float x - y)
        | Int64 x, Complex y -> Complex (new Complex(float x, 0.) - y)
        | Int64 x, Func1 y -> Func1 (fun z -> (y z) - Int64 x)

        | BigInt x, Int y -> BigInt (x - BigInteger y)
        | BigInt x, Int64 y -> BigInt (x - BigInteger y)
        | BigInt x, BigInt y -> BigInt (x - y)
        | BigInt x, Float y -> Float (float x - y)
        | BigInt x, Complex y -> Complex (new Complex(float x, 0.) - y)
        | BigInt x, Func1 y -> Func1 (fun z -> (y z) - BigInt x)

        | Float x, Int y -> Float (x - float y)
        | Float x, Int64 y -> Float (x - float y)
        | Float x, BigInt y -> Float (x - float y)
        | Float x, Float y -> Float (x - y)
        | Float x, Complex y -> Complex (new Complex(x, 0.) - y)
        | Float x, Func1 y -> Func1 (fun z -> (y z) - Float x)

        | Complex x, Int y -> Complex (x - new Complex(float y, 0.))
        | Complex x, Int64 y -> Complex (x - new Complex(float y, 0.))
        | Complex x, BigInt y -> Complex (x - new Complex(float y, 0.))
        | Complex x, Float y -> Complex (x - new Complex(float y, 0.))
        | Complex x, Complex y -> Complex (x - y)
        | Complex x, Func1 y -> Func1 (fun z -> (y z) - Complex x)

        | Func1 x, Int _ 
        | Func1 x, Int64 _ 
        | Func1 x, BigInt _
        | Func1 x, Float _
        | Func1 x, Complex _ -> Func1 (fun z -> (x z) - y)       
        | Func1 x, Func1 y -> Func1 (fun z -> (x z) - (y z))

    static member (/) (x, y) = 
        match x, y with
        | Int x, Int y -> Int (x / y)
        | Int x, Int64 y -> Int64 (int64 x / y)
        | Int x, BigInt y -> BigInt (BigInteger x / y)
        | Int x, Float y -> Float (float x / y)
        | Int x, Complex y -> Complex (new Complex(float x, 0.) / y)
        | Int x, Func1 y -> Func1 (fun z -> (y z) / Int x)

        | Int64 x, Int y -> Int64 (x / int64 y)
        | Int64 x, Int64 y -> Int64 (x / y)
        | Int64 x, BigInt y -> BigInt (BigInteger x / y)
        | Int64 x, Float y -> Float (float x / y)
        | Int64 x, Complex y -> Complex (new Complex(float x, 0.) / y)
        | Int64 x, Func1 y -> Func1 (fun z -> (y z) / Int64 x)

        | BigInt x, Int y -> BigInt (x / BigInteger y)
        | BigInt x, Int64 y -> BigInt (x / BigInteger y)
        | BigInt x, BigInt y -> BigInt (x / y)
        | BigInt x, Float y -> Float (float x / y)
        | BigInt x, Complex y -> Complex (new Complex(float x, 0.) / y)
        | BigInt x, Func1 y -> Func1 (fun z -> (y z) / BigInt x)

        | Float x, Int y -> Float (x / float y)
        | Float x, Int64 y -> Float (x / float y)
        | Float x, BigInt y -> Float (x / float y)
        | Float x, Float y -> Float (x / y)
        | Float x, Complex y -> Complex (new Complex(x, 0.) / y)
        | Float x, Func1 y -> Func1 (fun z -> (y z) / Float x)

        | Complex x, Int y -> Complex (x / new Complex(float y, 0.))
        | Complex x, Int64 y -> Complex (x / new Complex(float y, 0.))
        | Complex x, BigInt y -> Complex (x / new Complex(float y, 0.))
        | Complex x, Float y -> Complex (x / new Complex(float y, 0.))
        | Complex x, Complex y -> Complex (x / y)
        | Complex x, Func1 y -> Func1 (fun z -> (y z) / Complex x)

        | Func1 x, Int _ 
        | Func1 x, Int64 _ 
        | Func1 x, BigInt _
        | Func1 x, Float _
        | Func1 x, Complex _ -> Func1 (fun z -> (x z) / y)            
        | Func1 x, Func1 y -> Func1 (fun z -> (x z) / (y z))

    static member (+) (x, (y : int)) = 
        match x with
        | Int x -> Int (x + y)
        | Int64 x -> Int64 (x + int64 y)
        | BigInt x -> BigInt (x + BigInteger y)
        | Float x -> Float (x + float y)
        | Complex x -> Complex (x + new Complex(float y, 0.))
        | Func1 x -> Func1 (fun z -> (x z) + y)
        
    static member (+) ((x : int), y) = 
        match y with
        | Int y -> Int (x + y)
        | Int64 y -> Int64 (int64 x + y)
        | BigInt y -> BigInt (BigInteger x + y)
        | Float y -> Float (float x + y)
        | Complex y -> Complex (new Complex(float x, 0.) + y)
        | Func1 y -> Func1 (fun z -> (y z) + x)

    static member (*) (x, (y : int)) = 
        match x with
        | Int x -> Int (x * y)
        | Int64 x -> Int64 (x * int64 y)
        | BigInt x -> BigInt (x * BigInteger y)
        | Float x -> Float (x * float y)
        | Complex x -> Complex (x * new Complex(float y, 0.))
        | Func1 x -> Func1 (fun z -> (x z) * y)
        
    static member (*) ((x : int), y) = 
        match y with
        | Int y -> Int (x * y)
        | Int64 y -> Int64 (int64 x * y)
        | BigInt y -> BigInt (BigInteger x * y)
        | Float y -> Float (float x * y)
        | Complex y -> Complex (new Complex(float x, 0.) * y)
        | Func1 y -> Func1 (fun z -> (y z) * x)

    static member (-) (x, (y : int)) = 
        match x with
        | Int x -> Int (x - y)
        | Int64 x -> Int64 (x - int64 y)
        | BigInt x -> BigInt (x - BigInteger y)
        | Float x -> Float (x - float y)
        | Complex x -> Complex (x - new Complex(float y, 0.))
        | Func1 x -> Func1 (fun z -> (x z) - y)
        
    static member (-) ((x : int), y) = 
        match y with
        | Int y -> Int (x - y)
        | Int64 y -> Int64 (int64 x - y)
        | BigInt y -> BigInt (BigInteger x - y)
        | Float y -> Float (float x - y)
        | Complex y -> Complex (new Complex(float x, 0.) - y)
        | Func1 y -> Func1 (fun z -> x - (y z))

    static member (/) (x, (y : int)) = 
        match x with
        | Int x -> Int (x / y)
        | Int64 x -> Int64 (x / int64 y)
        | BigInt x -> BigInt (x / BigInteger y)
        | Float x -> Float (x / float y)
        | Complex x -> Complex (x / new Complex(float y, 0.))
        | Func1 x -> Func1 (fun z -> (x z) / y)
        
    static member (/) ((x : int), y) = 
        match y with
        | Int y -> Int (x / y)
        | Int64 y -> Int64 (int64 x / y)
        | BigInt y -> BigInt (BigInteger x / y)
        | Float y -> Float (float x / y)
        | Complex y -> Complex (new Complex(float x, 0.) / y)
        | Func1 y -> Func1 (fun z -> x / (y z))

    static member (+) (y : Scalar, (xs : UpIndexed)) : UpIndexed = 
        xs
        |> Array.map (fun x -> 
            match y + (Scalar.Func1 x) with
            | Scalar.Func1 x' -> x'
            | _ -> invalidArg "" "can't get here"
        )

    static member (+) ((xs : UpIndexed), y : Scalar) : UpIndexed = 
        y + xs

    static member (*) (y : Scalar, (xs : UpIndexed)) : UpIndexed = 
        xs
        |> Array.map (fun x -> 
            match y * (Scalar.Func1 x) with
            | Scalar.Func1 x' -> x'
            | _ -> invalidArg "" "can't get here"
        )

    static member (*) ((xs : UpIndexed), y : Scalar) : UpIndexed = 
        y * xs

and Time = Scalar

and UpIndexed = (Scalar -> Scalar) []

and DownIndexed =  (Scalar -> Scalar) []
    
let scalarToFloat scalar =
     match scalar with
     | Int x -> float x
     | Int64 x -> float x
     | BigInt x -> float x
     | Float x -> x
     | Complex x -> x.Real
     | Func1 _ -> invalidArg "" ""

/// cannot coerce (+) operator on UpIndexed, UpIndexed
let addUp (ys : UpIndexed) (xs : UpIndexed) : UpIndexed = 
    xs
    |> Array.zip ys
    |> Array.map (fun (x, y) -> 
        match Scalar.Func1 x + Scalar.Func1 y with
        | Func1 z -> z
        | _ -> invalidArg "" "can't get here"  
    )

let inline floatToTime t : Time = Scalar.Float t
let inline timeToFloat t = match t with | Scalar.Float t' -> t' | _ -> invalidArg "timeToFloat" ""

let wrapScalarFunction (f : (Scalar -> Scalar)) =
    fun (y : float) -> 
        f (Float y)
        |> scalarToFloat

let wrapFloatFunction (f : (float -> float)) =
    fun (y : Scalar) -> 
        match y with
        | Int y ->  f (float y) |> Scalar.Float
        | Int64 y -> f (float  y)  |> Scalar.Float
        | BigInt y -> f (float  y) |> Scalar.Float
        | Float y -> f y |> Scalar.Float 
        | Complex y -> f y.Real |> Scalar.Float 
        | Func1 _ -> invalidArg "" ""

// to do: look at http://diffsharp.github.io/DiffSharp/index.html
let derivitave (f : (Scalar -> Scalar)) =
    Differentiate.firstDerivativeFunc (wrapScalarFunction f) 
    |> wrapFloatFunction

let definiteIntegral (f : (Scalar -> Scalar)) start finish =
    let f' : System.Func<float, float> = System.Func<float, float>(wrapScalarFunction f)
    Integrate.OnClosedInterval(f', start, finish)
