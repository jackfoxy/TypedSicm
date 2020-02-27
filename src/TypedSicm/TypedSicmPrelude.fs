[<AutoOpen>]
module TypedSicm.TypedSicmPrelude

#if INTERACTIVE
    #r @"E:\GitRepos\TypedSicm\packages\MathNet.Numerics\lib\netstandard2.0\MathNet.Numerics.dll"
    #r @"E:\GitRepos\TypedSicm\packages\MathNet.Numerics.FSharp\lib\netstandard2.0\MathNet.Numerics.FSharp.dll"
#endif

open System
open System.Numerics
open MathNet.Numerics
open FSharpx.Collections

module Vector = RandomAccessList

type Scalar =
    | Int of int
    | Int64 of int64
    | BigInt of BigInteger
    | Float of float
    | Complex of Complex
   // | Quaternion of Quaternion

   with 

    static member (+) (x, y) = 
        match x, y with
        | Int x, Int y -> Int (x + y)
        | Int x, Int64 y -> Int64 (int64 x + y)
        | Int x, BigInt y -> BigInt (BigInteger x + y)
        | Int x, Float y -> Float (float x + y)
        | Int x, Complex y -> Complex (new Complex(float x, 0.) + y)

        | Int64 x, Int y -> Int64 (x + int64 y)
        | Int64 x, Int64 y -> Int64 (x + y)
        | Int64 x, BigInt y -> BigInt (BigInteger x + y)
        | Int64 x, Float y -> Float (float x + y)
        | Int64 x, Complex y -> Complex (new Complex(float x, 0.) + y)

        | BigInt x, Int y -> BigInt (x + BigInteger y)
        | BigInt x, Int64 y -> BigInt (x + BigInteger y)
        | BigInt x, BigInt y -> BigInt (x + y)
        | BigInt x, Float y -> Float (float x + y)
        | BigInt x, Complex y -> Complex (new Complex(float x, 0.) + y)

        | Float x, Int y -> Float (x + float y)
        | Float x, Int64 y -> Float (x + float y)
        | Float x, BigInt y -> Float (x + float y)
        | Float x, Float y -> Float (x + y)
        | Float x, Complex y -> Complex (new Complex(x, 0.) + y)

        | Complex x, Int y -> Complex (x + new Complex(float y, 0.))
        | Complex x, Int64 y -> Complex (x + new Complex(float y, 0.))
        | Complex x, BigInt y -> Complex (x + new Complex(float y, 0.))
        | Complex x, Float y -> Complex (x + new Complex(float y, 0.))
        | Complex x, Complex y -> Complex (x + y)

    static member (*) (x, y) = 
        match x, y with
        | Int x, Int y -> Int (x * y)
        | Int x, Int64 y -> Int64 (int64 x * y)
        | Int x, BigInt y -> BigInt (BigInteger x * y)
        | Int x, Float y -> Float (float x * y)
        | Int x, Complex y -> Complex (new Complex(float x, 0.) * y)

        | Int64 x, Int y -> Int64 (x * int64 y)
        | Int64 x, Int64 y -> Int64 (x * y)
        | Int64 x, BigInt y -> BigInt (BigInteger x * y)
        | Int64 x, Float y -> Float (float x * y)
        | Int64 x, Complex y -> Complex (new Complex(float x, 0.) * y)

        | BigInt x, Int y -> BigInt (x * BigInteger y)
        | BigInt x, Int64 y -> BigInt (x * BigInteger y)
        | BigInt x, BigInt y -> BigInt (x * y)
        | BigInt x, Float y -> Float (float x * y)
        | BigInt x, Complex y -> Complex (new Complex(float x, 0.) * y)

        | Float x, Int y -> Float (x * float y)
        | Float x, Int64 y -> Float (x * float y)
        | Float x, BigInt y -> Float (x * float y)
        | Float x, Float y -> Float (x * y)
        | Float x, Complex y -> Complex (new Complex(x, 0.) * y)

        | Complex x, Int y -> Complex (x * new Complex(float y, 0.))
        | Complex x, Int64 y -> Complex (x * new Complex(float y, 0.))
        | Complex x, BigInt y -> Complex (x * new Complex(float y, 0.))
        | Complex x, Float y -> Complex (x * new Complex(float y, 0.))
        | Complex x, Complex y -> Complex (x * y)

    static member (-) (x, y) = 
        match x, y with
        | Int x, Int y -> Int (x - y)
        | Int x, Int64 y -> Int64 (int64 x - y)
        | Int x, BigInt y -> BigInt (BigInteger x - y)
        | Int x, Float y -> Float (float x - y)
        | Int x, Complex y -> Complex (new Complex(float x, 0.) - y)


        | Int64 x, Int y -> Int64 (x - int64 y)
        | Int64 x, Int64 y -> Int64 (x - y)
        | Int64 x, BigInt y -> BigInt (BigInteger x - y)
        | Int64 x, Float y -> Float (float x - y)
        | Int64 x, Complex y -> Complex (new Complex(float x, 0.) - y)

        | BigInt x, Int y -> BigInt (x - BigInteger y)
        | BigInt x, Int64 y -> BigInt (x - BigInteger y)
        | BigInt x, BigInt y -> BigInt (x - y)
        | BigInt x, Float y -> Float (float x - y)
        | BigInt x, Complex y -> Complex (new Complex(float x, 0.) - y)

        | Float x, Int y -> Float (x - float y)
        | Float x, Int64 y -> Float (x - float y)
        | Float x, BigInt y -> Float (x - float y)
        | Float x, Float y -> Float (x - y)
        | Float x, Complex y -> Complex (new Complex(x, 0.) - y)

        | Complex x, Int y -> Complex (x - new Complex(float y, 0.))
        | Complex x, Int64 y -> Complex (x - new Complex(float y, 0.))
        | Complex x, BigInt y -> Complex (x - new Complex(float y, 0.))
        | Complex x, Float y -> Complex (x - new Complex(float y, 0.))
        | Complex x, Complex y -> Complex (x - y)

    static member (/) (x, y) = 
        match x, y with
        | Int x, Int y -> Int (x / y)
        | Int x, Int64 y -> Int64 (int64 x / y)
        | Int x, BigInt y -> BigInt (BigInteger x / y)
        | Int x, Float y -> Float (float x / y)
        | Int x, Complex y -> Complex (new Complex(float x, 0.) / y)

        | Int64 x, Int y -> Int64 (x / int64 y)
        | Int64 x, Int64 y -> Int64 (x / y)
        | Int64 x, BigInt y -> BigInt (BigInteger x / y)
        | Int64 x, Float y -> Float (float x / y)
        | Int64 x, Complex y -> Complex (new Complex(float x, 0.) / y)

        | BigInt x, Int y -> BigInt (x / BigInteger y)
        | BigInt x, Int64 y -> BigInt (x / BigInteger y)
        | BigInt x, BigInt y -> BigInt (x / y)
        | BigInt x, Float y -> Float (float x / y)
        | BigInt x, Complex y -> Complex (new Complex(float x, 0.) / y)

        | Float x, Int y -> Float (x / float y)
        | Float x, Int64 y -> Float (x / float y)
        | Float x, BigInt y -> Float (x / float y)
        | Float x, Float y -> Float (x / y)
        | Float x, Complex y -> Complex (new Complex(x, 0.) / y)

        | Complex x, Int y -> Complex (x / new Complex(float y, 0.))
        | Complex x, Int64 y -> Complex (x / new Complex(float y, 0.))
        | Complex x, BigInt y -> Complex (x / new Complex(float y, 0.))
        | Complex x, Float y -> Complex (x / new Complex(float y, 0.))
        | Complex x, Complex y -> Complex (x / y)

    static member (+) (x, (y : int)) = 
        match x with
        | Int x -> Int (x + y)
        | Int64 x -> Int64 (x + int64 y)
        | BigInt x -> BigInt (x + BigInteger y)
        | Float x -> Float (x + float y)
        | Complex x -> Complex (x + new Complex(float y, 0.))
        
    static member (+) ((x : int), y) = 
        match y with
        | Int y -> Int (x + y)
        | Int64 y -> Int64 (int64 x + y)
        | BigInt y -> BigInt (BigInteger x + y)
        | Float y -> Float (float x + y)
        | Complex y -> Complex (new Complex(float x, 0.) + y)

    static member (*) (x, (y : int)) = 
        match x with
        | Int x -> Int (x * y)
        | Int64 x -> Int64 (x * int64 y)
        | BigInt x -> BigInt (x * BigInteger y)
        | Float x -> Float (x * float y)
        | Complex x -> Complex (x * new Complex(float y, 0.))
        
    static member (*) ((x : int), y) = 
        match y with
        | Int y -> Int (x * y)
        | Int64 y -> Int64 (int64 x * y)
        | BigInt y -> BigInt (BigInteger x * y)
        | Float y -> Float (float x * y)
        | Complex y -> Complex (new Complex(float x, 0.) * y)

    static member (-) (x, (y : int)) = 
        match x with
        | Int x -> Int (x - y)
        | Int64 x -> Int64 (x - int64 y)
        | BigInt x -> BigInt (x - BigInteger y)
        | Float x -> Float (x - float y)
        | Complex x -> Complex (x - new Complex(float y, 0.))
        
    static member (-) ((x : int), y) = 
        match y with
        | Int y -> Int (x - y)
        | Int64 y -> Int64 (int64 x - y)
        | BigInt y -> BigInt (BigInteger x - y)
        | Float y -> Float (float x - y)
        | Complex y -> Complex (new Complex(float x, 0.) - y)

    static member (/) (x, (y : int)) = 
        match x with
        | Int x -> Int (x / y)
        | Int64 x -> Int64 (x / int64 y)
        | BigInt x -> BigInt (x / BigInteger y)
        | Float x -> Float (x / float y)
        | Complex x -> Complex (x / new Complex(float y, 0.))
        
    static member (/) ((x : int), y) = 
        match y with
        | Int y -> Int (x / y)
        | Int64 y -> Int64 (int64 x / y)
        | BigInt y -> BigInt (BigInteger x / y)
        | Float y -> Float (float x / y)
        | Complex y -> Complex (new Complex(float x, 0.) / y)

    static member (+) (x : Scalar, (ys : UpIndexed)) : UpIndexed = 
        ys 
        |> Vector.map (fun y -> 
            match y with
            | Scalar y' -> x + y' |> ScalarOrFunc.Scalar
            | Func y' -> x + y' |> ScalarOrFunc.Func
        )

    static member (+) ((xs : UpIndexed), y : Scalar) : UpIndexed = 
        y + xs

    static member (+) (ys : UpIndexed, xs : UpIndexed) : UpIndexed = 
        xs
        |> Vector.zip ys
        |> Vector.map (fun (x, y) ->  
            match x, y with
            | Scalar x', Scalar y' -> x' + y' |> ScalarOrFunc.Scalar
            | Func x', Scalar y' -> x' + y' |> ScalarOrFunc.Func
            | Scalar x', Func y' -> x' + y' |> ScalarOrFunc.Func
            | Func x', Func y' -> x' + y' |> ScalarOrFunc.Func
        )





    static member (*) (x : Scalar, (ys : UpIndexed)) : UpIndexed = 
        ys 
        |> Vector.map (fun y -> 
            match y with
            | Scalar y' -> x * y' |> ScalarOrFunc.Scalar
            | Func y' -> x * y' |> ScalarOrFunc.Func
        )

    static member (*) ((xs : UpIndexed), y : Scalar) : UpIndexed = 
        y * xs



    static member (*) (ys : UpIndexed, xs : UpIndexed) : UpIndexed = 
        xs
        |> Vector.zip ys
        |> Vector.map (fun (x, y) ->  
            match x, y with
            | Scalar x', Scalar y' -> x' * y' |> ScalarOrFunc.Scalar
            | Func x', Scalar y' -> x' * y' |> ScalarOrFunc.Func
            | Scalar x', Func y' -> x' * y' |> ScalarOrFunc.Func
            | Func x', Func y' -> x' * y' |> ScalarOrFunc.Func
        )

and Time = Scalar

and [<Class>] ScalarFunc(scalarFunc : (Scalar -> Scalar)) =  
    member _.Invoke = scalarFunc
    with 

    static member (+) (x, (y : ScalarFunc)) = 
        ScalarFunc (fun z -> (y.Invoke z) + x)

    static member (+) ((x : ScalarFunc), y) = 
        y + x

    static member (+) ((x: ScalarFunc),(y : ScalarFunc)) = 
        ScalarFunc (fun z -> (x.Invoke z) + (y.Invoke z))



    static member (*) (x, (y : ScalarFunc)) = 
        ScalarFunc (fun z -> (y.Invoke z) * x)

    static member (*) ((x : ScalarFunc), y) = 
        y * x

    static member (*) ((x: ScalarFunc),(y : ScalarFunc)) = 
        ScalarFunc (fun z -> (x.Invoke z) * (y.Invoke z))



    static member (/) (x, (y : ScalarFunc)) = 
        ScalarFunc (fun z -> (y.Invoke z) / x)

    static member (/) ((x : ScalarFunc), y) = 
        y / x

    static member (/) ((x: ScalarFunc),(y : ScalarFunc)) = 
        ScalarFunc (fun z -> (x.Invoke z) / (y.Invoke z))

and ScalarOrFunc =
    | Scalar of Scalar
    | Func of ScalarFunc

    with

    static member (+) ((x : Scalar), (y : ScalarOrFunc)) : ScalarOrFunc = 
        match y with
        | Scalar y' -> x + y' |> ScalarOrFunc.Scalar
        | Func y' -> 
            let w =  (fun z ->  x + (y'.Invoke z)) |> ScalarFunc
            Func w 

    static member (+) ((x : ScalarOrFunc), (y : Scalar)) : ScalarOrFunc = 
        y + x

    static member (+) ((x : ScalarOrFunc), (y : ScalarOrFunc)) : ScalarOrFunc =
        match x, y with
        | Scalar x', Scalar y' -> x' + y' |> ScalarOrFunc.Scalar
        | Func x', Scalar y' -> x' + y' |> ScalarOrFunc.Func
        | Scalar x', Func y' -> x' + y' |> ScalarOrFunc.Func
        | Func x', Func y' -> x' + y' |> ScalarOrFunc.Func




    static member (*) ((x : Scalar), (y : ScalarOrFunc)) : ScalarOrFunc = 
        match y with
        | Scalar y -> x * y |> ScalarOrFunc.Scalar
        | Func y' -> 
            let w =  (fun z -> x * (y'.Invoke z)) |> ScalarFunc
            Func w 

    static member (*) ((x : ScalarOrFunc), (y : Scalar)) : ScalarOrFunc = 
        y + x

    static member (*) ((x : ScalarOrFunc), (y : ScalarOrFunc)) : ScalarOrFunc =
        match x, y with
        | Scalar x', Scalar y' -> x' * y' |> ScalarOrFunc.Scalar
        | Func x', Scalar y' -> x' * y' |> ScalarOrFunc.Func
        | Scalar x', Func y' -> x' * y' |> ScalarOrFunc.Func
        | Func x', Func y' -> x' * y' |> ScalarOrFunc.Func



    static member (/) ((x : Scalar), (y : ScalarOrFunc)) : ScalarOrFunc = 
        match y with
        | Scalar y' -> x / y' |> ScalarOrFunc.Scalar
        | Func y' -> 
            let w =  (fun z -> x / (y'.Invoke z)) |> ScalarFunc
            Func w 

    static member (/) ((x : ScalarOrFunc), (y : Scalar)) : ScalarOrFunc = 
        match x with
        | Scalar x' -> x' / y |> ScalarOrFunc.Scalar
        | Func x' -> 
            let w =  (fun z -> (x'.Invoke z) / y) |> ScalarFunc
            Func w 

    static member (/) ((x : int), (y : ScalarOrFunc)) : ScalarOrFunc = 
        match y with
        | Scalar y' -> x / y' |> ScalarOrFunc.Scalar
        | Func y' -> 
            let w =  (fun z -> x / (y'.Invoke z)) |> ScalarFunc
            Func w 

    static member (/) ((x : ScalarOrFunc), (y : int)) : ScalarOrFunc = 
        match x with
        | Scalar x' -> x' / y |> ScalarOrFunc.Scalar
        | Func x' -> 
            let w =  (fun z -> (x'.Invoke z) / y) |> ScalarFunc
            Func w 

    static member (/) ((x : ScalarOrFunc), (y : ScalarOrFunc)) : ScalarOrFunc =
        match x, y with
        | Scalar x', Scalar y' -> x' / y' |> ScalarOrFunc.Scalar
        | Func x', Scalar y' -> x' / y' |> ScalarOrFunc.Func
        | Scalar x', Func y' -> x' / y' |> ScalarOrFunc.Func
        | Func x', Func y' -> x' / y' |> ScalarOrFunc.Func


and UpIndexed = RandomAccessList<ScalarOrFunc>

and DownIndexed =  (Scalar -> Scalar) []
    
let scalarToFloat scalar =
     match scalar with
     | Int x -> float x
     | Int64 x -> float x
     | BigInt x -> float x
     | Float x -> x
     | Complex x -> x.Real

/// cannot coerce (+) operator on UpIndexed, UpIndexed


(*


address this!!!!!

*)
let addUp (ys : UpIndexed) (xs : UpIndexed) : UpIndexed = 
    xs
    |> Vector.zip ys
    |> Vector.map (fun (x, y) -> x + y )

let inline floatToTime t : Time = Scalar.Float t

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

// to do: look at http://diffsharp.github.io/DiffSharp/index.html
let derivitave (f : (Scalar -> Scalar)) =
    Differentiate.firstDerivativeFunc (wrapScalarFunction f) 
    |> wrapFloatFunction

let definiteIntegral (f : (Scalar -> Scalar)) start finish =
    let f' : System.Func<float, float> = System.Func<float, float>(wrapScalarFunction f)
    Integrate.OnClosedInterval(f', start, finish)
