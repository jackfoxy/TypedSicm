#r "E:/GitRepos/TypedSicm/packages/MathNet.Numerics/lib/netstandard2.0/MathNet.Numerics.dll"
#r "E:/GitRepos/TypedSicm/packages/MathNet.Numerics.FSharp/lib/netstandard2.0/MathNet.Numerics.FSharp.dll"

open System
open System.Numerics
open MathNet.Numerics

//[<DefaultAugmentation(false)>]
//[<StructuralEquality>]
//[<StructuralComparison>]


//module list =
//    let inline (*) (__, y) =
//        __
//        |> List.map (fun x -> (*) x)

type Scalar =
    | Int of int
    | Int64 of int64
    | BigInt of BigInteger
    | Float of float
    | Complex of Complex
    //| Quaternion of Quaternion
    | Func1 of (Scalar -> Scalar)
     //   | UpIndexed of UpIndexed
       with 
    //static member list =

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

        //| _,  UpIndexed y -> 
        //    let x' = UpIndexed.Scalar x
        //    let aqsdf = x' * y
        //    aqsdf

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

    //static member (*) ((x : UpIndexed), y) = 
    //    match y with
    //    | Int y -> Int (x * y)
    //    | Int64 y -> Int64 (int64 x * y)
    //    | BigInt y -> BigInt (BigInteger x * y)
    //    | Float y -> Float (float x * y)
    //    | Complex y -> Complex (new Complex(float x, 0.) * y)
    //    | Func1 y -> Func1 (fun z -> (y z) * x)
       //     | UpIndexed y -> UpIndexed y
            //match y with
            //| UpIndexed.UpIndexed ys -> 
            //    ys
            //    |> List.map (fun y' -> 
            //        match y' with
            //        | UpIndexed.Scalar y'' ->
            //            x * y''
            //    )


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

//with
//    static member (+) (y, (x : UpIndexed)) = 
//        match y with
//        | Scalar ys -> 
//            ys
//            |> List.map (fun y' -> 
//                match y' with
//                | Scalar.Float y'' ->
//                    x + y''
//            )
//    static member (*) ((x : UpIndexed), y) = 
//        match y with
//        | Scalar ys -> 
//            ys
//            |> List.map (fun y' -> 
//                match y' with
//                | Scalar.Float y'' ->
//                    x * y''
//            )

type Time = Scalar

type UpIndexed = (Scalar -> Scalar) []

type DownIndexed =  (Scalar -> Scalar) []

type Local =
    {
        Time : Time      
        CoordninatePath : (Time -> Scalar) []
        Derivatives : UpIndexed []
    }

let localMetricToFloat localMetric =
 match localMetric with
 | Int x -> float x
 | Int64 x -> float x
 | BigInt x -> float x
 | Float x -> x
 | Complex x -> x.Real
 | Scalar.Func1 x -> 0.

//let inline (*) (x : Scalar)  (y : _) = x * y  //this works

let scalarType = typeof<Scalar>
let intType = typeof<int>
let upIndexedType = typeof<UpIndexed>
//let upDownIndexedType = typeof<DownIndexed>

// let upIndexedScalarMult x y =
//     let x' = (unbox x :> UpIndexed)
//     let y' = (unbox y :> Scalar)
//     y' * x'
//     |> List.map (fun x -> 
//         let x = unbox x
//         UpIndexed.Scalar x |> box)

// let upScalarScalarMult x y =
//     let x' = (unbox x :> UpIndexed)
//     let y' = (unbox y :> Scalar)
//     y' * x'
//     |> List.map (fun x -> 
//         let x = unbox x
//         UpIndexed.Scalar x |> box)

// let upScalarIntDiv x y =
//     let x' = (unbox x :> Scalar)
//     let y' = (unbox y :> int)
//     x' / (Scalar.Int y')
//     |> box

// let inline (*) (x : _) (y : _) = 
//     let xType = x.GetType()
//     let yType = y.GetType()

//     match xType, yType with
//     | _ when xType = upIndexedType && yType = scalarType ->
//         upIndexedScalarMult x y
//         |> List.map (fun x -> unbox x)

//     | _ when xType = scalarType && yType = scalarType ->
//         upScalarScalarMult x y
//         |> List.map (fun x -> unbox x)

//     | _ ->
//         invalidArg "" ""

//let inline (/) (x : _) (y : _) = 
//    let xType = x.GetType()
//    let yType = y.GetType()

//    match xType, yType with
//    | _ when xType = scalarType && yType = intType ->
//        upScalarIntDiv x y
//        |> unbox

//(x :'t when 't : (static member (*) : 't -> 't -> 't))
//let inline (*) (x : ^t when ^t : (member Func1 : (^t  -> ^t)) and ^t : ^t :> Scalar ) y  = 
//    match x, y with
//    | (:? Scalar as scalar), (:? UpIndexed as upIndexed) ->
//        scalar * UpIndexed


let inline floatToTime f : Time = Scalar.Float f


let wrapFunc1 (f : (Scalar -> Scalar)) =
    (fun (y : float) -> 
        match (f (Float y)) with
        | Int z -> float z
        | Int64 z -> float z
        | BigInt z -> float z
        | Float z -> z
        | Complex z -> z.Real
        | Scalar.Func1 _ -> 0. ) 

let wrapFloatFunction (f : (float -> float)) =
    fun (y : Scalar) -> 
        match y with
        | Int y ->  f (float y) |> Scalar.Float
        | Int64 y -> f (float  y)  |> Scalar.Float
        | BigInt y -> f (float  y) |> Scalar.Float
        | Float y -> f y |> Scalar.Float 
        | Complex y -> f y.Real |> Scalar.Float 

//let ffToFuncUp (f : float -> float) =
//    (fun f' ->
//        match f' with
//        | Scalar.Float n -> (f n) |> Scalar.Float 
//        | _ -> invalidArg "" "" )
//        |> UpIndexed.Func1

let firstDerivative (state : Local) =
    Array.zip state.Derivatives.[0] state.CoordninatePath
    |> Array.map (fun (f, p') -> 
        p' state.Time
        |> f
    )


// to do: look at http://diffsharp.github.io/DiffSharp/index.html
let derivitave (f : (Scalar -> Scalar)) =
    Differentiate.firstDerivativeFunc (wrapFunc1 f) 
    |> wrapFloatFunction

let definiteIntegral (f : (Scalar -> Scalar)) start finish =
    let f' : System.Func<float, float> = System.Func<float, float>(wrapFunc1 f)
    Integrate.OnClosedInterval(f', start, finish)

