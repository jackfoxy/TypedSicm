[<AutoOpen>]
module TypedSicm.TypedSicmPrelude

open System.Numerics
open MathNet.Numerics

type LocalMetric =
    | Int of int
    | Int64 of int64
    | BigInt of BigInteger
    | Float of float
    | Complex of Complex
  //  | Quaternion of ...  //implement netstandard2.1 preview
    | Function of (LocalMetric -> LocalMetric)
   with 
    static member (+) (x, y) = 
        match x, y with
        | Int x, Int y -> Int (x + y)
        | Int x, Int64 y -> Int64 (int64 x + y)
        | Int x, BigInt y -> BigInt (BigInteger x + y)
        | Int x, Float y -> Float (float x + y)
        | Int x, Complex y -> Complex (new Complex(float x, 0.) + y)
        | Int x, Function y -> Function (fun z -> (y z) + Int x)

        | Int64 x, Int y -> Int64 (x + int64 y)
        | Int64 x, Int64 y -> Int64 (x + y)
        | Int64 x, BigInt y -> BigInt (BigInteger x + y)
        | Int64 x, Float y -> Float (float x + y)
        | Int64 x, Complex y -> Complex (new Complex(float x, 0.) + y)
        | Int64 x, Function y -> Function (fun z -> (y z) + Int64 x)

        | BigInt x, Int y -> BigInt (x + BigInteger y)
        | BigInt x, Int64 y -> BigInt (x + BigInteger y)
        | BigInt x, BigInt y -> BigInt (x + y)
        | BigInt x, Float y -> Float (float x + y)
        | BigInt x, Complex y -> Complex (new Complex(float x, 0.) + y)
        | BigInt x, Function y -> Function (fun z -> (y z) + BigInt x)

        | Float x, Int y -> Float (x + float y)
        | Float x, Int64 y -> Float (x + float y)
        | Float x, BigInt y -> Float (x + float y)
        | Float x, Float y -> Float (x + y)
        | Float x, Complex y -> Complex (new Complex(x, 0.) + y)
        | Float x, Function y -> Function (fun z -> (y z) + Float x)

        | Complex x, Int y -> Complex (x + new Complex(float y, 0.))
        | Complex x, Int64 y -> Complex (x + new Complex(float y, 0.))
        | Complex x, BigInt y -> Complex (x + new Complex(float y, 0.))
        | Complex x, Float y -> Complex (x + new Complex(float y, 0.))
        | Complex x, Complex y -> Complex (x + y)
        | Complex x, Function y -> Function (fun z -> (y z) + Complex x)

        | Function x, Int _ 
        | Function x, Int64 _ 
        | Function x, BigInt _
        | Function x, Float _
        | Function x, Complex _ -> Function (x >> (+) y)          
        | Function x, Function y -> Function (fun z -> (x z) + (y z))

    static member (*) (x, y) = 
        match x, y with
        | Int x, Int y -> Int (x * y)
        | Int x, Int64 y -> Int64 (int64 x * y)
        | Int x, BigInt y -> BigInt (BigInteger x * y)
        | Int x, Float y -> Float (float x * y)
        | Int x, Complex y -> Complex (new Complex(float x, 0.) * y)
        | Int x, Function y -> Function (fun z -> (y z) * Int x)

        | Int64 x, Int y -> Int64 (x * int64 y)
        | Int64 x, Int64 y -> Int64 (x * y)
        | Int64 x, BigInt y -> BigInt (BigInteger x * y)
        | Int64 x, Float y -> Float (float x * y)
        | Int64 x, Complex y -> Complex (new Complex(float x, 0.) * y)
        | Int64 x, Function y -> Function (fun z -> (y z) * Int64 x)

        | BigInt x, Int y -> BigInt (x * BigInteger y)
        | BigInt x, Int64 y -> BigInt (x * BigInteger y)
        | BigInt x, BigInt y -> BigInt (x * y)
        | BigInt x, Float y -> Float (float x * y)
        | BigInt x, Complex y -> Complex (new Complex(float x, 0.) * y)
        | BigInt x, Function y -> Function (fun z -> (y z) * BigInt x)

        | Float x, Int y -> Float (x * float y)
        | Float x, Int64 y -> Float (x * float y)
        | Float x, BigInt y -> Float (x * float y)
        | Float x, Float y -> Float (x * y)
        | Float x, Complex y -> Complex (new Complex(x, 0.) * y)
        | Float x, Function y -> Function (fun z -> (y z) * Float x)

        | Complex x, Int y -> Complex (x * new Complex(float y, 0.))
        | Complex x, Int64 y -> Complex (x * new Complex(float y, 0.))
        | Complex x, BigInt y -> Complex (x * new Complex(float y, 0.))
        | Complex x, Float y -> Complex (x * new Complex(float y, 0.))
        | Complex x, Complex y -> Complex (x * y)
        | Complex x, Function y -> Function (fun z -> (y z) * Complex x)

        | Function x, Int _ 
        | Function x, Int64 _ 
        | Function x, BigInt _
        | Function x, Float _
        | Function x, Complex _ -> Function (x >> (*) y)          
        | Function x, Function y -> Function (fun z -> (x z) * (y z))

    static member (-) (x, y) = 
        match x, y with
        | Int x, Int y -> Int (x - y)
        | Int x, Int64 y -> Int64 (int64 x - y)
        | Int x, BigInt y -> BigInt (BigInteger x - y)
        | Int x, Float y -> Float (float x - y)
        | Int x, Complex y -> Complex (new Complex(float x, 0.) - y)
        | Int x, Function y -> Function (fun z -> (y z) - Int x)

        | Int64 x, Int y -> Int64 (x - int64 y)
        | Int64 x, Int64 y -> Int64 (x - y)
        | Int64 x, BigInt y -> BigInt (BigInteger x - y)
        | Int64 x, Float y -> Float (float x - y)
        | Int64 x, Complex y -> Complex (new Complex(float x, 0.) - y)
        | Int64 x, Function y -> Function (fun z -> (y z) - Int64 x)

        | BigInt x, Int y -> BigInt (x - BigInteger y)
        | BigInt x, Int64 y -> BigInt (x - BigInteger y)
        | BigInt x, BigInt y -> BigInt (x - y)
        | BigInt x, Float y -> Float (float x - y)
        | BigInt x, Complex y -> Complex (new Complex(float x, 0.) - y)
        | BigInt x, Function y -> Function (fun z -> (y z) - BigInt x)

        | Float x, Int y -> Float (x - float y)
        | Float x, Int64 y -> Float (x - float y)
        | Float x, BigInt y -> Float (x - float y)
        | Float x, Float y -> Float (x - y)
        | Float x, Complex y -> Complex (new Complex(x, 0.) - y)
        | Float x, Function y -> Function (fun z -> (y z) - Float x)

        | Complex x, Int y -> Complex (x - new Complex(float y, 0.))
        | Complex x, Int64 y -> Complex (x - new Complex(float y, 0.))
        | Complex x, BigInt y -> Complex (x - new Complex(float y, 0.))
        | Complex x, Float y -> Complex (x - new Complex(float y, 0.))
        | Complex x, Complex y -> Complex (x - y)
        | Complex x, Function y -> Function (fun z -> (y z) - Complex x)

        | Function x, Int _ 
        | Function x, Int64 _ 
        | Function x, BigInt _
        | Function x, Float _
        | Function x, Complex _ -> Function (fun z -> (x z) - y)       
        | Function x, Function y -> Function (fun z -> (x z) - (y z))

    static member (/) (x, y) = 
        match x, y with
        | Int x, Int y -> Int (x / y)
        | Int x, Int64 y -> Int64 (int64 x / y)
        | Int x, BigInt y -> BigInt (BigInteger x / y)
        | Int x, Float y -> Float (float x / y)
        | Int x, Complex y -> Complex (new Complex(float x, 0.) / y)
        | Int x, Function y -> Function (fun z -> (y z) / Int x)

        | Int64 x, Int y -> Int64 (x / int64 y)
        | Int64 x, Int64 y -> Int64 (x / y)
        | Int64 x, BigInt y -> BigInt (BigInteger x / y)
        | Int64 x, Float y -> Float (float x / y)
        | Int64 x, Complex y -> Complex (new Complex(float x, 0.) / y)
        | Int64 x, Function y -> Function (fun z -> (y z) / Int64 x)

        | BigInt x, Int y -> BigInt (x / BigInteger y)
        | BigInt x, Int64 y -> BigInt (x / BigInteger y)
        | BigInt x, BigInt y -> BigInt (x / y)
        | BigInt x, Float y -> Float (float x / y)
        | BigInt x, Complex y -> Complex (new Complex(float x, 0.) / y)
        | BigInt x, Function y -> Function (fun z -> (y z) / BigInt x)

        | Float x, Int y -> Float (x / float y)
        | Float x, Int64 y -> Float (x / float y)
        | Float x, BigInt y -> Float (x / float y)
        | Float x, Float y -> Float (x / y)
        | Float x, Complex y -> Complex (new Complex(x, 0.) / y)
        | Float x, Function y -> Function (fun z -> (y z) / Float x)

        | Complex x, Int y -> Complex (x / new Complex(float y, 0.))
        | Complex x, Int64 y -> Complex (x / new Complex(float y, 0.))
        | Complex x, BigInt y -> Complex (x / new Complex(float y, 0.))
        | Complex x, Float y -> Complex (x / new Complex(float y, 0.))
        | Complex x, Complex y -> Complex (x / y)
        | Complex x, Function y -> Function (fun z -> (y z) / Complex x)

        | Function x, Int _ 
        | Function x, Int64 _ 
        | Function x, BigInt _
        | Function x, Float _
        | Function x, Complex _ -> Function (fun z -> (x z) / y)            
        | Function x, Function y -> Function (fun z -> (x z) / (y z))

    static member (+) (x, (y : int)) = 
        match x with
        | Int x -> Int (x + y)
        | Int64 x -> Int64 (x + int64 y)
        | BigInt x -> BigInt (x + BigInteger y)
        | Float x -> Float (x + float y)
        | Complex x -> Complex (x + new Complex(float y, 0.))
        | Function x -> Function (fun z -> (x z) + y)
        
    static member (+) ((x : int), y) = 
        match y with
        | Int y -> Int (x + y)
        | Int64 y -> Int64 (int64 x + y)
        | BigInt y -> BigInt (BigInteger x + y)
        | Float y -> Float (float x + y)
        | Complex y -> Complex (new Complex(float x, 0.) + y)
        | Function y -> Function (fun z -> (y z) + x)

    static member (*) (x, (y : int)) = 
        match x with
        | Int x -> Int (x * y)
        | Int64 x -> Int64 (x * int64 y)
        | BigInt x -> BigInt (x * BigInteger y)
        | Float x -> Float (x * float y)
        | Complex x -> Complex (x * new Complex(float y, 0.))
        | Function x -> Function (fun z -> (x z) * y)
        
    static member (*) ((x : int), y) = 
        match y with
        | Int y -> Int (x * y)
        | Int64 y -> Int64 (int64 x * y)
        | BigInt y -> BigInt (BigInteger x * y)
        | Float y -> Float (float x * y)
        | Complex y -> Complex (new Complex(float x, 0.) * y)
        | Function y -> Function (fun z -> (y z) * x)

    static member (-) (x, (y : int)) = 
        match x with
        | Int x -> Int (x - y)
        | Int64 x -> Int64 (x - int64 y)
        | BigInt x -> BigInt (x - BigInteger y)
        | Float x -> Float (x - float y)
        | Complex x -> Complex (x - new Complex(float y, 0.))
        | Function x -> Function (fun z -> (x z) - y)
        
    static member (-) ((x : int), y) = 
        match y with
        | Int y -> Int (x - y)
        | Int64 y -> Int64 (int64 x - y)
        | BigInt y -> BigInt (BigInteger x - y)
        | Float y -> Float (float x - y)
        | Complex y -> Complex (new Complex(float x, 0.) - y)
        | Function y -> Function (fun z -> x - (y z))

    static member (/) (x, (y : int)) = 
        match x with
        | Int x -> Int (x / y)
        | Int64 x -> Int64 (x / int64 y)
        | BigInt x -> BigInt (x / BigInteger y)
        | Float x -> Float (x / float y)
        | Complex x -> Complex (x / new Complex(float y, 0.))
        | Function x -> Function (fun z -> (x z) / y)
        
    static member (/) ((x : int), y) = 
        match y with
        | Int y -> Int (x / y)
        | Int64 y -> Int64 (int64 x / y)
        | BigInt y -> BigInt (BigInteger x / y)
        | Float y -> Float (float x / y)
        | Complex y -> Complex (new Complex(float x, 0.) / y)
        | Function y -> Function (fun z -> x / (y z))
    
let localMetricToFloat localMetric =
    match localMetric with
    | Int x -> float x
    | Int64 x -> float x
    | BigInt x -> float x
    | Float x -> x
    | Complex x -> x.Real
    | Function x -> 0.

let wrapFloatFunction (f : (float -> float)) =
    fun (y : LocalMetric) -> 
        match y with
        | Int y ->  f (float y) |> Float
        | Int64 y -> f (float  y) |> Float
        | BigInt y -> f (float  y) |> Float
        | Float y -> f y |> Float
        | Complex y -> f y.Real |> Float
        | Function y -> f 0. |> Float
    |> LocalMetric.Function

type Time = LocalMetric

type Func1 = LocalMetric -> LocalMetric
type Func2 = LocalMetric -> LocalMetric -> LocalMetric

let wrapFunc1 (f : Func1) =
    (fun (y : float) -> 
        match (f (Float y)) with
        | Int z -> float z
        | Int64 z -> float z
        | BigInt z -> float z
        | Float z -> z
        | Complex z -> z.Real
        | Function _ -> 0. ) 

type Local = Func1 list

type Derivative = unit

type State =
    {
        Time : Time
        Local : Local
        Dt : (Local -> Time -> LocalMetric list) list
    }

type UpIndexed =
    | LocalMetric of LocalMetric
    | Func1 of Func1
    | Func2 of Func2
    | UpIndexed of UpIndexed list
    | DownIndexed of DownIndexed list
and DownIndexed =
    | LocalMetric of LocalMetric
    | Func1 of Func1
    | Func2 of Func2
    | DownIndexed of DownIndexed list
    | UpIndexed of UpIndexed list

/// (define (state->qdot state)
///    (if (not (and (vector? state) (fix:> (vector-length state) 2)))
///        (error "Cannot extract velocity from" state))
///    (ref state 2))
let firstDerivative (state : State) =
    state.Dt.Head state.Local state.Time

let derivitave (f : Func1) (time : Time) =
  //  LocalMetric.Float 0.  // to do: 1st time derivative of coordinate
    MathNet.Numerics.Differentiate.firstDerivativeFunc (wrapFunc1 f) 
    |> wrapFloatFunction
