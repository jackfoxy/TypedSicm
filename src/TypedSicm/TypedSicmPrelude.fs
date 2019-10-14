[<AutoOpen>]
module TypedSicm.TypedSicmPrelude

open System
open System.Numerics
open MathNet.Numerics

type LocalMetric =
    | Int of int
    | Int64 of int64
    | BigInt of BigInteger
    | Float of float
    | Complex of Complex
    //| Quaternion of Quaternion
    | Func1 of (LocalMetric -> LocalMetric)
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

and Time = LocalMetric

and Func1 = LocalMetric -> LocalMetric

and UpIndexed =
    | LocalMetric of LocalMetric
    | Func1 of Func1
    | Func2 of (Func1 -> Time -> LocalMetric)
    | UpIndexed of UpIndexed list
    | DownIndexed of DownIndexed list
and DownIndexed =
    | LocalMetric of LocalMetric
    | Func1 of Func1
    | Func2 of (Func1 -> Time -> LocalMetric)
    | DownIndexed of DownIndexed list
    | UpIndexed of UpIndexed list
    
let localMetricToFloat localMetric =
    match localMetric with
    | Int x -> float x
    | Int64 x -> float x
    | BigInt x -> float x
    | Float x -> x
    | Complex x -> x.Real
    | LocalMetric.Func1 x -> 0.

    
let inline complexToLocal  c = LocalMetric.Complex c
let inline intToLocal i = LocalMetric.Int
let inline int64ToLocal i = LocalMetric.Int64 i
let inline floatToLocal f = LocalMetric.Float f
//let inline floatToFunc1 f =  
//    fun x -> 
//let inline FunctionToLocal  ( : ) = LocalMetric. 
//let inline ToLocal  ( : ) = LocalMetric. 
   // function
    //match x with
    //| :? Complex as c -> 
    //| :? int as i -> LocalMetric.Int i
    //| Int64 of int64
    //| BigInt of BigInteger
    //| Float of float
    //| Complex of Complex
    ////| Quaternion of Quaternion
    //| Function of (LocalMetric -> LocalMetric)

let inline floatToTime f = LocalMetric.Float f :> Time

let wrapFunc1 (f : Func1) =
    (fun (y : float) -> 
        match (f (Float y)) with
        | Int z -> float z
        | Int64 z -> float z
        | BigInt z -> float z
        | Float z -> z
        | Complex z -> z.Real
        | LocalMetric.Func1 _ -> 0. ) 

let wrapFloatFunction (f : (float -> float)) =
    fun (y : LocalMetric) -> 
        match y with
        | Int y ->  f (float y) |> Float
        | Int64 y -> f (float  y) |> Float
        | BigInt y -> f (float  y) |> Float
        | Float y -> f y |> Float
        | Complex y -> f y.Real |> Float
        | LocalMetric.Func1 y -> f 0. |> Float
    |> UpIndexed.Func1 

let ffToFuncUp (f : float -> float) =
    (fun f' ->
        match f' with
        | LocalMetric.Float n -> (f n) |> LocalMetric.Float 
        | _ -> invalidArg "" "" )
        |> UpIndexed.Func1

type State =
    {
        Time : UpIndexed      
        Local : UpIndexed list
        Dt : UpIndexed list
    }
    with 
        member __.getTime = 
            match __.Time with
            | UpIndexed.LocalMetric x ->  x //:> Time
        member __.getLocal =
            //match __.Local with
            //| UpIndexed xs ->  
                __.Local
                |> List.map (fun x -> 
                    match x with
                    | UpIndexed.Func1 x' -> x'
                )

/// (define (state->qdot state)
///    (if (not (and (vector? state) (fix:> (vector-length state) 2)))
///        (error "Cannot extract velocity from" state))
///    (ref state 2))
let firstDerivative (state : State) =
    let time = state.getTime
    let local = state.getLocal
    match state.Dt.Head with
    | UpIndexed.UpIndexed xs -> 
        xs
        |> List.map (fun x -> 
            match x with
            | UpIndexed.Func1 x' -> x'
        )

    |> List.zip local
    |> List.map (fun (localFunction, d') -> 
        match (localFunction time) with
        | LocalMetric.Float n ->
            (wrapFunc1 d') n
    )

let derivitave (f : Func1) =
    Differentiate.firstDerivativeFunc (wrapFunc1 f) 

let definiteIntegral (f : Func1) start finish =
    let f' : System.Func<float, float> = System.Func<float, float>(wrapFunc1 f)
    Integrate.OnClosedInterval(f', start, finish)
