module TypedSicm.GenericArithmetic

open System
open System.Numerics
open FSharpx.Collections

module Vector = RandomAccessList

[<CustomComparison>]
[<CustomEquality>]
type Scalar =
| Int of int
| Int64 of int64
| BigInt of BigInteger
| Float of float

   with 

    interface IComparable with
        member __.CompareTo yobj =
            match yobj with
            | :? Scalar as y -> 
                match __, y with
                | Int x, Int y -> 
                    if x > y then 1
                    elif x < y then -1
                    else 0
                | Int x, Int64 y -> 
                    let x' = int64 x
                    if x' > y then 1
                    elif x' < y then -1
                    else 0
                | Int x, BigInt y -> 
                    let x' = BigInteger x
                    if x' > y then 1
                    elif x' < y then -1
                    else 0
                | Int x, Float y -> 
                    let x' = float x
                    if x' > y then 1
                    elif x' < y then -1
                    else 0

                | Int64 x, Int y -> 
                    let y' = int64 y
                    if x > y' then 1
                    elif x < y' then -1
                    else 0
                | Int64 x, Int64 y -> 
                    if x > y then 1
                    elif x < y then -1
                    else 0
                | Int64 x, BigInt y -> 
                    let x' = BigInteger x
                    if x' > y then 1
                    elif x' < y then -1
                    else 0
                | Int64 x, Float y -> 
                    let x' = float x
                    if x' > y then 1
                    elif x' < y then -1
                    else 0

                | BigInt x, Int y -> 
                    let y' = BigInteger y
                    if x > y' then 1
                    elif x < y' then -1
                    else 0
                | BigInt x, Int64 y -> 
                    let y' = BigInteger y
                    if x > y' then 1
                    elif x < y' then -1
                    else 0
                | BigInt x, BigInt y -> 
                    if x > y then 1
                    elif x < y then -1
                    else 0
                | BigInt x, Float y -> 
                    let x' =float x
                    if x' > y then 1
                    elif x' < y then -1
                    else 0
                | Float x, Int y ->
                    let y' = float y
                    if x > y' then 1
                    elif x < y' then -1
                    else 0
                | Float x, Int64 y -> 
                    let y' = float y
                    if x > y' then 1
                    elif x < y' then -1
                    else 0
                | Float x, BigInt y -> 
                    let y' = float y
                    if x > y' then 1
                    elif x < y' then -1
                    else 0
                | Float x, Float y -> 
                    if x > y then 1
                    elif x < y then -1
                    else 0

            | _ -> invalidArg "Scalar" "cannot compare Scalar with other type"

    override __.Equals yobj = 
        match yobj with
        | :? Scalar as y -> 
            match __, y with
            | Int x, Int y -> x = y
            | Int x, Int64 y -> int64 x = y
            | Int x, BigInt y -> BigInteger x = y
            | Int x, Float y -> float x = y

            | Int64 x, Int y -> x = int64 y
            | Int64 x, Int64 y -> x = y
            | Int64 x, BigInt y -> BigInteger x = y
            | Int64 x, Float y -> float x = y

            | BigInt x, Int y -> x = BigInteger y
            | BigInt x, Int64 y -> x = BigInteger y
            | BigInt x, BigInt y -> x = y
            | BigInt x, Float y -> float x = y

            | Float x, Int y -> x = float y
            | Float x, Int64 y -> x = float y
            | Float x, BigInt y -> x = float y
            | Float x, Float y -> x = y

        | _ -> invalidArg "Scalar" "cannot compare Scalar with other type"

    override __.GetHashCode() = hash __

    static member (+) (x, y) = 
        match x, y with
        | Int x, Int y -> Int (x + y)
        | Int x, Int64 y -> Int64 (int64 x + y)
        | Int x, BigInt y -> BigInt (BigInteger x + y)
        | Int x, Float y -> Float (float x + y)

        | Int64 x, Int y -> Int64 (x + int64 y)
        | Int64 x, Int64 y -> Int64 (x + y)
        | Int64 x, BigInt y -> BigInt (BigInteger x + y)
        | Int64 x, Float y -> Float (float x + y)

        | BigInt x, Int y -> BigInt (x + BigInteger y)
        | BigInt x, Int64 y -> BigInt (x + BigInteger y)
        | BigInt x, BigInt y -> BigInt (x + y)
        | BigInt x, Float y -> Float (float x + y)

        | Float x, Int y -> Float (x + float y)
        | Float x, Int64 y -> Float (x + float y)
        | Float x, BigInt y -> Float (x + float y)
        | Float x, Float y -> Float (x + y)

    static member (*) (x, y) = 
        match x, y with
        | Int x, Int y -> Int (x * y)
        | Int x, Int64 y -> Int64 (int64 x * y)
        | Int x, BigInt y -> BigInt (BigInteger x * y)
        | Int x, Float y -> Float (float x * y)

        | Int64 x, Int y -> Int64 (x * int64 y)
        | Int64 x, Int64 y -> Int64 (x * y)
        | Int64 x, BigInt y -> BigInt (BigInteger x * y)
        | Int64 x, Float y -> Float (float x * y)

        | BigInt x, Int y -> BigInt (x * BigInteger y)
        | BigInt x, Int64 y -> BigInt (x * BigInteger y)
        | BigInt x, BigInt y -> BigInt (x * y)
        | BigInt x, Float y -> Float (float x * y)

        | Float x, Int y -> Float (x * float y)
        | Float x, Int64 y -> Float (x * float y)
        | Float x, BigInt y -> Float (x * float y)
        | Float x, Float y -> Float (x * y)

    static member (-) (x, y) = 
        match x, y with
        | Int x, Int y -> Int (x - y)
        | Int x, Int64 y -> Int64 (int64 x - y)
        | Int x, BigInt y -> BigInt (BigInteger x - y)
        | Int x, Float y -> Float (float x - y)

        | Int64 x, Int y -> Int64 (x - int64 y)
        | Int64 x, Int64 y -> Int64 (x - y)
        | Int64 x, BigInt y -> BigInt (BigInteger x - y)
        | Int64 x, Float y -> Float (float x - y)

        | BigInt x, Int y -> BigInt (x - BigInteger y)
        | BigInt x, Int64 y -> BigInt (x - BigInteger y)
        | BigInt x, BigInt y -> BigInt (x - y)
        | BigInt x, Float y -> Float (float x - y)

        | Float x, Int y -> Float (x - float y)
        | Float x, Int64 y -> Float (x - float y)
        | Float x, BigInt y -> Float (x - float y)
        | Float x, Float y -> Float (x - y)

    static member (/) (x, y) = 
        match x, y with
        | Int x, Int y -> Int (x / y)
        | Int x, Int64 y -> Int64 (int64 x / y)
        | Int x, BigInt y -> BigInt (BigInteger x / y)
        | Int x, Float y -> Float (float x / y)

        | Int64 x, Int y -> Int64 (x / int64 y)
        | Int64 x, Int64 y -> Int64 (x / y)
        | Int64 x, BigInt y -> BigInt (BigInteger x / y)
        | Int64 x, Float y -> Float (float x / y)

        | BigInt x, Int y -> BigInt (x / BigInteger y)
        | BigInt x, Int64 y -> BigInt (x / BigInteger y)
        | BigInt x, BigInt y -> BigInt (x / y)
        | BigInt x, Float y -> Float (float x / y)

        | Float x, Int y -> Float (x / float y)
        | Float x, Int64 y -> Float (x / float y)
        | Float x, BigInt y -> Float (x / float y)
        | Float x, Float y -> Float (x / y)

    static member (+) (x, y : int) = 
        match x with
        | Int x -> Int (x + y)
        | Int64 x -> Int64 (x + int64 y)
        | BigInt x -> BigInt (x + BigInteger y)
        | Float x -> Float (x + float y)
    
    static member (+) (x : int, y : Scalar) = 
        y + x

    static member (+) (x, y : int64) = 
        match x with
        | Int x -> Int64 (int64 x + y)
        | Int64 x -> Int64 (x + y)
        | BigInt x -> BigInt (x + BigInteger y)
        | Float x -> Float (x + float y)
    
    static member (+) ((x : int64), y : Scalar) = 
        y + x

    static member (+) (x, y : BigInteger) = 
        match x with
        | Int x -> BigInt (BigInteger x + y)
        | Int64 x -> BigInt (BigInteger x + y)
        | BigInt x -> BigInt (x + y)
        | Float x -> Float (x + float y)
    
    static member (+) ((x : BigInteger), y : Scalar) = 
        y + x

    static member (+) (x, y : float) = 
        match x with
        | Int x -> Float (float x + y)
        | Int64 x -> Float (float x + y)
        | BigInt x -> Float (float x + y)
        | Float x -> Float (x + y)
    
    static member (+) ((x : float), y : Scalar) = 
        y + x

    static member (*) (x, y : int) = 
        match x with
        | Int x -> Int (x * y)
        | Int64 x -> Int64 (x * int64 y)
        | BigInt x -> BigInt (x * BigInteger y)
        | Float x -> Float (x * float y)
    
    static member (*) (x : int, y : Scalar) = 
        y * x

    static member (*) (x, y : int64) = 
        match x with
        | Int x -> Int64 (int64 x * y)
        | Int64 x -> Int64 (x * y)
        | BigInt x -> BigInt (x * BigInteger y)
        | Float x -> Float (x * float y)
    
    static member (*) ((x : int64), y : Scalar) = 
        y * x

    static member (*) (x, y : BigInteger) = 
        match x with
        | Int x -> BigInt (BigInteger x * y)
        | Int64 x -> BigInt (BigInteger x * y)
        | BigInt x -> BigInt (x * y)
        | Float x -> Float (x * float y)
    
    static member (*) ((x : BigInteger), y : Scalar) = 
        y * x

    static member (*) (x, y : float) = 
        match x with
        | Int x -> Float (float x * y)
        | Int64 x -> Float (float x * y)
        | BigInt x -> Float (float x * y)
        | Float x -> Float (x * y)
    
    static member (*) ((x : float), y : Scalar) = 
        y * x

    static member (-) (x, y : int) = 
        match x with
        | Int x -> Int (x - y)
        | Int64 x -> Int64 (x - int64 y)
        | BigInt x -> BigInt (x - BigInteger y)
        | Float x -> Float (x - float y)
    
    static member (-) (x : int, y) = 
        match y with
        | Int y -> Int (x - y)
        | Int64 y -> Int64 (int64 x - y)
        | BigInt y -> BigInt (BigInteger x - y)
        | Float y -> Float (float x - y)

    static member (-) (x, y : int64) = 
        match x with
        | Int x -> Int64 (int64 x - y)
        | Int64 x -> Int64 (x - y)
        | BigInt x -> BigInt (x - BigInteger y)
        | Float x -> Float (x - float y)
    
    static member (-) (x : int64, y) = 
        match y with
        | Int y -> Int64 (x - int64 y)
        | Int64 y -> Int64 (x - y)
        | BigInt y -> BigInt (BigInteger x - y)
        | Float y -> Float (float x - y)

    static member (-) (x, y : BigInteger) = 
        match x with
        | Int x -> BigInt (BigInteger x - y)
        | Int64 x -> BigInt (BigInteger x - y)
        | BigInt x -> BigInt (x - y)
        | Float x -> Float (x - float y)
    
    static member (-) (x : BigInteger, y) = 
        match y with
        | Int y -> BigInt (x - BigInteger y)
        | Int64 y -> BigInt (x - BigInteger y)
        | BigInt y -> BigInt (x - y)
        | Float y -> Float (float x - y)

    static member (-) (x, y : float) = 
        match x with
        | Int x -> Float (float x - y)
        | Int64 x -> Float (float x - y)
        | BigInt x -> Float (float x - y)
        | Float x -> Float (x - y)
    
    static member (-) (x : float, y) = 
        match y with
        | Int y -> Float (x - float y)
        | Int64 y -> Float (x - float y)
        | BigInt y -> Float (x - float y)
        | Float y -> Float (x - y)

    static member (/) (x, y : int) = 
        match x with
        | Int x -> Int (x / y)
        | Int64 x -> Int64 (x / int64 y)
        | BigInt x -> BigInt (x / BigInteger y)
        | Float x -> Float (x / float y)
    
    static member (/) (x : int, y) = 
        match y with
        | Int y -> Int (x / y)
        | Int64 y -> Int64 (int64 x / y)
        | BigInt y -> BigInt (BigInteger x / y)
        | Float y -> Float (float x / y)

    static member (/) (x, y : int64) = 
        match x with
        | Int x -> Int64 (int64 x / y)
        | Int64 x -> Int64 (x / y)
        | BigInt x -> BigInt (x / BigInteger y)
        | Float x -> Float (x / float y)
    
    static member (/) (x : int64, y) = 
        match y with
        | Int y -> Int64 (x / int64 y)
        | Int64 y -> Int64 (x / y)
        | BigInt y -> BigInt (BigInteger x / y)
        | Float y -> Float (float x / y)

    static member (/) (x, y : BigInteger) = 
        match x with
        | Int x -> BigInt (BigInteger x / y)
        | Int64 x -> BigInt (BigInteger x / y)
        | BigInt x -> BigInt (x / y)
        | Float x -> Float (x / float y)
    
    static member (/) (x : BigInteger, y) = 
        match y with
        | Int y -> BigInt (x / BigInteger y)
        | Int64 y -> BigInt (x / BigInteger y)
        | BigInt y -> BigInt (x / y)
        | Float y -> Float (float x / y)

    static member (/) (x, y : float) = 
        match x with
        | Int x -> Float (float x / y)
        | Int64 x -> Float (float x / y)
        | BigInt x -> Float (float x / y)
        | Float x -> Float (x / y)

    static member (/) (x : float, y) = 
        match y with
        | Int y -> Float (x / float y)
        | Int64 y -> Float (x / float y)
        | BigInt y -> Float (x / float y)
        | Float y -> Float (x / y)

    static member (+) (x : Scalar, ys : UpIndexed) : UpIndexed = 
        ys 
        |> Vector.map (fun y -> 
            match y with
            | Scalar y' -> x + y' |> Indexable.Scalar
            | Func y' -> x + y' |> Indexable.Func
        )

    static member (+) (xs : UpIndexed, y : Scalar) : UpIndexed = 
        y + xs

    static member (*) (x : Scalar, ys : UpIndexed) : UpIndexed = 
        ys 
        |> Vector.map (fun y -> 
            match y with
            | Scalar y' -> x * y' |> Indexable.Scalar
            | Func y' -> x * y' |> Indexable.Func
        )

    static member (*) (xs : UpIndexed, y : Scalar) : UpIndexed = 
        y * xs

    static member (-) (x : Scalar, ys : UpIndexed) : UpIndexed = 
        ys 
        |> Vector.map (fun y -> 
            match y with
            | Scalar y' -> x - y' |> Indexable.Scalar
            | Func y' -> x - y' |> Indexable.Func
        )

    static member (-) (xs : UpIndexed, y : Scalar) : UpIndexed = 
        xs 
        |> Vector.map (fun x -> 
            match x with
            | Scalar x' -> x' - y |> Indexable.Scalar
            | Func x' -> x' - y |> Indexable.Func
        )

    static member (/) (x : Scalar, ys : UpIndexed) : UpIndexed = 
        ys 
        |> Vector.map (fun y -> 
            match y with
            | Scalar y' -> x / y' |> Indexable.Scalar
            | Func y' -> x / y' |> Indexable.Func
        )

    static member (/) (xs : UpIndexed, y : Scalar) : UpIndexed = 
        xs 
        |> Vector.map (fun x -> 
            match x with
            | Scalar x' -> x' / y |> Indexable.Scalar
            | Func x' -> x' / y |> Indexable.Func
        )

    static member (+) (x : Scalar, ys) = 
        ys |> Vector.map (fun y -> x + y)

    static member (*) (x : Scalar, ys) = 
        ys |> Vector.map (fun y -> x * y)

    static member (-) (x : Scalar, ys) = 
        ys |> Vector.map (fun y -> x - y)

    static member (-) (xs, y : Scalar) = 
        xs |> Vector.map (fun x -> x - y)

    static member (/) (x : Scalar, ys) = 
        ys |> Vector.map (fun y -> x / y)

    static member (/) (xs, y : Scalar) = 
        xs |> Vector.map (fun x -> x / y)

and Time = Scalar

and [<Class>] ScalarFunc(scalarFunc : (Scalar -> Scalar)) =  
    member _.Invoke = scalarFunc
    with 

    static member (+) (x : Scalar, y : ScalarFunc) = 
        ScalarFunc (fun z -> x + y.Invoke z)

    static member (+) (x : ScalarFunc, y : Scalar) = 
        y + x

    static member (+) (x : int, y : ScalarFunc) = 
        ScalarFunc (fun z -> x + y.Invoke z)

    static member (+) (x : ScalarFunc, y : int) = 
        y + x

    static member (+) (x : int64, y : ScalarFunc) = 
        ScalarFunc (fun z -> x + y.Invoke z)

    static member (+) (x : ScalarFunc, y : int64) = 
        y + x

    static member (+) (x : BigInteger, y : ScalarFunc) = 
        ScalarFunc (fun z -> x + y.Invoke z)

    static member (+) (x : ScalarFunc, y : BigInteger) = 
        y + x

    static member (+) (x : float, y : ScalarFunc) = 
        ScalarFunc (fun z -> x + y.Invoke z)

    static member (+) (x : ScalarFunc, y : float) = 
        y + x

    static member (+) (x: ScalarFunc, y : ScalarFunc) = 
        ScalarFunc (fun z -> x.Invoke z + y.Invoke z)

    static member (*) (x : Scalar, y : ScalarFunc) = 
        ScalarFunc (fun z -> x * y.Invoke z)

    static member (*) (x : ScalarFunc, y : Scalar) = 
        y * x

    static member (*) (x : int, y : ScalarFunc) = 
        ScalarFunc (fun z -> x * y.Invoke z)

    static member (*) (x : ScalarFunc, y : int) = 
        y * x

    static member (*) (x : int64, y : ScalarFunc) = 
        ScalarFunc (fun z -> x * y.Invoke z)

    static member (*) (x : ScalarFunc, y : int64) = 
        y * x

    static member (*) (x : BigInteger, y : ScalarFunc) = 
        ScalarFunc (fun z -> x * y.Invoke z)

    static member (*) (x : ScalarFunc, y : BigInteger) = 
        y * x

    static member (*) (x : float, y : ScalarFunc) = 
        ScalarFunc (fun z -> x * y.Invoke z)

    static member (*) (x : ScalarFunc, y : float) = 
        y * x

    static member (*) (x: ScalarFunc, y : ScalarFunc) = 
        ScalarFunc (fun z -> x.Invoke z * y.Invoke z)

    static member (-) (x : Scalar, y : ScalarFunc) = 
        ScalarFunc (fun z -> x - y.Invoke z)

    static member (-) (x : ScalarFunc, y : Scalar) = 
       ScalarFunc (fun z -> x.Invoke z - y)

    static member (-) (x : int, y : ScalarFunc) = 
        ScalarFunc (fun z -> x - y.Invoke z)

    static member (-) (x : ScalarFunc, y : int) = 
       ScalarFunc (fun z -> x.Invoke z - y)

    static member (-) (x : int64, y : ScalarFunc) = 
        ScalarFunc (fun z -> x - y.Invoke z)

    static member (-) (x : ScalarFunc, y : int64) = 
       ScalarFunc (fun z -> x.Invoke z - y)

    static member (-) (x : BigInteger, y : ScalarFunc) = 
        ScalarFunc (fun z -> x - y.Invoke z)

    static member (-) (x : ScalarFunc, y : BigInteger) = 
       ScalarFunc (fun z -> x.Invoke z - y)

    static member (-) (x : float, y : ScalarFunc) = 
        ScalarFunc (fun z -> x - y.Invoke z)

    static member (-) (x : ScalarFunc, y : float) = 
       ScalarFunc (fun z -> x.Invoke z - y)

    static member (-) (x: ScalarFunc, y : ScalarFunc) = 
        ScalarFunc (fun z -> x.Invoke z - y.Invoke z)

    static member (/) (x : Scalar, y : ScalarFunc) = 
        ScalarFunc (fun z -> x / y.Invoke z)

    static member (/) (x : ScalarFunc, y : Scalar) = 
       ScalarFunc (fun z -> x.Invoke z / y)

    static member (/) (x : int, y : ScalarFunc) = 
        ScalarFunc (fun z -> x / y.Invoke z)

    static member (/) (x : ScalarFunc, y : int) = 
       ScalarFunc (fun z -> x.Invoke z / y)

    static member (/) (x : int64, y : ScalarFunc) = 
        ScalarFunc (fun z -> x / y.Invoke z)

    static member (/) (x : ScalarFunc, y : int64) = 
       ScalarFunc (fun z -> x.Invoke z / y)

    static member (/) (x : BigInteger, y : ScalarFunc) = 
        ScalarFunc (fun z -> x / y.Invoke z)

    static member (/) (x : ScalarFunc, y : BigInteger) = 
       ScalarFunc (fun z -> x.Invoke z / y)

    static member (/) (x : float, y : ScalarFunc) = 
        ScalarFunc (fun z -> x / y.Invoke z)

    static member (/) (x : ScalarFunc, y : float) = 
       ScalarFunc (fun z -> x.Invoke z / y)

    static member (/) (x: ScalarFunc, y : ScalarFunc) = 
        ScalarFunc (fun z -> x.Invoke z / y.Invoke z)

and Indexable =
| Scalar of Scalar
| Func of ScalarFunc

    with

    static member (+) (x : Scalar, y : Indexable) : Indexable = 
        match y with
        | Scalar y' -> x + y' |> Indexable.Scalar
        | Func y' -> 
            let w =  (fun z ->  x + y'.Invoke z) |> ScalarFunc
            Func w 

    static member (+) (x : Indexable, y : Scalar) : Indexable = 
        y + x

    static member (+) (x : int, y : Indexable) : Indexable = 
        match y with
        | Scalar y' -> x + y' |> Indexable.Scalar
        | Func y' -> 
            let w =  (fun z ->  x + y'.Invoke z) |> ScalarFunc
            Func w 

    static member (+) (x : Indexable, y : int) : Indexable = 
        y + x

    static member (+) (x : int64, y : Indexable) : Indexable = 
        match y with
        | Scalar y' -> x + y' |> Indexable.Scalar
        | Func y' -> 
            let w =  (fun z ->  x + y'.Invoke z) |> ScalarFunc
            Func w 

    static member (+) (x : Indexable, y : int64) : Indexable = 
        y + x

    static member (+) (x : BigInteger, y : Indexable) : Indexable = 
        match y with
        | Scalar y' -> x + y' |> Indexable.Scalar
        | Func y' -> 
            let w =  (fun z ->  x + y'.Invoke z) |> ScalarFunc
            Func w 

    static member (+) (x : Indexable, y : BigInteger) : Indexable = 
        y + x

    static member (+) (x : float, y : Indexable) : Indexable = 
        match y with
        | Scalar y' -> x + y' |> Indexable.Scalar
        | Func y' -> 
            let w =  (fun z ->  x + y'.Invoke z) |> ScalarFunc
            Func w 

    static member (+) (x : Indexable, y : float) : Indexable = 
        y + x

    static member (+) (x : Indexable, y : Indexable) : Indexable =
        match x, y with
        | Scalar x', Scalar y' -> x' + y' |> Indexable.Scalar
        | Func x', Scalar y' -> x' + y' |> Indexable.Func
        | Scalar x', Func y' -> x' + y' |> Indexable.Func
        | Func x', Func y' -> x' + y' |> Indexable.Func

    static member (*) (x : Scalar, y : Indexable) : Indexable = 
        match y with
        | Scalar y -> x * y |> Indexable.Scalar
        | Func y' -> 
            let w =  (fun z -> x * y'.Invoke z) |> ScalarFunc
            Func w 

    static member (*) (x : Indexable, y : Scalar) : Indexable = 
        y + x

    static member (*) (x : int, y : Indexable) : Indexable = 
        match y with
        | Scalar y' -> x * y' |> Indexable.Scalar
        | Func y' -> 
            let w =  (fun z ->  x * y'.Invoke z) |> ScalarFunc
            Func w 

    static member (*) (x : Indexable, y : int) : Indexable = 
        y * x

    static member (*) (x : int64, y : Indexable) : Indexable = 
        match y with
        | Scalar y' -> x * y' |> Indexable.Scalar
        | Func y' -> 
            let w =  (fun z ->  x * y'.Invoke z) |> ScalarFunc
            Func w 

    static member (*) (x : Indexable, y : int64) : Indexable = 
        y * x

    static member (*) (x : BigInteger, y : Indexable) : Indexable = 
        match y with
        | Scalar y' -> x * y' |> Indexable.Scalar
        | Func y' -> 
            let w =  (fun z ->  x * y'.Invoke z) |> ScalarFunc
            Func w 

    static member (*) (x : Indexable, y : BigInteger) : Indexable = 
        y * x

    static member (*) (x : float, y : Indexable) : Indexable = 
        match y with
        | Scalar y' -> x * y' |> Indexable.Scalar
        | Func y' -> 
            let w =  (fun z ->  x * y'.Invoke z) |> ScalarFunc
            Func w 

    static member (*) (x : Indexable, y : float) : Indexable = 
        y * x

    static member (*) (x : Indexable, y : Indexable) : Indexable =
        match x, y with
        | Scalar x', Scalar y' -> x' * y' |> Indexable.Scalar
        | Func x', Scalar y' -> x' * y' |> Indexable.Func
        | Scalar x', Func y' -> x' * y' |> Indexable.Func
        | Func x', Func y' -> x' * y' |> Indexable.Func

    static member (-) (x : Scalar, y : Indexable) : Indexable = 
        match y with
        | Scalar y' -> x - y' |> Indexable.Scalar
        | Func y' -> 
            let w =  (fun z -> x - y'.Invoke z) |> ScalarFunc
            Func w 

    static member (-) (x : Indexable, y : Scalar) : Indexable = 
        match x with
        | Scalar x' -> x' - y |> Indexable.Scalar
        | Func x' -> 
            let w =  (fun z -> x'.Invoke z - y) |> ScalarFunc
            Func w 

    static member (-) (x : int, y : Indexable) : Indexable = 
        match y with
        | Scalar y' -> x - y' |> Indexable.Scalar
        | Func y' -> 
            let w =  (fun z -> x - y'.Invoke z) |> ScalarFunc
            Func w 

    static member (-) (x : Indexable, y : int) : Indexable = 
        match x with
        | Scalar x' -> x' - y |> Indexable.Scalar
        | Func x' -> 
            let w =  (fun z -> x'.Invoke z - y) |> ScalarFunc
            Func w 

    static member (-) (x : int64, y : Indexable) : Indexable = 
        match y with
        | Scalar y' -> x - y' |> Indexable.Scalar
        | Func y' -> 
            let w =  (fun z -> x - y'.Invoke z) |> ScalarFunc
            Func w 

    static member (-) (x : Indexable, y : int64) : Indexable = 
        match x with
        | Scalar x' -> x' - y |> Indexable.Scalar
        | Func x' -> 
            let w =  (fun z -> x'.Invoke z - y) |> ScalarFunc
            Func w

    static member (-) (x : BigInteger, y : Indexable) : Indexable = 
        match y with
        | Scalar y' -> x - y' |> Indexable.Scalar
        | Func y' -> 
            let w =  (fun z -> x - y'.Invoke z) |> ScalarFunc
            Func w 

    static member (-) (x : Indexable, y : BigInteger) : Indexable = 
        match x with
        | Scalar x' -> x' - y |> Indexable.Scalar
        | Func x' -> 
            let w =  (fun z -> x'.Invoke z - y) |> ScalarFunc
            Func w 

    static member (-) (x : float, y : Indexable) : Indexable = 
        match y with
        | Scalar y' -> x - y' |> Indexable.Scalar
        | Func y' -> 
            let w =  (fun z -> x - y'.Invoke z) |> ScalarFunc
            Func w 

    static member (-) (x : Indexable, y : float) : Indexable = 
        match x with
        | Scalar x' -> x' - y |> Indexable.Scalar
        | Func x' -> 
            let w =  (fun z -> x'.Invoke z - y) |> ScalarFunc
            Func w 

    static member (-) (x : Indexable, y : Indexable) : Indexable =
        match x, y with
        | Scalar x', Scalar y' -> x' - y' |> Indexable.Scalar
        | Func x', Scalar y' -> x' - y' |> Indexable.Func
        | Scalar x', Func y' -> x' - y' |> Indexable.Func
        | Func x', Func y' -> x' - y' |> Indexable.Func

    static member (/) (x : Scalar, y : Indexable) : Indexable = 
        match y with
        | Scalar y' -> x / y' |> Indexable.Scalar
        | Func y' -> 
            let w =  (fun z -> x / y'.Invoke z) |> ScalarFunc
            Func w 

    static member (/) (x : Indexable, y : Scalar) : Indexable = 
        match x with
        | Scalar x' -> x' / y |> Indexable.Scalar
        | Func x' -> 
            let w =  (fun z -> x'.Invoke z / y) |> ScalarFunc
            Func w 

    static member (/) (x : int, y : Indexable) : Indexable = 
        match y with
        | Scalar y' -> x / y' |> Indexable.Scalar
        | Func y' -> 
            let w =  (fun z -> x / y'.Invoke z) |> ScalarFunc
            Func w 

    static member (/) (x : Indexable, y : int) : Indexable = 
        match x with
        | Scalar x' -> x' / y |> Indexable.Scalar
        | Func x' -> 
            let w =  (fun z -> x'.Invoke z / y) |> ScalarFunc
            Func w 

    static member (/) (x : int64, y : Indexable) : Indexable = 
        match y with
        | Scalar y' -> x / y' |> Indexable.Scalar
        | Func y' -> 
            let w =  (fun z -> x / y'.Invoke z) |> ScalarFunc
            Func w 

    static member (/) (x : Indexable, y : int64) : Indexable = 
        match x with
        | Scalar x' -> x' / y |> Indexable.Scalar
        | Func x' -> 
            let w =  (fun z -> x'.Invoke z / y) |> ScalarFunc
            Func w

    static member (/) (x : BigInteger, y : Indexable) : Indexable = 
        match y with
        | Scalar y' -> x / y' |> Indexable.Scalar
        | Func y' -> 
            let w =  (fun z -> x / y'.Invoke z) |> ScalarFunc
            Func w 

    static member (/) (x : Indexable, y : BigInteger) : Indexable = 
        match x with
        | Scalar x' -> x' / y |> Indexable.Scalar
        | Func x' -> 
            let w =  (fun z -> x'.Invoke z / y) |> ScalarFunc
            Func w

    static member (/) (x : float, y : Indexable) : Indexable = 
        match y with
        | Scalar y' -> x / y' |> Indexable.Scalar
        | Func y' -> 
            let w =  (fun z -> x / y'.Invoke z) |> ScalarFunc
            Func w 

    static member (/) (x : Indexable, y : float) : Indexable = 
        match x with
        | Scalar x' -> x' / y |> Indexable.Scalar
        | Func x' -> 
            let w =  (fun z -> x'.Invoke z / y) |> ScalarFunc
            Func w

    static member (/) (x : Indexable, y : Indexable) : Indexable =
        match x, y with
        | Scalar x', Scalar y' -> x' / y' |> Indexable.Scalar
        | Func x', Scalar y' -> x' / y' |> Indexable.Func
        | Scalar x', Func y' -> x' / y' |> Indexable.Func
        | Func x', Func y' -> x' / y' |> Indexable.Func

and UpIndexed = RandomAccessList<Indexable>

and DownIndexed =  (Scalar -> Scalar) []

//to do: remainder
//to do: exponentiation
//to do: integer overflow on addition
//to do: integer overflow on multiplication
//to do: integer overflow on exponentiation
