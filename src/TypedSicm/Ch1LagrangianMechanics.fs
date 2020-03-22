namespace TypedSicm

open System
open FSharpx.Collections
open GenericArithmetic
open Utilities
open NelderMead

module Vector = RandomAccessList

/// Chapter 1, Lagrangian Mechanics
module Ch1_LagrangianMechanics =
    type Local =
        {
            Time : Time      
            CoordninatePath : UpIndexed
            Derivatives : UpIndexed []
        }

    /// (define (path->state-path q #!optional n)
    /// (if (default-object? n)
    ///     (set! n 3)
    ///     (assert (fix:> n 1)))
    /// (lambda (t)
    ///   (list->vector
    ///    (cons t
    ///        (cons (q t)
    ///          (let lp ((i (fix:- n 2)) (fi (D q)))
    ///            (if (fix:= i 0)
    ///                '()
    ///                (cons (fi t)
    ///                  (lp (- i 1)
    ///                  (D fi))))))))))
    ///
    /// (define Gamma path->state-path)
    type Γ = UpIndexed -> Time -> Local
           
    let gamma (q : UpIndexed) (time : Time) =
        let ds =
            q
            |> Vector.map (fun f -> 
                match f with
                | Func f' -> 
                    derivative f'.Invoke |> indexableFunc
                | Scalar s -> 
                    derivative id |> indexableFunc
            )
        {
            Time = time      
            CoordninatePath = q 
            Derivatives = [|ds|]
        }

    /// (define (state->qdot state)
    ///   (if (not (and (vector? state) (fix:> (vector-length state) 2)))
    ///   (error "Cannot extract velocity from" state))
    ///   (ref state 2))
    ///
    /// (define velocity state->qdot)
    let velocity (local : Local) = local.Derivatives.[0]

    /// (define (v:dot-product v1 v2)
    ///     (assert (and (vector? v1) (vector? v2))
    ///       "Not vectors -- V:DOT-PRODUCT" (list v1 v2))
    ///     (let ((n (v:dimension v1)))
    ///       (assert (fix:= n (v:dimension v2))
    ///         "Not same dimension -- V:DOT-PRODUCT" (list v1 v2))
    ///       (let lp ((i 0) (ans :zero))
    ///         (if (fix:= i n)
    ///       ans
    ///       (lp (fix:+ i 1)
    ///           (g:+ ans
    ///            (g:* (vector-ref v1 i)
    ///             (vector-ref v2 i))))))))
    let dotProduct (vector1 : UpIndexed)  (vector2 : UpIndexed)  =
        vector1
        |> Vector.zip vector2
        |> Vector.map (fun (v1, v2) ->   v1 * v2)
        |> Vector.reduce (fun s t -> t + s )

    module S4ComputingActions =
        /// (define ((L-free-particle mass) local)
        ///     (let ((v (velocity local)))
        ///     (* 1/2 mass (dot-product v v))))
        let lagrangianFreeParticle (mass : Scalar) local = 
            let v = 
                velocity local
                |> Vector.map (fun f ->  
                    match f with
                    | Func f' -> f'.Invoke local.Time |> Indexable.Scalar
                    | _ -> f
                )
            (mass * (dotProduct v v)) / 2

        /// (define (Lagrangian-action L q t1 t2)
        ///     (definite-integral (compose L (Gamma q)) t1 t2))
        let lagrangianAction (lagrangian : Local -> Indexable) path (time1 : Time) (time2 : Time) =
            let f =
                fun time ->
                    lagrangian (gamma path time) 
                |> definiteIntegral

            f time1 time2

        /// (define (test-path t)
        ///     (up (+ (* 4 t) 7)
        ///         (+ (* 3 t) 5)
        ///         (+ (* 2 t) 1)))
        let testPath : UpIndexed =
            [|
                (fun (time : Time) -> 4 * time + 7) |> indexableFunc
                (fun (time : Time) -> 3 * time + 5) |> indexableFunc
                (fun (time : Time) -> 2 * time + 1) |> indexableFunc
            |] |> Vector.ofSeq

        let test1 () = lagrangianAction (lagrangianFreeParticle (Scalar.Int 3)) testPath (floatToTime 0.) (floatToTime 10.)
        
        /// (define ((make-eta nu t1 t2) t)
        ///     (* (- t t1) (- t t2) (nu t)))
        let makeEta (nu : UpIndexed) (time1 : Time) (time2 : Time) : UpIndexed =
            nu
            |> Vector.map (fun f -> 
                match f with
                | Func f' -> 
                    fun (t : Scalar) -> (t - time1) * (t - time2) * f'.Invoke t 
                    |> indexableFunc
                | _ -> 
                    f
            )

        /// (define ((varied-free-particle-action mass q nu t1 t2) eps)
        ///     (let ((eta (make-eta nu t1 t2)))
        ///         (Lagrangian-action (L-free-particle mass)
        ///                            (+ q (* eps eta))
        ///                            t1
        ///                            t2)))
        let variedFreeParticleAction mass q nu time1 time2 (eps : Scalar) =
            let eta = makeEta nu time1 time2
            lagrangianAction 
                (lagrangianFreeParticle mass) 
                (q + (eps * eta))
                time1 
                time2
       
        ///(up sin cos square)
        let nu : UpIndexed =
            [|
                wrapFloatFunction Math.Sin |> indexableFunc
                wrapFloatFunction Math.Cos |> indexableFunc
                (fun x -> x * x) |> indexableFunc
            |] |> Vector.ofSeq

        let test2 () = 
            variedFreeParticleAction (Scalar.Int 3) testPath nu (floatToTime 0.) (floatToTime 10.) (Scalar.Float 0.001)

        let test3 () = 
            let vFPA = variedFreeParticleAction (Scalar.Int 3) testPath nu (floatToTime 0.) (floatToTime 10.)
            let vFPA' =
                fun (x : float) ->
                    vFPA (Scalar.Float x)
            minimize vFPA' -2.0 1.

        /// (define ((parametric-path-action Lagrangian t0 q0 t1 q1) qs)
        ///     (let ((path (make-path t0 q0 t1 q1 qs)))
        ///         (Lagrangian-action Lagrangian path t0 t1)))
        let parametricPathAction lagrangian t0 q0 t1 q1 qs =
            let path = makePath t0 q0 t1 q1 qs 
            lagrangianAction lagrangian path t0 t1

        /// (define (find-path Lagrangian t0 q0 t1 q1 n)
        ///     (let ((initial-qs (linear-interpolants q0 q1 n)))
        ///         (let ((minimizing-qs
        ///                 (multidimensional-minimize
        ///                     (parametric-path-action Lagrangian t0 q0 t1 q1)
        ///                     initial-qs)))
        ///             (make-path t0 q0 t1 q1 minimizing-qs))))
        let findPath lagrangian time0 q0 time1 q1 n =
            let initialQs = linearInterpolants q0 q1 n
            let minimizingQs =
                multidimensionalMinimize
                    (parametricPathAction lagrangian time0 q0 time1 q1)
                    initialQs
            makePath time0 q0 time1 q1 minimizingQs

        /// (define ((L-harmonic m k) local)
        ///   (let ((q (coordinate local))
        ///       (v (velocity local)))
        ///     (- (* 1/2 m (square v)) (* 1/2 k (square q)))))
        let lagrangianHarmonic (m : float) (k : float) local =
            let q = local.CoordninatePath
            let v = velocity local

            (m * (squareVector v) / 2) - (k * (squareVector q) / 2)

        /// (define q       
        ///   (find-path (L-harmonic 1.0 1.0) 0.0 1.0 :pi/2 0.0 3))
        let q = findPath (lagrangianHarmonic 1.0 1.0) (floatToTime 0.0) (Scalar.Float 1.0) (pi/2) (Scalar.Float 0.0) 3
        