namespace TypedSicm

open System
open Utilities
open NelderMead

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
            |> Array.map (fun f -> derivitave f)
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
    let dotProduct (vector1 : Scalar [])  (vector2 : Scalar [])  =
        vector1
        |> Array.zip vector2
        |> Array.map (fun (v1, v2) -> v1 * v2)
        |> Array.fold (fun s t -> t + s ) (Int 0)

    module S4ComputingActions =
        /// (define ((L-free-particle mass) local)
        ///     (let ((v (velocity local)))
        ///     (* 1/2 mass (dot-product v v))))
        let lagrangianFreeParticle (mass : Scalar) local = 
            let v = 
                velocity local
                |> Array.map (fun f ->  f local.Time)
                    
            (mass * (dotProduct v v)) / 2

        /// (define (Lagrangian-action L q t1 t2)
        ///     (definite-integral (compose L (Gamma q)) t1 t2))
        let lagrangianAction (lagrangian : Local -> Scalar) path (time1 : Time) (time2 : Time) =
            let f =
                fun time ->
                    lagrangian (gamma path time) 
                |> definiteIntegral

            f (scalarToFloat time1) (scalarToFloat time2)

        /// (define (test-path t)
        ///     (up (+ (* 4 t) 7)
        ///         (+ (* 3 t) 5)
        ///         (+ (* 2 t) 1)))
        let testPath : UpIndexed =
            [|
                (fun (time : Time) -> 4 * time + 7)
                (fun (time : Time) -> 3 * time + 5)
                (fun (time : Time) -> 2 * time + 1)
            |]

        let test1 () = lagrangianAction (lagrangianFreeParticle (Scalar.Int 3)) testPath (floatToTime 0.) (floatToTime 10.)
        
        /// (define ((make-eta nu t1 t2) t)
        ///     (* (- t t1) (- t t2) (nu t)))
        let makeEta (nu : UpIndexed) (time1 : Time) (time2 : Time) : UpIndexed =
            nu
            |> Array.map (fun f -> 
                fun (t : Scalar) -> (t - time1) * (t - time2) * f t 
            )

        /// (define ((varied-free-particle-action mass q nu t1 t2) eps)
        ///     (let ((eta (make-eta nu t1 t2)))
        ///         (Lagrangian-action (L-free-particle mass)
        ///                            (+ q (* eps eta))
        ///                            t1
        ///                            t2)))
        let variedFreeParticleAction mass (q : UpIndexed) nu time1 time2 (eps : Scalar) =
            let eta = makeEta nu time1 time2
            lagrangianAction 
                (lagrangianFreeParticle mass) 
                (addUp q (eps * eta))
                time1 
                time2
       
        ///(up sin cos square)
        let nu : UpIndexed =
            [|
                wrapFloatFunction Math.Sin
                wrapFloatFunction Math.Cos
                (fun x -> x * x)
            |]

        let test2 () = variedFreeParticleAction (Scalar.Int 3) testPath nu (floatToTime 0.) (floatToTime 10.) 
                                                                                            (Scalar.Float 0.001)
        let test3 () = 
            let vFPA = variedFreeParticleAction (Scalar.Int 3) testPath nu (floatToTime 0.) (floatToTime 10.)
            let vFPA' =
                fun (x : float) ->
                    vFPA (Scalar.Float x)
            minimize vFPA' -2.0 1.

        /// (define ((parametric-path-action Lagrangian t0 q0 t1 q1) qs)
        ///     (let ((path (make-path t0 q0 t1 q1 qs)))
        ///         (Lagrangian-action Lagrangian path t0 t1)))
        let parametricPathAction lagrangian t0 q0 t1 q1 =
            fun qs ->
                let path = makePath t0 q0 t1 q1 qs 
                lagrangianAction lagrangian path t0 t1

        /// (define (find-path Lagrangian t0 q0 t1 q1 n)
        ///     (let ((initial-qs (linear-interpolants q0 q1 n)))
        ///         (let ((minimizing-qs
        ///                 (multidimensional-minimize
        ///                     (parametric-path-action Lagrangian t0 q0 t1 q1)
        ///                     initial-qs)))
        ///             (make-path t0 q0 t1 q1 minimizing-qs))))
        let findPath lagrangian t0 q0 t1 q1 n =
            let initialQs = linearInterpolants q0 q1 n
            let minimizingQs =
                multidimensionalMinimize
                    (parametricPathAction lagrangian t0 q0 t1 q1)
                    initialQs
                |> Array.toList
            makePath t0 q0 t1 q1 minimizingQs
