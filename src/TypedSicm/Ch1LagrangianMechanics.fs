namespace TypedSicm

open System.Numerics

/// Chapter 1, Lagrangian Mechanics
module Ch1_LagrangianMechanics = 

    /// (define velocity state->qdot)
    let velocity = firstDerivative

    /// (define (v:dot-product v1 v2)
    /// (assert (and (vector? v1) (vector? v2))
    ///   "Not vectors -- V:DOT-PRODUCT" (list v1 v2))
    /// (let ((n (v:dimension v1)))
    ///   (assert (fix:= n (v:dimension v2))
    ///     "Not same dimension -- V:DOT-PRODUCT" (list v1 v2))
    ///   (let lp ((i 0) (ans :zero))
    ///     (if (fix:= i n)
    ///   ans
    ///   (lp (fix:+ i 1)
    ///       (g:+ ans
    ///        (g:* (vector-ref v1 i)
    ///         (vector-ref v2 i))))))))
    let inline dotProduct (vector1 : LocalMetric list)  (vector2 : LocalMetric list)  =
        vector1
        |> List.zip vector2
        |> List.map (fun (v1, v2) -> v1 * v2)
        |> List.fold (fun s t -> t + s ) (Int 0)

    module S4ComputingActions =
        /// (define ((L-free-particle mass) local)
        /// (let ((v (velocity local)))
        /// (* 1/2 mass (dot-product v v))))
        let inline lagrangianFreeParticle mass local = 
            let v = velocity local
            (mass * (dotProduct v v)) / 2

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
        //let inline gamma (q : UpIndexed list ) (time : Time) =
        let gamma (q : Local ) (time : Time) =
            let coordinate, derivatives =
                q
                |> List.map (fun x -> (UpIndexed.LocalMetric (x time)),  UpIndexed.LocalMetric (fstD x time))
                |> List.unzip
        
            UpIndexed.UpIndexed
                [
                    UpIndexed.LocalMetric time
                    UpIndexed.UpIndexed coordinate
                    UpIndexed.UpIndexed derivatives  
                ]

        /// (define (Lagrangian-action L q t1 t2)
        ///     (definite-integral (compose L (Gamma q)) t1 t2))
        let lagrangianAction lagrangian path time1 time2 =
            //to do
            gamma path
            |> lagrangian
            //|> definiteIntegral

        /// (define (test-path t)
        ///     (up (+ (* 4 t) 7)
        ///         (+ (* 3 t) 5)
        ///         (+ (* 2 t) 1)))
        let testPath : Local  =
            [
                fun (time : Time) -> 4 * time + 7
                fun (time : Time) -> 3 * time + 5
                fun (time : Time) -> 2 * time + 1
            ]

        