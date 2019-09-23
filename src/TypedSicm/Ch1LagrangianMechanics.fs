namespace TypedSicm

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
        let inline lagrangianFreeParticle (mass : LocalMetric) local = 
                let v = 
                    velocity local
                    |> List.map (fun x -> LocalMetric.Float x )
                    
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
                |> List.map (fun x -> UpIndexed.Func1 x,  wrapFloatFunction (derivitave x) )
                |> List.unzip

            let d = UpIndexed.UpIndexed derivatives

            {
                Time = UpIndexed.LocalMetric time
                Local = UpIndexed.UpIndexed coordinate
                Dt = [d]
            }

        /// (define (Lagrangian-action L q t1 t2)
        ///     (definite-integral (compose L (Gamma q)) t1 t2))
        let lagrangianAction (lagrangian : State -> LocalMetric) path time1 time2 =
            let f =
                fun time ->
                    lagrangian (gamma path time) 
                |> definiteIntegral
            f time1 time2

        /// (define (test-path t)
        ///     (up (+ (* 4 t) 7)
        ///         (+ (* 3 t) 5)
        ///         (+ (* 2 t) 1)))
        let testPath : Local  =
            [
                //fun (time : Time) -> 4 * time + 7
                //fun (time : Time) -> 3 * time + 5
                //fun (time : Time) -> 2 * time + 1
                fun (time : Time) -> 4. * (localMetricToFloat time) + 7. |> Float
                fun (time : Time) -> 3. * (localMetricToFloat time) + 5. |> Float
                fun (time : Time) -> 2. * (localMetricToFloat time) + 1. |> Float
            ]

        let test () = lagrangianAction (lagrangianFreeParticle (LocalMetric.Int 3)) testPath 0. 10.
        