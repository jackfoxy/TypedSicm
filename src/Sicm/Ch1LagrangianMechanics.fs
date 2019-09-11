namespace Sicm

open System.Numerics

/// Chapter 1, Lagrangian Mechanics
module LagrangianMechanics = 

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
    let inline dotProduct (vector1 : array<LocalMetric>)  (vector2 : array<LocalMetric>)  =
        vector1
        |> Array.zip vector2
        |> Array.map (fun (v1, v2) -> v1 * v2)
        |> Array.fold (fun s t -> t + s ) (Int 0)

    module S4ComputingActions =
        /// (define ((L-free-particle mass) local)
        /// (let ((v (velocity local)))
        /// (* 1/2 mass (dot-product v v))))
        let inline lagrangianFreeParticle mass local = 
            let v = velocity local
            (mass * (dotProduct v v)) / 2
