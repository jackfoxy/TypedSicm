1       2                                 2
(define (findPath Lagrangian t0 q0 t1 q1 n)
    2    34          5                          543
    (let ((initialQs (linearInterpolants q0 q1 n)))
        3    45            
        (let ((minimizingQs
                6                        
                (multidimensionalMinimize
                    7                                           7
                    (parametricPathAction Lagrangian t0 q0 t1 q1)
                             654
                    initialQs)))
            4                                 4321
            (makePath t0 q0 t1 q1 minimizingQs))))
