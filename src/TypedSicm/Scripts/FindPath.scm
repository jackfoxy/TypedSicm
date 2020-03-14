(define (find-path Lagrangian t0 q0 t1 q1 n)
    (let ((initial-qs (linear-interpolants q0 q1 n)))
        (let ((minimizing-qs
                (multidimensional-minimize
                    (parametric-path-action Lagrangian t0 q0 t1 q1)
                    initial-qs)))
            (make-path t0 q0 t1 q1 minimizing-qs))))