(define (multidimensional-minimize f parameters)
  (let ((f (compose f vector->list)))
    (let ((result
       (nelder-mead f
            (list->vector parameters)
            nelder-start-step
            nelder-epsilon
            nelder-maxiter)))
      (if (eq? 'ok (car result))
      (vector->list (caadr result))
      (error "Minimizer did not converge" result)))))