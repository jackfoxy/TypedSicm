1       2                                     2
(define (multidimensionalMinimize f parameters)
  2    34  5                     543
  (let ((f (compose f vector>list)))
    3    45      
    (let ((result
       6            
       (nelderMead f
            7                      7
            (list>vector parameters)
                           
            nelderStartStep
                         
            nelderEpsilon
                         654
            nelderMaxiter)))
      4   5        6          65
      (if (eq? 'ok (car result))
      5            6            65
      (vector>list (caadr result))
      5                                         54321
      (error "Minimizer did not converge" result)))))
