(define (lagrange-interpolation-function ys xs)
  (let ((n (length ys)))
    (assert (fix:= (length xs) n))
    (define (poly x)
      (reduce + :zero
          (generate-list n
        (lambda (i)
          (/ (reduce * :one
               (generate-list n
                 (lambda (j)
               (if (fix:= j i)
                   (list-ref ys i)
                   (- x (list-ref xs j))))))
             (let ((xi (list-ref xs i)))
               (reduce * :one
             (generate-list n
                   (lambda (j)
                 (cond ((fix:< j i) (- (list-ref xs j) xi))
                   ((fix:= j i) (expt :-one i))
                   (else    (- xi (list-ref xs j)))))))))))))
    poly))