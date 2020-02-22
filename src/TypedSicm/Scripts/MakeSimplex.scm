(define (make-simplex point step f)
(simplex-sort
  (map (lambda (vertex) (simplex-entry vertex (f vertex)))
       (cons point
           (let ((n (vector-length point)))
         (generate-list n
           (lambda (i)
             (vector+vector point
               (scalar*vector step
             (v:make-basis-unit n i))))))))))