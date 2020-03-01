1       2                                   2
(define (lagrangeInterpolationFunction ys xs)
  2    34  5         543
  (let ((n (length ys)))
    3       4      5         5  43
    (assert (fix:= (length xs) n))
    3       4      4
    (define (poly x)
      4              
      (reduce + :zero
          5              
          (generateList n
        6       7 7
        (lambda (i)
          7  8             
          (/ (reduce * :one
               9              
               (generateList n
                 A       B B
                 (lambda (j)
               B   C         C
               (if (fix:= j i)
                   C            C
                   (listRef ys i)
                   C   D            DCBA98
                   ( x (listRef xs j))))))
             8    9A   B            BA9
             (let ((xi (listRef xs i)))
               9             
               (reduce * :one
             A              
             (generateList n
                   B       C C
                   (lambda (j)
                 C     DE         E E F            F   ED
                 (cond ((fix:< j i) ( (listRef xs j) xi))
                   DE         E E           ED
                   ((fix:= j i) (expt :One i))
                   D        E    F            FEDCBA9876543
                   (else    ( xi (listRef xs j)))))))))))))
        21
    poly))
