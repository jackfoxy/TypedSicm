1       2                       2
(define (simplexAddEntry entry s)
  2    34   5                  543
  (let ((fv (simplexValue entry)))
    3         45   54
    (let loop ((s s))
      4     56       6 6          65
      (cond ((null? s) (list entry))
            56     7             8     876 6            65
            ((> fv (simplexValue (car s))) (cons entry s))
            5     6     7     7 7     8     87654321
            (else (cons (car s) (loop (cdr s))))))))
