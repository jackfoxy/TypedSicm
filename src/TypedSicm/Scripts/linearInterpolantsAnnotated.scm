1       2                          2
(define (linearInterpolants x0 x1 n)
  2    34   5      54 4    5         543
  (let ((dx ( x1 x0)) (n+1 (fix:+ n 1)))
    3       45   5 5    6654
    (let lp ((i 1) (xs '()))
      4   5         5
      (if (fix:> i n)
      5          5
      (reverse xs)
      5   6         6
      (lp (fix:+ i 1)
          6     7     8  9      9    87   654321
          (cons (+ x0 (/ (* i dx) n+1)) xs))))))
