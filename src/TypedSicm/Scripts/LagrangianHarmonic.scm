(define ((L-harmonic m k) local)
  (let ((q (coordinate local))
      (v (velocity local)))
    (- (* 1/2 m (square v)) (* 1/2 k (square q)))))
