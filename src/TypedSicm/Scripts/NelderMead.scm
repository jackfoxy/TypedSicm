(define (nelder-mead f start-pt start-step epsilon maxiter)
  (define shrink-coef 0.5)
  (define reflection-coef 2.0)
  (define expansion-coef 3.0)
  (define contraction-coef-1 1.5)
  (define contraction-coef-2 (- 2 contraction-coef-1))
  (define (simplex-shrink point simplex)
    (let ((pv (simplex-vertex point)))
      (simplex-sort
       (map (lambda (sp)
          (if (eq? point sp) 
          sp
          (let ((vertex ((extender pv (simplex-vertex sp)) 
                 shrink-coef)))
            (simplex-entry vertex (f vertex)))))
        simplex))))
  (define (nm-step simplex)
    (let ((g (simplex-highest simplex))
          (h (simplex-next-highest simplex))
          (s (simplex-lowest simplex))
          (s-h (simplex-but-highest simplex)))
      (let* ((vg (simplex-vertex g)) (fg (simplex-value g))
             (fh (simplex-value h)) (fs (simplex-value s))
             (extend (extender vg (simplex-centroid s-h))))
        (let* ((vr (extend reflection-coef))
               (fr (f vr)))                 ;try reflection
          (if (< fr fh)                     ;reflection successful
              (if (< fr fs)                 ;new minimum 
                  (let* ((ve (extend expansion-coef))
                         (fe (f ve)))       ;try expansion
                    (if (< fe fs)           ;expansion successful
                        (simplex-adjoin ve fe s-h)
                        (simplex-adjoin vr fr s-h)))
                  (simplex-adjoin vr fr s-h))
              (let* ((vc (extend (if (< fr fg) 
                                     contraction-coef-1
                                     contraction-coef-2)))
                     (fc (f vc)))           ;try contraction
                (if (< fc fg)               ;contraction successful
                    (simplex-adjoin vc fc s-h)
                    (simplex-shrink s simplex))))))))
  (define (limit simplex count)
    (if nelder-wallp? (write-line (simplex-lowest simplex)))
    (if (stationary? simplex epsilon)
        (list 'ok (simplex-lowest simplex) count)
        (if (fix:= count maxiter)
            (list 'maxcount (simplex-lowest simplex) count)
            (limit (nm-step simplex) (fix:+ count 1)))))
  (limit (make-simplex start-pt start-step f) 0))