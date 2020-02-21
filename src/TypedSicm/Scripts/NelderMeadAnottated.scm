1       2                                              2
(define (nelderMead f startPt startStep epsilon maxiter)
  2                     2
  (define shrinkCoef 0.5)
  2                         2
  (define reflectionCoef 2.0)
  2                        2
  (define expansionCoef 3.0)
  2                           2
  (define contractionCoef1 1.5)
  2                        3                   32
  (define contractionCoef2 ( 2 contractionCoef1))
  2       3                           3
  (define (simplexShrink point simplex)
    3    45   6                   654
    (let ((pv (simplexVertex point)))
      4           
      (simplexSort
       5    6       7  7
       (map (lambda (sp)
          7   8            8 
          (if (eq? point sp) 
            
          sp
          8    9A       BC            D                DC 
          (let ((vertex ((extender pv (simplexVertex sp)) 
                           BA9
                 shrinkCoef)))
            9                    A        A9876
            (simplexEntry vertex (f vertex)))))
               5432
        simplex))))
  2       3              3
  (define (nmStep simplex)
    3    45  6                      65
    (let ((g (simplexHighest simplex))
          5  6                          65
          (h (simplexNextHighest simplex))
          5  6                     65
          (s (simplexLowest simplex))
          5   6                         654
          (sH (simplexButHighest simplex)))
      4     56   7               76 6   7              76
      (let* ((vg (simplexVertex g)) (fg (simplexValue g))
             6   7              76 6   7              76
             (fh (simplexValue h)) (fs (simplexValue s))
             6       7            8                  8765
             (extend (extender vg (simplexCentroid sH))))
        5     67   8                     87
        (let* ((vr (extend reflectionCoef))
               7   8    876                                
               (fr (f vr)))                 ;try reflection
          6   7       7                                           
          (if (< fr fh)                     ;reflection successful
              7   8       8                              
              (if (< fr fs)                 ;new minimum 
                  8     9A   B                    BA
                  (let* ((ve (extend expansionCoef))
                         A   B    BA9                     
                         (fe (f ve)))       ;try expansion
                    9   A       A                                
                    (if (< fe fs)           ;expansion successful
                        A                      A
                        (simplexAdjoin ve fe sH)
                        A                      A98
                        (simplexAdjoin vr fr sH)))
                  8                      87
                  (simplexAdjoin vr fr sH))
              7     89   A       B   C       C 
              (let* ((vc (extend (if (< fr fg) 
                                                     
                                     contractionCoef1
                                                     BA9
                                     contractionCoef2)))
                     9   A    A98                           
                     (fc (f vc)))           ;try contraction
                8   9       9                                      
                (if (< fc fg)               ;contraction successful
                    9                      9
                    (simplexAdjoin vc fc sH)
                    9                       98765432
                    (simplexShrink s simplex))))))))
  2       3                   3
  (define (limit simplex count)
    3                4          5                     543
    (if nelderWallp? (writeLine (simplexLowest simplex)))
    3   4                           4
    (if (stationary? simplex epsilon)
        4         5                     5      4
        (list 'ok (simplexLowest simplex) count)
        4   5                   5
        (if (fix:= count maxiter)
            5               6                     6      5
            (list 'maxcount (simplexLowest simplex) count)
            5      6              6 6             65432
            (limit (nmStep simplex) (fix:+ count 1)))))
  2      3                               3  21
  (limit (makeSimplex startPt startStep f) 0))
