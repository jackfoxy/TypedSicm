module TypedSicm.NelderMead

open TypedSicm.Utilities

/// (define nelder-start-step .01)
let nelderStartStep = 0.01 
/// (define nelder-epsilon 1.0e-10)
let nelderEpsilon = 1.0e-10
/// (define nelder-maxiter 1000)
let nelderMaxiter = 1000

/// (define simplex-vertex car)
let simplexVertex = List.head

/// (define simplex-value cdr)
let simplexValue = List.tail

/// ((define simplex-highest car)
let simplexHighest = List.head

/// ((define simplex-but-highest cdr)
let simplexButHighest = List.tail

/// ((define simplex-next-highest cadr) ;"the car of the cdr"
//let simplexNextHighest = List.tail >> List.head

/// (define (simplex-lowest s) (car (last-pair s)))
let simplexLowest s = (List.rev >> List.head) s

/// (define simplex-entry cons)
let simplexEntry = cons

/// (define (make-simplex point step f)
/// (simplex-sort
///   (map (lambda (vertex) (simplex-entry vertex (f vertex)))
///        (cons point
///            (let ((n (vector-length point)))
///          (generate-list n
///            (lambda (i)
///              (vector+vector point
///                (scalar*vector step
///              (v:make-basis-unit n i))))))))))
let makeSimplex point step f =
    ()


/// (define simplex-centroid
/// (lambda (simplex)
///   (scalar*vector (/ 1 (simplex-size simplex))
///            (a-reduce vector+vector
///                  (map simplex-vertex simplex)))))
let simplexCentroid =
    fun simplex ->
        let scalar = 1. / (Seq.length simplex |> float)
        simplex
     //   |> Seq.reduce ()
    ()

/// (define (simplex-add-entry entry s)
///   (let ((fv (simplex-value entry)))
///     (let loop ((s s))
///       (cond ((null? s) (list entry))
///             ((> fv (simplex-value (car s))) (cons entry s))
///             (else (cons (car s) (loop (cdr s))))))))
let simplexAddEntry entry s =
    let fv = List.tail entry
    let rec loop s' =
        if s' = List.empty then
            entry::[]
        elif fv > List.tail s'.Head then
            entry::s'
        else
            s'.Head::(loop s'.Tail)
            
    loop s

/// (define (simplex-sort s)
///   (let lp ((s s) (ans '()))
///     (if (null? s)
///         ans
///         (lp (cdr s) (simplex-add-entry (car s) ans)))))
let simplexSort s =
    let rec loop s ans =
        match s with
        | [] -> ans
        | hd::tl ->
            loop tl (simplexAddEntry hd ans)

    loop s []

/// (define extender
///   (lambda (p1 p2)
///     (let ((dp (vector-vector p2 p1)))
///       (lambda (k)
///         (vector+vector p1 (scalar*vector k dp))))))
let extender =
    fun (p1 : float []) p2 ->
        let dp =
            Array.zip p2 p1
            |> Array.map (fun (p2, p1) -> p2 - p1)
        fun k ->
            let scalarTimesVector scalar vector =
                vector |> Array.map (fun x -> scalar * x)
            Array.zip p1 (scalarTimesVector k dp)
            |> Array.map (fun (p1, p2) -> p1 + p2)


/// (define (nelder-mead f start-pt start-step epsilon maxiter)
///   (define shrink-coef 0.5)
///   (define reflection-coef 2.0)
///   (define expansion-coef 3.0)
///   (define contraction-coef-1 1.5)
///   (define contraction-coef-2 (- 2 contraction-coef-1))
///   (define (simplex-shrink point simplex)
///     (let ((pv (simplex-vertex point)))
///       (simplex-sort
///        (map (lambda (sp)
///           (if (eq? point sp) 
///           sp
///           (let ((vertex ((extender pv (simplex-vertex sp)) 
///                  shrink-coef)))
///             (simplex-entry vertex (f vertex)))))
///         simplex))))
///   (define (nm-step simplex)
///     (let ((g (simplex-highest simplex))
///           (h (simplex-next-highest simplex))
///           (s (simplex-lowest simplex))
///           (s-h (simplex-but-highest simplex)))
///       (let* ((vg (simplex-vertex g)) (fg (simplex-value g))
///              (fh (simplex-value h)) (fs (simplex-value s))
///              (extend (extender vg (simplex-centroid s-h))))
///         (let* ((vr (extend reflection-coef))
///                (fr (f vr)))                 ;try reflection
///           (if (< fr fh)                     ;reflection successful
///               (if (< fr fs)                 ;new minimum 
///                   (let* ((ve (extend expansion-coef))
///                          (fe (f ve)))       ;try expansion
///                     (if (< fe fs)           ;expansion successful
///                         (simplex-adjoin ve fe s-h)
///                         (simplex-adjoin vr fr s-h)))
///                   (simplex-adjoin vr fr s-h))
///               (let* ((vc (extend (if (< fr fg) 
///                                      contraction-coef-1
///                                      contraction-coef-2)))
///                      (fc (f vc)))           ;try contraction
///                 (if (< fc fg)               ;contraction successful
///                     (simplex-adjoin vc fc s-h)
///                     (simplex-shrink s simplex))))))))
///   (define (limit simplex count)
///     (if nelder-wallp? (write-line (simplex-lowest simplex)))
///     (if (stationary? simplex epsilon)
///         (list 'ok (simplex-lowest simplex) count)
///         (if (fix:= count maxiter)
///             (list 'maxcount (simplex-lowest simplex) count)
///             (limit (nm-step simplex) (fix:+ count 1)))))
///   (limit (make-simplex start-pt start-step f) 0))
let nelderMead f startPt startStep epsilon maxiter =
    let shrinkCoef = 0.5
    let reflectionCoef = 2.0
    let expansionCoef = 3.0
    let contractionCoef1 = 1.5
    let contractionCoef2 = 2. - contractionCoef1

    let simplexShrink point simplex =
        let pv = Array.head point
        simplex
        |> Array.map (fun sp ->
            if point = sp then
                sp
            else
                let vertex = (extender pv (Array.head sp)) shrinkCoef
                [|Array.append vertex (f vertex)|]
        )
        |> Array.map (fun xs -> 
            xs
            |> Array.map (fun ys -> Array.toList ys)
            |> Array.toList
        )
        |> Array.toList
        |> simplexSort

    let nmStep simplex =
        let g = simplexHighest simplex
        //let h = simplexNextHighest simplex
        let s = simplexLowest simplex
        let sH = simplexButHighest simplex
        let vg = simplexVertex g
        let fg = simplexValue g
        //let fh = simplexValue h
        let fs = simplexValue s
        //let extend = extender vg (simplexCentroid sH)
        //let vr = extend reflectionCoef   
        ()
        //let fr = f vr                 //try reflection
                                      
        //if fr < fh then                  //reflection successful                         
        //      if fr < fs  then               //new minimum 
        //          let ve = extend expansionCoef              
        //          let fe = f ve      //try expansion
                  
        //          if fe < fs then           //expansion successful
        //                simplexAdjoin ve fe sH
        //          else
        //                simplexAdjoin vr fr sH
        //      else
        //          simplexAdjoin vr fr sH

        //else
        //      let vc = extend (if fr < fg then contractionCoef1 else contractionCoef2)                        
        //      let fc = f vc          //try contraction                                 
        //      if fc < fg then              //contraction successful
        //            simplexAdjoin vc fc sH
        //      else
        //            simplexShrink s simplex



    ()


/// (define (multidimensional-minimize f parameters)
///   (let ((f (compose f vector->list)))
///     (let ((result
///        (nelder-mead f
///             (list->vector parameters)
///             nelder-start-step
///             nelder-epsilon
///             nelder-maxiter)))
///       (if (eq? 'ok (car result))
///       (vector->list (caadr result))
///       (error "Minimizer did not converge" result)))))

let multidimensionalMinimize f parameters : Scalar [] =
  // let f = compose f vector->list
   [||]