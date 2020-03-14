module TypedSicm.NelderMead

open FSharpx.Collections
open Utilities
open GenericArithmetic

module Vector = RandomAccessList

type Point = RandomAccessList<Indexable>
type SimplexEntry = Point * Scalar
type Simplex = RandomAccessList<SimplexEntry>

/// (define nelder-start-step .01)
let nelderStartStep = 0.01 
/// (define nelder-epsilon 1.0e-10)
let nelderEpsilon = 1.0e-10
/// (define nelder-maxiter 1000)
let nelderMaxiter = 1000

/// (define simplex-vertex car)
let simplexVertex = fst

/// (define simplex-value cdr)
let simplexValue = snd

/// ((define simplex-highest car)
let simplexHighest = Vector.head

/// ((define simplex-but-highest cdr)
let simplexButHighest = Vector.tail

/// ((define simplex-next-highest cadr) ;"the car of the cdr"
let simplexNextHighest = Vector.tail >> Vector.head

/// (define (simplex-lowest s) (car (last-pair s)))
let simplexLowest s = (Vector.rev >> Vector.head) s

/// (define simplex-entry cons)
let simplexEntry point scalar = point, scalar

/// (define (simplex-add-entry entry s)
///   (let ((fv (simplex-value entry)))
///     (let loop ((s s))
///       (cond ((null? s) (list entry))
///             ((> fv (simplex-value (car s))) (cons entry s))
///             (else (cons (car s) (loop (cdr s))))))))
let simplexAddEntry (entry : SimplexEntry) s =
    let fv = simplexValue entry

    let rec loop (s' : Simplex) =
        if s'.IsEmpty then
            [|entry|] |> Vector.ofSeq
        elif fv > (simplexValue s'.Head) then
            Vector.cons entry s'
        else
            Vector.cons s'.Head (loop s'.Tail)
            
    loop s

/// (define (simplex-sort s)
///   (let lp ((s s) (ans '()))
///     (if (null? s)
///         ans
///         (lp (cdr s) (simplex-add-entry (car s) ans)))))
let simplexSort s =
    let rec loop (s : Simplex) ans =
        match s with
        | Vector.Nil -> ans
        | Vector.Cons  (hd, tl) ->
            loop tl (simplexAddEntry hd ans)

    loop s Vector.empty<SimplexEntry>

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
//let makeSimplex (point : RandomAccessList<Scalar>) (step : Scalar) (f : RandomAccessList<Scalar> -> Scalar) =
let makeSimplex (point : Point) step f =
    let n = point.Length

    let generatedList =
        generateList n (fun i -> point + (makeBasisUnit n i * step) )

    Vector.cons point generatedList    //to do:
    |> Vector.map (fun vertex -> vertex, (f vertex) )
    |> simplexSort

/// (define simplex-centroid
/// (lambda (simplex)
///   (scalar*vector (/ 1 (simplex-size simplex))
///            (a-reduce vector+vector
///                  (map simplex-vertex simplex)))))
let simplexCentroid =
    fun (simplex : Simplex) ->
        let scalar = Scalar.Float 1. / simplex.Length
        scalar * (Vector.reduce (+) <| Vector.map simplexVertex simplex)

/// (define extender
///   (lambda (p1 p2)
///     (let ((dp (vector-vector p2 p1)))
///       (lambda (k)
///         (vector+vector p1 (scalar*vector k dp))))))
let extender =
    fun (p1 : Point) (p2 : Point) ->
        let dp = p2 - p1
        fun k -> (p1 + (dp * k)) :> Point

/// (define (simplex-adjoin v fv s)
///   (simplex-add-entry (simplex-entry v fv) s))
let simplexAdjoin v fv s =
    simplexAddEntry (simplexEntry v fv) s

/// (define (stationary? simplex epsilon)
///   (close-enuf? (simplex-value (simplex-highest simplex))
///                (simplex-value (simplex-lowest simplex))
///                epsilon))

let isStationary (simplex : Simplex) (epsilon : float) =
    let diff = (simplexValue (simplexHighest simplex)) - simplexValue (simplexLowest simplex)

    let epsilon' = Scalar.Float epsilon

    if diff < epsilon' && diff > (-1. * epsilon') then
        true
    else
        false

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
let nelderMead (f : Point -> Scalar) startPt startStep epsilon maxiter =
    let shrinkCoef = Scalar.Float 0.5
    let reflectionCoef = Scalar.Float 2.0
    let expansionCoef = Scalar.Float 3.0
    let contractionCoef1 = Scalar.Float 1.5
    let contractionCoef2 = 2. - contractionCoef1

    let simplexShrink point (simplex : Simplex) =
        let pv = (simplexVertex point)
        simplex
        |> Vector.map (fun sp ->
            if Vector.structuralEquals (simplexVertex point) (simplexVertex sp) then 
                sp
            else
                let vertex = (extender pv (simplexVertex sp)) shrinkCoef
                vertex, (f vertex)
        )
        |> simplexSort

    let nmStep (simplex : Simplex) =
        let g = simplexHighest simplex
        let h = simplexNextHighest simplex
        let s = simplexLowest simplex
        let sH = simplexButHighest simplex
        let vg = simplexVertex g
        let fg = simplexValue g
        let fh = simplexValue h
        let fs = simplexValue s
        let extend = extender vg (simplexCentroid sH)
        let vr = extend reflectionCoef   
        let fr = f vr                
                                      
        if fr < fh then                  //reflection successful                         
            if fr < fs  then               //new minimum 
                let ve = extend expansionCoef              
                let fe = f ve      //try expansion
                  
                if fe < fs then           //expansion successful
                    simplexAdjoin ve fe sH
                else
                    simplexAdjoin vr fr sH
            else
                simplexAdjoin vr fr sH
        else
            let vc = extend (if fr < fg then contractionCoef1 else contractionCoef2)                        
            let fc = f vc          //try contraction                                 
            if fc < fg then              //contraction successful
                simplexAdjoin vc fc sH
            else
                simplexShrink s simplex

    let rec limit simplex count =
        if false then printfn "%A" (simplexLowest simplex)
        if isStationary simplex epsilon then
            Ok ((simplexLowest simplex), count)
        else
            if count = maxiter then
                Error ("max iterations", (simplexLowest simplex), count)
            else 
                limit (nmStep simplex) (count + 1)

    limit (makeSimplex startPt startStep f) 0

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

let multidimensionalMinimize (f : UpIndexed -> float) parameters =
    let f' = 
        fun p -> f p |> Scalar.Float

    match nelderMead f' parameters nelderStartStep nelderEpsilon nelderMaxiter with
    | Ok ((point, _), _) ->
        point
    | Error (_, best, count) ->
        failwith <| sprintf "multidimensionalMinimize failed max iterations %i %A" count best
