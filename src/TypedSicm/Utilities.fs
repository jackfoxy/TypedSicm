module TypedSicm.Utilities

open System
open FSharpx.Collections
open GenericArithmetic

module Vector = RandomAccessList

/// (define (v:make-basis-unit n i)	; #(0 0 ... 1 ... 0) n long, 1 in ith position
///  (v:generate n (lambda (j) (if (fix:= j i) :one :zero))))
let makeBasisUnit n i =
    n |> fun j ->
            if j = i then Scalar.Int 1
            else Scalar.Int 0

/// (define *machine-epsilon*
///   (let loop ((e 1.0))
///      (if (= 1.0 (+ e 1.0))
///          (* 2 e)
///          (loop (/ e 2)))))
let machineEpsilon =
    // stackoverflow when running in FSI
    // = 2.220446049250313e-016
    let rec loop e =
        if 1.0 = (e + 1.0) then
            2.0 * e
        else
            loop (e / 2.0)

    loop 1.0

type MinimizeResult =
    {
        Argument : float
        Minimum : float
        Iterations : int
    }

/// (define simplex-entry cons)
let private privateCons (x : Scalar)  y = Vector.cons x y
let cons = privateCons

/// (define *sqrt-machine-epsilon* 
///   (sqrt *machine-epsilon*))
/// 
/// ;;; Brent's algorithm for univariate minimization -- transcribed from
/// ;;; pages 79-80 of his book "Algorithms for Minimization Without Derivatives"
/// 
/// (define (brent-min f a b eps)
///   (let ((a (min a b)) (b (max a b))
///     (maxcount 100)
///     (small-bugger-factor *sqrt-machine-epsilon*)
///     (g (/ (- 3 (sqrt 5)) 2))
///     (d 0) (e 0) (old-e 0) (p 0) (q 0) (u 0) (fu 0))
///     (let* ((x (+ a (* g (- b a))))
///        (fx (f x))
///        (w x) (fw fx) (v x) (fv fx))
///       (let loop ((count 0))
///     (if (> count maxcount)
///         (list 'maxcount x fx count) ;failed to converge
///         (let* ((tol (+ (* eps (abs x)) small-bugger-factor))
///            (2tol (* 2 tol))
///            (m (/ (+ a b) 2)))
///           ;; test for convergence
///           (if (< (max (- x a) (- b x)) 2tol)
///           (list x fx count)
///           (begin
///             (if (> (abs e) tol)
///             (let* ((t1 (* (- x w) (- fx fv)))
///                    (t2 (* (- x v) (- fx fw)))
///                    (t3 (- (* (- x v) t2) (* (- x w) t1)))
///                    (t4 (* 2 (- t2 t1))))
///               (set! p (if (positive? t4) (- t3) t3))
///               (set! q (abs t4))
///               (set! old-e e)
///               (set! e d)))
///             (if (and (< (abs p) (abs (* 0.5 q old-e)))
///                  (> p (* q (- a x)))
///                  (< p (* q (- b x))))
///             ;; parabolic step
///             (begin (set! d (/ p q))
///                    (set! u (+ x d))
///                    (if (< (min (- u a) (- b u)) 2tol)
///                    (set! d (if (< x m) tol (- tol)))))
///             ;;else, golden section step
///             (begin (set! e (if (< x m) (- b x) (- a x)))
///                    (set! d (* g e))))
///             (set! u (+ x (if (> (abs d) tol) 
///                      d
///                      (if (positive? d) tol (- tol)))))
///             (set! fu (f u))
///             (if (<= fu fx)
///             (begin (if (< u x) (set! b x) (set! a x))
///                    (set! v w) (set! fv fw)
///                    (set! w x) (set! fw fx)
///                    (set! x u) (set! fx fu))
///             (begin (if (< u x) (set! a u) (set! b u))
///                    (if (or (<= fu fw) (= w x))
///                    (begin (set! v w) (set! fv fw)
///                       (set! w u) (set! fw fu))
///                    (if (or (<= fu fv) (= v x) (= v w))
///                        (begin (set! v u) (set! fv fu))))))
///             (loop (+ count 1))))))))))
let brentMin f (a' : float) b' eps =
    let mutable a = Math.Min(a', b')
    let mutable b = Math.Max(a', b')
    let maxcount = 100
    let smallBuggerFactor = Math.Sqrt(machineEpsilon)
    let g = (3. - Math.Sqrt(5.)) / 2.

    let mutable d = 0.
    let mutable e = 0.
    let mutable old_e = 0.
    let mutable p = 0.
    let mutable q = 0.
    let mutable u = 0.
    let mutable fu = 0.

    let mutable x = a + g * (b - a)
    let mutable fx = f x
    let mutable w = x
    let mutable fw = fx
    let mutable v = x
    let mutable fv = fx

    let rec loop count =
        if count > maxcount then
            {
                Argument = x
                Minimum = fx
                Iterations = count
            } |> Error

        else
            let tol = eps * (Math.Abs x) + smallBuggerFactor
            let tol2 = 2.0 * tol
            let m = (a + b) / 2.0

            match Math.Max((x - a), (b - x)) < tol2 with
            | true ->
                {
                    Argument = x
                    Minimum = fx
                    Iterations = count
                } |> Ok

            | false ->
                if Math.Abs(e) > tol then
                    let t1 = (x - w) * (fx - fv)
                    let t2 = (x - v) * (fx - fw)
                    let t3 = ((x - v) * t2) - ((x - w) * t1)
                    let t4 = 2. * (t2 - t1)

                    p <- if t4 >= 0. then -t3 else t3
                    q <-  Math.Abs(t4)
                    old_e <- e
                    e <- d

                if Math.Abs(p) < Math.Abs (0.5 * q * old_e)
                    && p > q * (a - x)
                    && p < q * (b - x) then

                    // parabolic step
                    d <- p / q
                    u <- x + d
                    if Math.Min((u - a), (b - u)) < tol2 then 
                        d <- (if x < m then tol else -tol)
                else
                    // golden section step
                    e <- (if x < m then b - x else a - x)
                    d <- g * e

                u <- x + ( if Math.Abs(d) > tol then
                                d 
                            else
                                if d >= 0. then tol else -tol )

                fu <- f u

                if fu <= fx then
                    if u < x then b <- x else a <- x
                    v <- w
                    fv <- fw
                    w <- x
                    fw <- fx
                    x <- u
                    fx <- fu
                else
                    if u < x then 
                        a <- u
                    else 
                        b <- u

                    if ( fu <= fw) || ( w = x) then
                        v <- w
                        fv <-fw
                        w <-u
                        fw <-fu
                    else 
                        if ( fu <= fv) || ( v = x) || (v =  w) then
                            v <- u 
                            fv <- fu

                loop (count + 1)
    loop 0

/// (define (minimize f lowx highx)
///      (define brent-error 1.0e-5)
///        (brent-min f lowx highx brent-error))

let minimize f lowx highx =
    let brentError = 1.0e-5
    brentMin f lowx highx brentError

/// (define (linear-interpolants x0 x1 n)
///   (let ((dx (- x1 x0)) (n+1 (fix:+ n 1)))
///     (let lp ((i 1) (xs '()))
///       (if (fix:> i n)
///       (reverse xs)
///       (lp (fix:+ i 1)
///           (cons (+ x0 (/ (* i dx) n+1)) xs))))))
let linearInterpolants (x0 : Scalar) (x1 : Scalar) n =
    let dx = x1 - x0
    let sucN = n + 1
    let rec loop i (xs : RandomAccessList<Indexable>) =
        if i > n then
            Vector.rev xs
        else
            loop  (i + 1) (Vector.cons (Indexable.Scalar (x0 + (i * dx) / sucN)) xs)

    loop 1 Vector.empty<Indexable>

/// define (generate-list n proc) ; ==> ( (proc 0) (proc 1) ... (proc n-1) )
///   (let loop ((i (fix:- n 1)) (list '()))
///     (if (fix:< i 0)
///         list
///         (loop (fix:- i 1) (cons (proc i) list)))))
let generateList n proc =
    let rec loop xs i =
        if i < 0 then
            xs
        else
            loop (Vector.cons (proc i) xs) (i - 1) 

    loop Vector.empty (n - 1)

/// (define (lagrange-interpolation-function ys xs)
///   (let ((n (length ys)))
///     (assert (fix:= (length xs) n))
///     (define (poly x)
///       (reduce + :zero
///           (generate-list n
///         (lambda (i)
///           (/ (reduce * :one
///                (generate-list n
///                  (lambda (j)
///                (if (fix:= j i)
///                    (list-ref ys i)
///                    (- x (list-ref xs j))))))
///              (let ((xi (list-ref xs i)))
///                (reduce * :one
///              (generate-list n
///                    (lambda (j)
///                  (cond ((fix:< j i) (- (list-ref xs j) xi))
///                    ((fix:= j i) (expt :-one i))
///                    (else    (- xi (list-ref xs j)))))))))))))
///     poly))
let lagrangeInterpolation (ys : UpIndexed) (xs : UpIndexed) =
    let n = ys.Length
    if xs.Length <> n then
        invalidArg "lagrangeInterpolation" "lists are not same length"
    let poly x =
        Vector.reduce (+)
            <| generateList n
                ( fun i ->
                    let dividend =
                        Vector.reduce (*) 
                            <| generateList n
                                (fun (j : int) ->
                                    if j = i then 
                                        scalarOrFuncToScalar ys.[i]
                                    else 
                                        x - scalarOrFuncToScalar xs.[j] )
                    let divisor =
                        let xi = xs.[i]
                        Vector.reduce (*) 
                            <| generateList n
                                (fun j ->
                                    if j < i then 
                                        scalarOrFuncToScalar (xs.[j] - xi) 
                                    elif j = i then 
                                        (-1. ** (float i)) 
                                        |> Scalar.Float
                                    else 
                                        scalarOrFuncToScalar (xi - xs.[j]) )
                    dividend / divisor )
    poly

/// (define (make-path t0 q0 t1 q1 qs)
/// (let ((n (length qs)))
///     (let ((ts (linear-interpolants t0 t1 n)))
///       (Lagrange-interpolation-function
///        (append (list q0) qs (list q1))
///        (append (list t0) ts (list t1))))))
let makePath (t0 : Time) q0 (t1 : Time) q1 (qs : UpIndexed) =
    let ts = linearInterpolants t0 t1 qs.Length

    let q0 = Indexable.Scalar q0
    let q1 = Indexable.Scalar q1
    let t0 = Indexable.Scalar t0
    let t1 = Indexable.Scalar t1

    [| 
        lagrangeInterpolation
            (Vector.cons q0 (vectorConj q1 qs))
            (Vector.cons t0 (vectorConj t1 ts)) 
        |> indexableFunc
    |] |> Vector.ofSeq
