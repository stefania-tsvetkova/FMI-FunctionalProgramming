(define (accumulate op nv a b term next)
  (if (> a b) nv
      (op (term a)
          (accumulate op nv (next a) b term next))))

(define (accumulate-i op nv a b term next)
    (if (> a b) nv
        (accumulate-i op (op nv (term a)) (next a) b term next)))

; Problem 1
(define (argmin f a b)
  (define (operation res curr)
    (if (< (f curr) (f res))
        curr
        res))

  (define (id x) x)

  (define (1+ x) (+ 1 x))
  
  (accumulate-i operation (f a) a b id 1+))

; Problem 2
(define (best-pair a b)
  (define (evaluate-pair p)
    (define (dividers-count x i)
      (cond
        ((= i x) 1)
        ((= (remainder x i) 0) (+ 1 (dividers-count x (+ i 1))))
        (else (dividers-count x (+ i 1)))))

    (dividers-count (+ (car p) (cdr p)) 1))

  (define (1+ x) (+ 1 x))
  
  (define (pair-better-than p1 p2)
    (> (evaluate-pair p1) (evaluate-pair p2)))

  (define (choose-pair res curr)
    (if (pair-better-than curr res)
        curr
        res))

  (define (term n)
    (define (make-pair x) (cons x n))
    
    (accumulate-i choose-pair (cons a n) (1+ a) (- n 1) make-pair 1+))

  (accumulate-i choose-pair (cons a (1+ a)) a b term 1+))

; Problem 3
(define (integrate2 f a b c d dx dy)
  (define (integrate a b f dx)
    (* dx (accumulate + 0 a b f (lambda (x) (+ x dx)))))

  (define (inner-integrate y)
    (integrate a b (lambda (x) (f x y)) dx))
  
  (* dy (accumulate + 0 c d inner-integrate (lambda (y) (+ y dy)))))

; Problem 4 - option 1
(define (n-rooks has-figure n)
  (define (1+ x) (+ x 1))

  (define (row-has-1-figure x)
     (= (accumulate + 0 0 (- n 1) (lambda (y) (if (has-figure x y n) 1 0)) 1+) 1))

  (define (column-has-1-figure y)
     (= (accumulate + 0 0 (- n 1) (lambda (x) (if (has-figure x y n) 1 0)) 1+) 1))

  (define (&& a b) (and a b))
  
  (and
   (accumulate && #t 0 (- n 1) row-has-1-figure 1+)
   (accumulate && #t 0 (- n 1) column-has-1-figure 1+)))
  