(define (foldr op nv l)
  (if (null? l) nv
          (op (car l) (foldr op nv (cdr l)))))

; my-flatten
(define (my-flatten l)
  (foldr (lambda (el res) (if (list? el) (append (my-flatten el) res) (cons el res))) '() l))

; sum-of-products ll
(define (sum-of-products ll)
  (define (prod l)
    (if (null? l)
        1
        (* (car l) (prod (cdr l)))))
  
  (if(null? ll)
     0
    (let (
          (head (car ll))
          (tail (cdr ll)))
      (+ (prod head) (sum-of-products tail)))))

; dimentions
(define (dimentions m)
  (cons (length m) (if (null? m) 0 (length (car m)))))

; main-diagonal
(define (main-diagonal m)
  (define (ith-element l i)
    (if (= i 0)
        (car l)
        (ith-element (cdr l) (- i 1))))
  
  (define (main-diagonal m i)
    (if (null? m)
        '()
        (cons (ith-element (car m) i) (main-diagonal (cdr m) (+ i 1)))))

  (main-diagonal m 0))

; second-diagonal
(define (second-diagonal m)
  (define (ith-element l i)
    (if (= i 0)
        (car l)
        (ith-element (cdr l) (- i 1))))
  
  (define (second-diagonal m i)
    (if (null? m)
        '()
        (cons (ith-element (car m) i) (second-diagonal (cdr m) (- i 1)))))

  (second-diagonal m (- (length m) 1)))

; nth-row
(define (nth-row m n)
  (cond
    ((null? m) m)
    ((= n 0) (car m))
    (else (nth-row (cdr m) (- n 1)))))

; nth-column
(define (nth-column m n)
  (define (ith-element l i)
    (if (= i 0)
        (car l)
        (ith-element (cdr l) (- i 1))))
  
  (if (null? m)
      m
      (cons (ith-element (car m) n) (nth-column (cdr m) n))))

; transpose
(define (transpose m)
  (let (
        (columns (cdr (dimentions m))))
    (define (transpose m i)
      (if (= i columns)
          '()
          (cons (nth-column m i) (transpose m (+ i 1)))))

    (transpose m 0)))

; skip-nth-row
(define (skip-nth-row m n)
  (define (skip-nth-row m i)
    (cond
      ((< i n) (cons (car m) (skip-nth-row (cdr m) (+ i 1))))
      ((= i n) (cdr m))))

  (skip-nth-row m 0))

; skip-nth-column
(define (skip-nth-column m n)
  (define (skip-nth-element l i)
    (if (= i n)
        (cdr l)
        (cons (car l) (skip-nth-element (cdr l) (+ i 1)))))

  (if (null? m)
      '()
      (cons (skip-nth-element (car m) 0) (skip-nth-column (cdr m) n))))

; sc-multiply
(define (sc-multiply m x)
  (define (multiply-list l)
    (if (null? l)
        l
        (cons (* x (car l)) (multiply-list (cdr l)))))

  (if (null? m)
      m
      (cons (multiply-list (car m)) (sc-multiply (cdr m) x))))

; multiply
(define (multiply a b)
  (define (multiply-list a b)
    (if (null? a)
        a
        (cons (* (car a) (car b)) (multiply-list (cdr a) (cdr b)))))
                         
  (define (multiply a b i)
    (if (= i (length a))
        '()
        (cons (multiply-list (nth-row a i) (nth-column b i)) (multiply a b (+ i 1)))))

  (multiply a b 0))