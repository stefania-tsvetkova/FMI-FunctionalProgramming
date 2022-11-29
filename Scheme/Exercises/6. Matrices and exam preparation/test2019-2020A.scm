; Problem 1

(define (product-digits n)
  (if (= n 0)
      1
      (* (remainder n 10) (product-digits (quotient n 10)))))

(define (largest-diff a b)
  (define (diff n)
    (- n (product-digits n)))

  (define (largest-diff a b min max)
    (if (> a b)
        (- max min)
        (let ((curr (diff a)))
          (cond
            ((< curr min) (largest-diff (+ a 1) b curr max))
            ((> curr max) (largest-diff (+ a 1) b min curr))
            (else (largest-diff (+ a 1) b min max))))))

  (largest-diff (+ a 1) b (diff a) (diff a)))

; Problem 2

(define (max-metric mm ll)  
  (define (metric m l)
    (if (null? l)
        0
        (+ (m (car l)) (metric m (cdr l)))))

  (define (max-metric mm ll res-m res-sum)
    (if (null? mm)
        res-m
        (let* (
               (curr-m (car mm))
               (curr-sum (metric curr-m ll)))
          (if (> curr-sum res-sum)
              (max-metric (cdr mm) ll curr-m curr-sum)
              (max-metric (cdr mm) ll res-m res-sum)))))

  (max-metric (cdr mm) ll (car mm) (metric (car mm) ll)))
      

; Problem 3

(define (deep-repeat l)
  (define (repeat x n)
    (if (= n 0)
        '()
        (cons x (repeat x (- n 1)))))
  
  (define (deep-repeat l i)
    (if (null? l)
        '()
        (let (
              (head (car l))
              (tail (cdr l)))
          (if (list? head)
              (cons (deep-repeat head (+ i 1)) (deep-repeat tail i))
              (append (repeat head i) (deep-repeat tail i))))))

  (deep-repeat l 1))