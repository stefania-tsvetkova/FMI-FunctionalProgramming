;; automorphic? n - когато повдигнем n на квадрат дали завършва със себе си
(define (automorphic? n)
  (define (ends-with? square n)
    (if (= (quotient n 10) 0)
        (= (remainder n 10) (remainder square 10))
        (and (= (remainder n 10) (remainder square 10)) (ends-with? (quotient square 10) (quotient n 10)))))

  (ends-with? (* n n) n))

;; accumulate
(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))))

;; accumulate-iter
(define (accumulate-iter op nv a b term next)
  (if (> a b)
      nv
      (accumulate-iter op (op nv (term a)) (next a) b term next)))
        
;; 1. repeat f n - композиция на f n пъти в себе си
;; ???
(define (repeat f n)
  (if (= n 0)
      f
      (f (repeat f (- n 1)))))

(define (square x) (* x x))

;; 2. factorial - използвайки accumulate
(define (factorial n)
  (define (id x) x)
  (define (1+ x) (+ 1 x))
  (define (product a b) (* a b))
  (accumulate product 1 1 n id 1+))

;; 3. all? a b pred - проверка дали всички числа в интервала [a, b] удовлетворяват предиката pred
;; ???
;(define (all? a b pred)
;  (define (1+ x) (+ 1 x))
;  (aaccumulate and #t a b pred 1+))

;; 4. any? a b pred - проверка дали някой елемент в интервала удовлетворява предиката pred


;; 5. count-primes a b - брои колко прости числа има в интервала
(define (count-primes a b)
  (define (1+ x) (+ 1 x))
    
  (define (is-prime? x)
    (define (is-prime? x i)
      (cond
        ((= i x) 1)
        ((= (remainder x i) 0) 0)
        (else (is-prime? x (1+ i)))))
    (if (= x 1)
        0
        (is-prime? x 2)))

  (accumulate + 0 a b is-prime? 1+))

;; 6. sum-roots a b c - намира сбора на корените на квадратно уравнение с параметри a, b и c
(define (sum-roots a b c)
  (let* (
      (D (- (* b b) (* 4 a c)))
      (x1 (/ (- (- b) (sqrt D)) (* 2 a)))
      (x2 (/ (+ (- b) (sqrt D)) (* 2 a))))

    (+ x1 x2)))