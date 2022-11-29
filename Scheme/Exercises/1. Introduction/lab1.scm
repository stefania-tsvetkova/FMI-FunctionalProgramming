; ((5+7)*4-11*(92-3))/2
(define sum1 (/ (- (* (+ 5 7) 4) (* 11 (- 92 3))) 2))

; >=
(define (gr-or-equal a b)
  (not (< a b)))

; abs
(define (my-abs x)
  (cond
    ((= x 0) 0)
    ((> x 0) x)
    (else (- x))))

(define (my-abs2 x)
  (if (gr-or-equal x 0)
      x
      (- x)))

; x^n
(define (my-pow x n)
  (if (= n 0)
      1
      (* x (my-pow x (- n 1)))))

; count digits in a number
(define (count-digits n)
  (if (< n 10)
      1
      (+ 1 (count-digits (quotient n 10)))))


; sum-interval [a, b]
(define (sum-interval a b)
  (cond
    ((= a b) a)
    ((< a b) (+ a (sum-interval (+ a 1) b)))))

; my-odd? x
(define (my-odd? x)
  (= (remainder x 2) 1))

; my-even? x -> using my-odd
(define (my-even? x)
  (not (my-odd? x)))

; fast-pow
(define (fast-pow x n)
  (cond
    ((= n 0) 1)
    ((= n 2) (* x x))
    ((my-odd? n) (* x (fast-pow x (- n 1))))
    (else (fast-pow (fast-pow x (/ n 2)) 2))))

; factorial n
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

; fibonachi n
(define (fibonachi n)
  (define (fibonachi prev1 prev2 i n)
    (if (= i n)
        (+ prev1 prev2)
        (fibonachi prev2 (+ prev1 prev2) (+ i 1) n)))
  
  (cond
    ((= n 0) 0)
    ((and (> n 0) (< n 3)) 1)
    ((> n 0) (fibonachi 1 1 3 n))))



; reverse-digits x
(define (reverse-digits x)
  (define (reverse-digits x result)
    (if (= x 0)
        result
        (reverse-digits (quotient x 10) (+ (* result 10) (remainder x 10)))))
  
  (reverse-digits x 0))



; palindrome? x
(define (palindrome x)
  (= x (reverse-digits x)))