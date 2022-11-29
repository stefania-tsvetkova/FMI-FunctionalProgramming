; Problem 1
(define (n^q n q)
  (define (n^q n q result)
          (if (= q 0)
          result
          (n^q n (- q 1) (* result n))))

  (if (> q 0)
  (n^q n q 1)
  (/ 1 (n^q n (- q) 1)))
)

; Problem 2
(define (digits-count n)
  (define (digits-count n result)
    (if (< n 10)
        result
        (digits-count (quotient n 10) (+ result 1))))

  (if (< n 0)
      (digits-count (- n) 1)
      (digits-count n 1)))

; Problem 3
(define (sum-interval a b)
  (define (sum-interval a b sum)
    (if (> a b)
        sum
        (sum-interval (+ a 1) b (+ sum a))))

  (sum-interval a b 0))

; Problem 4
(define (reverse-number n)
  (define (reverse-number n result)
    (if (= n 0)
        result
        (reverse-number (quotient n 10) (+ (* result 10) (remainder n 10)))))

  (reverse-number n 0))

; Problem 5
(define (digits-sum n)
  (define (digits-sum n result)
          (if (= n 0)
              result
              (digits-sum (quotient n 10) (+ result (remainder n 10)))))

  (digits-sum n 0))

; Problem 6
(define (prime? n)
  (define (prime? n divisor)
    (cond
      ((= n 1) #f)
      ((= divisor n) #t)
      ((= (remainder n divisor) 0) #f)
      (else (prime? n (+ divisor 1)))))

  (prime? n 2))

; Problem 7
(define (increasing-digits? n)
  (define (increasing-digits? n last)
    (cond
      ((= n 0) #t)
      ((< last (remainder n 10)) #f)
      (else (increasing-digits? (quotient n 10) (remainder n 10)))))
  
  (increasing-digits? n 10))

; Problem 8
(define (ends-with? n k)
  (if (< k 10)
      (= (remainder n 10) k)
      (and (=(remainder n 10) (remainder k 10))
           (ends-with? (quotient n 10) (quotient k 10)))))

; Problem 9
(define (decimal-to-binary n)
  (define (decimal-to-binary n result starting-0s)
    (cond
      ((= n 0) (* (reverse-number result) (n^q 10 starting-0s)))
      ((and (= result 0) (= (remainder n 2) 0)) (decimal-to-binary (quotient n 2) result (+ starting-0s 1)))
      (else (decimal-to-binary (quotient n 2) (+ (* result 10) (remainder n 2)) starting-0s))))

  (decimal-to-binary n 0 0))

; Problem 10
(define (binary-to-decimal n)
  (define (binary-to-decimal n result multiplier)
    (if (< n 10)
        (+ result (* n multiplier))
        (binary-to-decimal
         (quotient n 10)
         (+ result (* (remainder n 10) multiplier))
         (* multiplier 2))))

  (binary-to-decimal n 0 1))

; is palindrome
(define (palindrome? n)
  (define (palindrome? front back)
    (cond
      ((< front back) #f)
      ((or (= front back) (= (quotient front 10) back)) #t)
      (else (palindrome? (quotient front 10) (+ (* back 10) (remainder front 10))))))

  (palindrome? n 0))