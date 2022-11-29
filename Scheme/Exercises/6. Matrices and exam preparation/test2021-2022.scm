; Problem1

(define (done? n)
  (define (dividers-sum n i)
    (cond
      ((= i n) 0)
      ((= (remainder n i) 0) (+ i (dividers-sum n (+ i 1))))
      (else (dividers-sum n (+ i 1)))))
  
  (= n (- (dividers-sum n 1) 2)))



(define (sum-almost-done a b)
  (define (closest-done i end n res)
    (cond
      ((> i end) res)
      ((and (done? i) (or (= res -1) (> (abs (- res n)) (abs (- i n))))) (closest-done (+ i 1) end n i))
      (else (closest-done (+ i 1) end n res))))
  
  (define (sum-almost-done a b i)
    (cond
      ((> i b) 0)
      ((done? i) (+ i (sum-almost-done a b (+ i 1))))
      ((> (min (- i a) (- b i)) (abs (- i (closest-done a b i -1)))) (+ i (sum-almost-done a b (+ i 1))))
      (else (sum-almost-done a b (+ i 1)))))

  (sum-almost-done a b a))

; Problem 2

(define (run-machine l)
  (define (pair-op f n s)
    (if (or (null? s) (= n 0))
        s
        (let (
              (first (car s))
              (second (cadr s))
              (tail (cddr s)))
          (if (and (number? first) (number? second))
              (pair-op f (- n 1) (cons (f first second) tail))
              s))))
  
  (define (run-machine l stack)
    (if (null? l)
        stack
        (let (
              (head (car l))
              (tail (cdr l)))
          (cond
            ((or (number? head) (symbol? head)) (run-machine tail (cons head stack)))
            ((procedure? head) (run-machine tail (map (lambda (x) (if (number? x) (head x) x)) stack)))
            ((and (pair? head) (procedure? (car head)) (number? (cdr head))) (run-machine tail (pair-op (car head) (cdr head) stack)))
            (else (run-machine tail stack))))))

  (run-machine l '()))

; Problem 3

(define (is-major? ll)
  (define (smaller? a b)
    (if (null? a)
        #t
        (and (<= (car a) (car b)) (smaller? (cdr a) (cdr b)))))

  (define (major? a b)
    (cond
      ((> (length a) (length b)) #f)
      ((smaller? a b) #t)
      (else (major? a (cdr b)))))

  (define (is-major? last-l ll)
    (if (null? ll)
        #t
        (let (
              (head (car ll))
              (tail (cdr ll)))
          (if (major? last-l head)
              (is-major? head tail)
              #f))))

  (is-major? (car ll) (cdr ll)))  