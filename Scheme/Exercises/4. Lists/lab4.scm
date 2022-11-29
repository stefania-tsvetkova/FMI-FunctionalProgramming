(define (accumulate op nv a b term next)
  (if (> a b)
      nv
      (op (term a) (accumulate op nv (next a) b term next))))

;; count-divisors n - намира броя делители на числото n чрез accumulate
(define (count-divisors n)
  (define (1+ x) (+ 1 x))
  (define (divisor? x) (if (= (remainder n x) 0) 1 0))

  (if (< n 0)
      (+ 1 (count-divisors (- n)))
      (accumulate + 0 1 n divisor? 1+)))

;; задачи с наредени двойки
;; (x . y) - рационално число, х - числител, у - знаменател

(define (make-rational x y)
  (cons x y))

(define (nominator a)
  (car a))

(define (my-denominator a)
  (cdr a))

(define (sum-rationals a b)
  (/ (+ (* (nominator a) (my-denominator b)) (* (my-denominator a) (nominator b))) (* (my-denominator a) (my-denominator b))))

(define (substract-rationals a b)
  (/ (- (* (nominator a) (my-denominator b)) (* (my-denominator a) (nominator b))) (* (my-denominator a) (my-denominator b))))

(define (mult-rationals a b)
  (/ (* (nominator a) (nominator b)) (* (my-denominator a) (my-denominator b))))

(define (div-rationals a b)
  (mult-rationals a (cons (my-denominator b) (nominator b))))

(define (mult-rationals2 a b)
  (let (
        (nom (* (nominator a) (nominator b)))
        (denom(* (my-denominator a) (my-denominator b))))
    (make-rational nom denom)))

;; задачи със списъци
(define (sum-elements l)
  (if (null? l)
      0
      (+ 1 (sum-elements (cdr l)))))

(define (member? x l)
  (cond
    ((null? l) #f)
    ((= (car l) x) #t)
    (else (member? x (cdr l)))))


;; = - сравнява числа
;; eqv? - сравнява прости типове
;; equal? - сравнява списъци
;; eq? - сравнява по адреси, имплементацията е различна в зависимост от средата, не го използваме

;; my-lenght l - намира дължината на списъка
(define (my-lenght l)
  (if (null? l)
      0
      (+ 1 (my-lenght (cdr l)))))

;; n-th l n - намира n-тия елемент на l
(define (n-th l n)
  (cond
    ((or (< n 1) (null? l)) 'error)
    ((= n 1) (car l))
    (else (n-th (cdr l) (- n 1)))))

;; count-occurs x l - намира броя на срещанията на x в l
(define (count-occurs x l)
  (cond
    ((null? l) 0)
    ((eqv? x (car l)) (+ 1 (count-occurs x (cdr l))))
    (else (count-occurs x (cdr l)))))

;; my-append l1 l2 - залепя l1 и l2
(define (my-append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (my-append (cdr l1) l2))))

;; push-back x l - залепя x в края на l
(define (push-back x l)
  (my-append l (cons x '())))

;; take n l - връща първите n елемента на l
(define (take n l)
  (if (or (= n 0) (null? l))
      '()
      (cons (car l) (take (- n 1) (cdr l)))))

;; drop n l - подсписък без първите n елемента на l
(define (drop n l)
  (if (or (= n 0) (null? l))
      l
      (drop (- n 1) (cdr l))))

;; remove x l - маха първото срещане на x в l
(define (remove x l)
  (cond
    ((null? l) l)
    ((eqv? x (car l)) (cdr l))
    (else (cons (car l) (remove x (cdr l))))))

;; remove-all x l - маха всички срещания на x в l
(define (remove-all x l)
  (cond
    ((null? l) l)
    ((eqv? x (car l)) (remove-all x (cdr l)))
    (else (cons (car l) (remove-all x (cdr l))))))

;; my-reverse l - обръща реда на елементите на l
(define (my-reverse l)
  (if (null? l)
      l
      (append (reverse (cdr l)) (list (car l)))))

;; my-map f l - прилага функцията f върху елементите на l
(define (my-map f l)
  (if (null? l)
      l
      (cons (f (car l)) (my-map f (cdr l)))))

;; filter p l - връща само елементите от l, които отговарят на p
(define (filter p l)
  (cond
    ((null? l) l)
    ((p (car l)) (cons (car l) (filter p (cdr l))))
    (else (filter p (cdr l)))))