; Problem 1

(define (simple? n)
  (define (simple? i)
    (cond
      ((= i n) #t)
      ((= (remainder n i) 0) #f)
      (else (simple? (+ i 1)))))

  (simple? 2))

(define (trim n)
  (define (helper i res)
    (cond
      ((> i n) res)
      ((and (= (remainder n i) 0) (simple? i)) (helper (+ i 1) (quotient res i)))
      (else (helper (+ i 1) res))))

  (helper 2 n))
        
; Problem 2

(define (commonUnitary n1 n2)
  (define (unitary? n k)
    (if (> (remainder n k) 0)
        #f
        (= (gcd (quotient n k) k) 1)))

  (define (commonUnitary i)
    (if (> i (min n1 n2))
        0
        (+
         (if (and (unitary? n1 i) (unitary? n2 i)) 1 0)
         (commonUnitary (+ i 1)))))

  (commonUnitary 1))
      
; Problem 3

(define (selectiveMerge f a b)
  (define (helper a b last-rule)
    (if (null? a)
        a
        (let* (
               (ai (car a))
               (bi (car b))
               (f-val (f ai bi))
               (min-val (min ai bi)))
          (cond
            ((< f-val min-val) (cons ai (helper (cdr a) (cdr b) 1)))
            ((> f-val min-val) (cons f-val (helper (cdr a) (cdr b) 2)))
            ((= last-rule 1) (cons ai (helper (cdr a) (cdr b) 1)))
            (else (cons f-val (helper (cdr a) (cdr b) 2)))))))

  (cons (car a) (helper (cdr a) (cdr b) 1)))

; Problem 4

(define (contains? l x)
  (cond
    ((null? l) #f)
    ((= (car l) x) #t)
    (else (contains? (cdr l) x))))

(define (remove-first l x)
  (cond
    ((null? l) l)
    ((= (car l) x) (cdr l))
    (else (cons (car l) (remove-first (cdr l) x)))))

(define (common device network)
  (cond
    ((or (null? device) (null? network)) '())
    ((contains? network (car device)) (cons (car device) (common (cdr device) (remove-first network (car device)))))
    (else (common (cdr device) network))))

(define (common-count device network)
  (length (common device network)))

(define (compatible? device network)
  (>= (common-count device network) 2))

(define (coverage device network)
  (if (null? network)
      0
      (/ (common-count device network) (length network))))

(define (prefferedNetwork device networks)
  (define (prefferedNetwork device networks result)
    (if (null? networks)
        result
        (let ((network (car networks)))
          (prefferedNetwork device
                            (cdr networks)
                            (if (and (compatible? device network) (> (coverage device network) (coverage device result)))
                                network
                                result)))))

  (common device (prefferedNetwork device networks '())))

; Bonus

(define (coverage-sum devices network)
  (if (null? devices)
      0
      (+ (coverage (car devices) network) (coverage-sum (cdr devices) network))))

(define (compatible-devices-count devices network)
  (if (null? devices)
      0
      (+
       (if (compatible? (car devices) network) 1 0)
       (compatible-devices-count (cdr devices) network))))

(define (all-lists-contain? ll x)
  (cond
    ((null? ll) #t)
    ((not (contains? (car ll) x)) #f)
    (else (all-lists-contain? (cdr ll) x))))

(define (compatible-devices devices network)
  (cond
    ((null? devices) '())
    ((compatible? (car devices) network) (cons (car devices) (compatible-devices (cdr devices) network)))
    (else (compatible-devices (cdr devices) network))))

(define (compatible-subnetwork devices network)
  (cond
    ((null? network) '())
    ((all-lists-contain? devices (car network)) (cons (car network) (compatible-subnetwork devices (cdr network))))
    (else (compatible-subnetwork devices (cdr network)))))
      

(define (prefferedNetworkForAll devices networks)
  (define (prefferedNetworkForAll devices networks best-network best-devices-count best-coverage-sum)
    (if (null? networks)
        best-network
        (let* (
               (network (car networks))
               (devices-count (compatible-devices-count devices network))
               (coverage-sum (coverage-sum devices network)))
          (if (or (> devices-count best-devices-count) (and (= devices-count best-devices-count) (> coverage-sum best-coverage-sum)))
              (prefferedNetworkForAll devices (cdr networks) network devices-count coverage-sum)
              (prefferedNetworkForAll devices (cdr networks) best-network best-devices-count best-coverage-sum)))))

  (let* (
         (network (prefferedNetworkForAll devices networks '() 0 0))
         (compatible-devices (compatible-devices devices network)))
    (compatible-subnetwork compatible-devices network)))
          