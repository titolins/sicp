; helpers
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc x) (+ x 1))

(define (dec x) (- x 1))
(define (identity x) x)

; 1.29
(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (yk k)
    (f (+ a (* k h))))
  (define (term k)
    (* (cond ((or (= k 0) (= k n)) 1)
             ((odd? k) 4)
             (else 2))
       (yk k)))
  (* (/ h 3.0)
     (sum term 0 inc n)))

; 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

; 1.31
; a
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (fact x)
  (product identity 1 inc x))

; b
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (fact x)
  (product-iter identity 1 inc x))

; 1.32
; a

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
         (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate-iter + 0 term a next b))

(define (integral-simpson f a b n)
  (define h (/ (- b a) n))
  (define (yk k)
    (f (+ a (* k h))))
  (define (term k)
    (* (cond ((or (= k 0) (= k n)) 1)
             ((odd? k) 4)
             (else 2))
       (yk k)))
  (* (/ h 3.0)
     (sum term 0 inc n)))

(define (product term a next b)
  (accumulate-iter * 1 term a next b))

(define (fact x)
  (product identity 1 inc x))

; 1.33
(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (if (filter a) (term a) null-value)))))
  (iter a null-value))

; a
(define (prime? x)
  (define (iter a)
    (cond ((= a x) #t)
          ((= 0 (remainder x a)) #f)
          (else (iter (inc a)))))
  (if (< x 2) #f (iter 2)))

(define (filtered-sum filter term a next b)
  (filtered-accumulate filter + 0 term a next b))

(define (sum-square-primes a b)
  (filtered-sum prime? square a inc b))

; b
(define (filtered-product filter term a next b)
  (filtered-accumulate filter * 1 term a next b))

(define (relative-prime? x y)
  (= 1 (gcd x y)))

(define (product-rel-primes n)
  (define (filter i) (relative-prime? i n))
  (filtered-product rel-prime identity 1 inc (- n 1)))

; 1.34
(define (f g)
  (g 2))
; calling (f f) will first call (f 2), but then this will expand on the next call to (2 2).
; since 2 is not a procedure, and thus cannot be used as an operator, this is not a valid
; scheme expression
