;; 1.33
(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (if (filter a) (term a) null-value)))))
  (iter a null-value))

;; a
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

;; b
(define (filtered-product filter term a next b)
  (filtered-accumulate filter * 1 term a next b))

(define (relative-prime? x y)
  (= 1 (gcd x y)))

(define (product-rel-primes n)
  (define (filter i) (relative-prime? i n))
  (filtered-product rel-prime identity 1 inc (- n 1)))
