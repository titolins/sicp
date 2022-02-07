; helpers
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc x) (+ x 1))

(define (dec x) (- x 1))
(define (identity x) x)

(define (average x y)
  (/ (+ x y) 2))

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

; 1.35
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    ;(display guess)
    ;(newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define (golden-ratio)
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

; 1.36
(define (no-avg-damp)
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0))

(define (with-avg-damp)
  (fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2.0)) 2.0))

; 1.37
;; recursive
(define (cont-frac-rec n d k)
  (define (rec i)
    (if (= i k)
      0
      (/ (n i) (+ (d i) (rec (+ i 1))))))
  (rec 1))

;; iterative
(define (cont-frac-iter n d k)
  (define (iter i res)
  (if (= i 1)
    res
    (iter (- i 1) (/ (n i) (+ (d i) res)))))
  (iter k 0))

; (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)
;Value: .6179775280898876
;(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 12)
;Value: .6180555555555556 <-- .6180 ; k = 12

; 1.38
; taken from http://community.schemewiki.org/?sicp-ex-1.38
; why +2?
(define (e-euler k) 
  (+ 2.0 (cont-frac (lambda (i) 1) 
    (lambda (i) 
      (if (= (remainder i 3) 2)
        (/ (+ i 1) 1.5) 
        1)) 
    k)))

;; 1.3.4
(define dx 0.00001)

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
;(define (newtons-method g guess)
;  (fixed-point (newton-transform g) guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))
(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform
                            1.0))

(define (newtons-method g guess)
  (fixed-point-of-transform g newton-transform guess))

; 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

; 1.41
(define (double f)
  (lambda (x)
    (f (f x))))

(define (inc x)
  (+ x 1))

; 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))

; 1.43
;;(define (repeated f n)
;;  (define (repeat n res)
;;    (if (= n 0) res
;;        (repeat (- n 1) (f res))))
;;  (lambda (x) (repeat n x)))

(define (repeated f n)
  (if (= n 1) f
    (repeated (compose f f) (- n 1))))

; 1.44
(define dx 2)

(define (average x y z)
  (/ (+ x y z) 3))

(define (smooth f)
  (lambda (x) (average (f (- x dx)) (f x) (f (+ x dx)))))

; 1.45
; @todo

; 1.46
(define (iterative-improve good-enough? improve)
  (define (iter guess x)
    (if (good-enough? guess)
      guess
      (iter (improve guess) x)))
  (lambda (x) (iter 1.0 x)))

;; sqrt
(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve-guess guess)
    (define (average x y)
        (/ (+ x y) 2))
    (average guess (/ x guess)))
  ((iterative-improve good-enough? improve-guess) x))

;; fixed-point
(define tolerance 0.00001)
(define (fixed-point f guess)
  (define (close-enough? guess)
    (< (abs (- guess (f guess))) tolerance))
  ((iterative-improve close-enough? f) guess))

