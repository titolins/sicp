; 1.3
(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (sum-of-2-larger a b c)
  (cond ((= (min a b c) a) (sum-of-squares b c))
        ((= (min a b c) b) (sum-of-squares a c))
        (else (sum-of-squares a b))))

; 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; applicative order institutes that each expression in the combination shall be first
; evaluated. in that sense, the operator can also be a compound expression which will
; be evaluated until a primitive operator is obtained, which will then be used to evaluate
; the combination (after resolving the value of the operands if those are also compound).

; 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))
; p is an infinitely recursive function. in normal form, the alternative expression will
; never be evaluated. in applicative order, the compound procedure will try to be evaluated
; which will lead to the program hanging.

; 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))
; since the new sqrt-iter doesn't use a special form, it will try to evaluate the alternative
; expression even when it reaches the base case, which will cause it to get into a infinite loop

; 1.7
(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; for small numbers, this tolarance is just too big. for big numbers, the precision of the machine
; is not enough to represent the required guesses (at some point the algorithm enters an infinite loop
; since the same number keeps being returned by improve due to the precision limit).

; 1.8
(define (improve-cube guess x)
  (/ (+ (/ x (square guess)) (* 2 guess)) 3))

(define (good-enough? guess x) 
  (= (improve guess x) guess))

(define (cbrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (cbrt-iter (improve-cube guess x)
                 x)))

(define (cbrt x)
  (cbrt-iter 1.1 x))
