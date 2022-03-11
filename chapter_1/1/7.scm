;; 1.7
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

;; for small numbers, this tolarance is just too big. for big numbers, the precision of the machine
;; is not enough to represent the required guesses (at some point the algorithm enters an infinite loop
;; since the same number keeps being returned by improve due to the precision limit).
