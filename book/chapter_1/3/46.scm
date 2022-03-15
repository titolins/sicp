;; 1.46
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

