(define tolerance 0.00001)

(define (average x y)
  (/ (+ x y) 2))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

; 1.35
(define (golden-ratio)
  (fixed-point (lambda (y) (+ 1.0 (/ 1.0 y)))
               1.0))

; 1.36
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (xx)
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 1.2))

(define (xx-avg)
  (fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2)) 1.2))
