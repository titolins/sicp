;; 1.36
(define (no-avg-damp)
  (fixed-point (lambda (x) (/ (log 1000) (log x))) 2.0))

(define (with-avg-damp)
  (fixed-point (lambda (x) (/ (+ x (/ (log 1000) (log x))) 2.0)) 2.0))
