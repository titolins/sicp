;; 1.44
(define dx 2)

(define (average x y z)
  (/ (+ x y z) 3))

(define (smooth f)
  (lambda (x) (average (f (- x dx)) (f x) (f (+ x dx)))))
