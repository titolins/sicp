;; 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))
