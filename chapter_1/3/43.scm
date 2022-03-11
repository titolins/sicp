;; 1.43
;;;(define (repeated f n)
;;;  (define (repeat n res)
;;;    (if (= n 0) res
;;;        (repeat (- n 1) (f res))))
;;;  (lambda (x) (repeat n x)))

(define (repeated f n)
  (if (= n 1) f
      (repeated (compose f f) (- n 1))))
