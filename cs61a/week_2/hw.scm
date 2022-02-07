;; book exercises are in the ex.scm file
(load "berkeley.scm")

; 2
(define (every f ps)
  (define (iter l res)
    (if (null? l)
      res
      (iter (bl l) (cons (f (last l)) res))))
  (iter ps '()))
