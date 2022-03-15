;; 1.34
(define (f g)
  (g 2))
;; calling (f f) will first call (f 2), but then this will expand on the next call to (2 2).
;; since 2 is not a procedure, and thus cannot be used as an operator, this is not a valid
;; scheme expression
