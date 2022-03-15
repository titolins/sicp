;; 1.4
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
;; applicative order institutes that each expression in the combination shall be first
;; evaluated. in that sense, the operator can also be a compound expression which will
;; be evaluated until a primitive operator is obtained, which will then be used to evaluate
;; the combination (after resolving the value of the operands if those are also compound).
