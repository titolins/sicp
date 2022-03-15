;; 1.6
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))
;; since the new sqrt-iter doesn't use a special form, it will try to evaluate the alternative
;; expression even when it reaches the base case, which will cause it to get into a infinite loop
