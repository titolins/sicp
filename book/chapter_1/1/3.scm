;; 1.3
(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (sum-of-2-larger a b c)
  (cond ((= (min a b c) a) (sum-of-squares b c))
        ((= (min a b c) b) (sum-of-squares a c))
        (else (sum-of-squares a b))))
