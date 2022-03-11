;; we have to do:
;; lower-x - upper-y -> to get the smallest possible val
;; upper x - lower-y -> to get the largest possible val
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))
