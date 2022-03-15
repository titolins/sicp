(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;; for
;; i1 = 1, 8
;; i2 = 2, 3

;scheme@(guile-user) [1]> (define i1 (make-interval 1 8))
;scheme@(guile-user) [1]> (width i1)
;$6 = 7/2
;scheme@(guile-user) [1]> (define i2 (make-interval 2 3))
;scheme@(guile-user) [1]> (width i2)
;$8 = 1/2
;scheme@(guile-user) [1]> (add-interval i1 i2)
;$10 = (3 . 11)
;scheme@(guile-user) [1]> (width (add-interval i1 i2))
;$9 = 4
;scheme@(guile-user) [1]> (sub-interval i1 i2)
;$11 = (-2 . 6)
;scheme@(guile-user) [1]> (width (sub-interval i1 i2))

;; ======================

;; for
;; i1 = -2.1, 7.9
;; i2 = 5.2, 7.8

;scheme@(guile-user) [2]> (define i1 (make-interval -2.1 7.9))
;scheme@(guile-user) [2]> (define i2 (make-interval 5.2 7.8))
;scheme@(guile-user) [2]> (width i1)
;$13 = 5.0
;scheme@(guile-user) [2]> (width i2)
;$14 = 1.2999999999999998
;scheme@(guile-user) [2]> (width (add-interval i1 i2))
;$15 = 6.3
;scheme@(guile-user) [2]> (width (sub-interval i1 i2))
;$16 = 6.300000000000001
;scheme@(guile-user) [2]> 12 = 4

;; ======================
;; for sum / sub, the width of the resulting interval will always
;; be the sum of the width of the argument intervals
;; ======================

;; for
;; i1 = 1, 8
;; i2 = 2, 3

;scheme@(guile-user) [3]> (mul-interval i1 i2)
;$21 = (2 . 24)
;scheme@(guile-user) [3]> (width (mul-interval i1 i2))
;$22 = 11
;scheme@(guile-user) [3]> (div-interval i1 i2)
;$23 = (0.3333333333333333 . 4.0)
;scheme@(guile-user) [3]> (width (div-interval i1 i2))
;$24 = 1.8333333333333333

;; ======================

;; for
;; i1 = -2.1, 7.9
;; i2 = 5.2, 7.8

;scheme@(guile-user) [3]> (mul-interval i1 i2)
;$17 = (-16.38 . 61.620000000000005)
;scheme@(guile-user) [3]> (width (mul-interval i1 i2))
;$18 = 39.0
;scheme@(guile-user) [3]> (div-interval i1 i2)
;$19 = (-0.40384615384615385 . 1.5192307692307692)
;scheme@(guile-user) [3]> (width (div-interval i1 i2))
;$20 = 0.9615384615384615

;; ======================
;; for multiplication / division, there's just no relation
;; if the width of the result was a function of the widths of the inputs,
;; then multiplying different intervals with the same widths should give the same answer:
;;scheme@(guile-user) [3]> (width (make-interval 0 10))
;;$25 = 5
;;scheme@(guile-user) [3]> (width (make-interval 0 2))
;;$26 = 1
;;scheme@(guile-user) [3]> (width (mul-interval (make-interval 0 10) (make-interval 0 2)))
;;$27 = 10
;;scheme@(guile-user) [3]> (width (make-interval -5 5))
;;$28 = 5
;;scheme@(guile-user) [3]> (width (make-interval -1 1))
;;$29 = 1
;;scheme@(guile-user) [3]> (width (mul-interval (make-interval -5 5) (make-interval -1 1)))
;;$30 = 5
;; ======================
