;; testing values
(define i1 (make-center-percent 5 1.0))
(define i2 (make-center-percent 10 .5))
(define i3 (make-center-percent 20 .5))

;; scheme@(guile-user) [22]> (percent (new-mul i1 i2))
;; $164 = 1.4999250037498266
;; scheme@(guile-user) [22]> (percent (new-mul i2 i3))
;; $166 = 0.9999750006249987
;; scheme@(guile-user) [22]> (percent (new-mul i1 i3))
;; $167 = 1.4999250037498266

;; for small tolerance (positive) intervals, the tolerance of their products can be approximated
;; by summing the tolerance of the factors
