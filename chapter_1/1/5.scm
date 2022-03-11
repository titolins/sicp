;; 1.5
(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))
;; p is an infinitely recursive function. in normal form, the alternative expression will
;; never be evaluated. in applicative order, the compound procedure will try to be evaluated
;; which will lead to the program hanging.
