(define x (list 1 2 3))
(define y (list 4 5 6))

;; 1.
(append x y)
;; (1 2 3 4 5 6)

;; 2.
(cons x y)
;; ((1 2 3) 4 5 6)

;; 3.
(list x y)
;; ((1 2 3) (4 5 6))
