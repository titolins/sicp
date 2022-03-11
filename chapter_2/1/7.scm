(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

;; or, if we'd like to be safe in terms of the order
;; of the bounds passed to the constructor
(define (lower-bound i) (min (car i) (cdr i)))
(define (upper-bound i) (max (car i) (cdr i)))

;; this could also be done inside the constructor
(define (make-interval a b) (cons (min a b) (max a b)))
