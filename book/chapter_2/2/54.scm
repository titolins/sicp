;; first iteration
(define (equal? a b)
  (cond ((and (null? a) (null? b)) #t)
        ((and (symbol? a) (symbol? b)) (eq? a b))
        ((and (number? a) (number? b)) (= a b))
        ((and (list? a) (list? b)) (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
        (else #f)))

;; we don't actually need to differentiate symbols, numbers, etc (can just use eq? for all not pairs)
;; also, using pair? instead of list? seems like the most scheme-like option
(define (equal? a b)
  (cond ((and (not (pair? a)) (not (pair? b))) (eq? a b))
        ((and (pair? a) (pair? b)) (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b))))
        (else #f)))
