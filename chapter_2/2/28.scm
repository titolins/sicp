
(define (fringe x)
  (cond ((null? x) '())
        ((pair? (car x)) (append (fringe (car x)) (fringe (cdr x))))
        (else (append (list (car x)) (fringe (cdr x))))))

;; seemed a lot easier but it's just counter-intuitive that it can't be done with cons here..
;; or maybe it can? :D

(define x (list (list 1 2) (list 3 4)))

;; scheme@(guile-user) [16]> (fringe x)
;; $28 = (1 2 3 4)
;; scheme@(guile-user) [16]> (fringe (list x x))
;; $29 = (1 2 3 4 1 2 3 4)
