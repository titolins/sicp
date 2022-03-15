;; without map

(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* factor tree))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

;; scheme@(guile-user) [3]> (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
;; $11 = (10 (20 (30 40) 50) (60 70))

;; with map
(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))
