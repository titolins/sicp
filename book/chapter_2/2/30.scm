;; without map

(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

;; scheme@(guile-user) [1]> (square-tree (list 1 (list 2 (list 3 4 (list 5)) 6)))
;; $3 = (1 (4 (9 16 (25)) 36))


;; using map

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (* sub-tree sub-tree)))
       tree))

;; scheme@(guile-user) [4]> (square-tree (list 1 (list 2 (list 3 4 (list 5)) 6)))
;; $4 = (1 (4 (9 16 (25)) 36))
