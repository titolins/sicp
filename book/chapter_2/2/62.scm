(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1)) (x2 (car set2)))
                (cond ((= x1 x2)
                       (cons x1
                             (union-set (cdr set1)
                                        (cdr set2))))
                      ((< x1 x2)
                       (cons x1
                             (union-set (cdr set1)
                                        set2)))
                      (else ;; (< x2 x1)
                       (cons x2
                             (union-set set1
                                        (cdr set2)))))))))

;; this is pretty much the same logic as the one used for the intersection solution from the book.
;; considering x1 and x2 the first elements of each set:
;; - if x1 = x2:
;;   - add only one of those and cdr both sets
;; - if x1 < x2:
;;   - add x1 and cdr set1
;; - if x2 < x1:
;;   - add x2 and cdr set2
;; The first cond will only check if either of the sets are empty and return the other one if this
;; is true. If both are empty, it will just fall into the first case which will return the necessary
;; '() for the list

;; without let for a more compact solution
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2)) (cons (car set1) (union-set (cdr set1) set2)))
        (else (cons (car set2) (union-set set1 (cdr set2))))))
